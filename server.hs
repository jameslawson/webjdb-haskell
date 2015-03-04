{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Language.Java.Jdi
import qualified Language.Java.Jdi.VirtualMachine as VM
import qualified Language.Java.Jdi.Event as E
import qualified Language.Java.Jdi.EventSet as ES
import qualified Language.Java.Jdi.EventRequest as ER
import qualified Language.Java.Jdi.ReferenceType as RT
import qualified Language.Java.Jdi.ArrayReference as AR
import qualified Language.Java.Jdi.StringReference as SR
import qualified Language.Java.Jdi.Value as V
import qualified Language.Java.Jdi.StackFrame as SF
import qualified Language.Java.Jdi.ThreadReference as TR
import qualified Language.Java.Jdi.ThreadGroupReference as TG
import qualified Language.Java.Jdi.ObjectReference as OR
import qualified Language.Java.Jdi.Method as M
import qualified Language.Java.Jdi.Field as F
import qualified Language.Java.Jdi.Location as L

import           Data.List            (intercalate)
import           Data.Text            hiding (map, head, filter, intercalate, last)
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.=))
import           Network.WebSockets   (Message (DataMessage), DataMessage(Text))
import           Network.Socket.Internal (PortNumber(..))
import           Network.Socket       (withSocketsDo)
import           Network
import qualified Network.WebSockets   as WS
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

import Control.Concurrent  (forkIO, MVar, takeMVar, putMVar, newEmptyMVar)
import Control.Applicative ((<$>))
import Control.Monad (forM_, filterM, void, liftM, when, forever, unless)
import Control.Monad.Error (MonadError(..), runErrorT, ErrorT, Error(..))
import Control.Monad.Trans (liftIO, lift)
import System.Exit


data StackFrame = StackFrame
  { threadName  :: String
  , depthNumber :: Int
  } deriving (Show)

instance ToJSON StackFrame where
  toJSON sf = A.object
    [ "threadName"  .= threadName sf
    , "depthNumber" .= depthNumber sf
    ]

main :: IO ()
main = WS.runServer "127.0.0.1" 3080 serve

serve :: WS.PendingConnection -> IO ()
serve pendconn = do
  done <- newEmptyMVar
  m    <- newEmptyMVar
  n    <- newEmptyMVar
  conn <- WS.acceptRequest pendconn
  _ <- forkIO $ forever $ do
      msg <- WS.receiveDataMessage conn
      processMsg msg conn m done
  _ <- forkIO $ do
      VM.runVirtualMachine "localhost" (PortNumber 5050) (debugger m n)
  _ <- forkIO $ do
      sendToClient conn n

  takeMVar done
  WS.sendClose conn ("Bye!" :: Text)


debugger :: MVar String -> MVar Text -> VM.VirtualMachine IO ()
debugger m n = do
    -- setup
    es <- ES.removeEvent

    -- prepare main class
    prepReq <- ER.enable ER.createClassPrepareRequest
    pollEvents $ \e -> case E.eventKind e of
       E.ClassPrepare -> isMainClass $ E.referenceType e
       _ -> False

    -- get locations
    classes <- VM.allClasses
    let mainClass = head $ filter isMainClass classes
    classLineLocations <- RT.allLineLocations mainClass
    let isMainMethod = ("main" ==) . M.name
    methods <- RT.methods mainClass
    let methodMain = head $ filter isMainMethod methods
    mainLocation <- M.location methodMain

    -- store main thread
    threads <- VM.allThreads
    let isMainThread = ("main" ==) . TR.name
    -- liftIO . putStrLn $ intercalate "\n" (map (show . TR.name) threads)
    let mainThread = head $ filter isMainThread threads
    liftIO . putStrLn $ (TR.name mainThread)


    -- setup breakpoint 
    bpReq <- ER.enable $ ER.createBreakpointRequest mainLocation
    ev <- pollEvents $ \e -> case E.eventKind e of
        E.Breakpoint -> True
        _ -> False

    -- print stack frame
    VM.suspend

    -- listen for commands
    let loop = do
        r <- liftIO $ takeMVar m 
        liftIO (putStrLn r)
        case r of 
          "step" -> do
              -- SuspendAll policy is being used
              evThread <- E.thread ev
              stepReq <- ER.enable $ (ER.createStepRequest evThread StepLine StepOver)
              VM.resume

              stepEvt <- pollEvents $ \e -> case E.eventKind e of
                E.SingleStep -> True
                _ -> False
              VM.suspend

              currentThread <- E.thread stepEvt
              allFrames <- TR.allFrames currentThread
              let frame = head allFrames 
              loc <- SF.location frame
              let method = L.method loc
              let lineNum = L.lineNumber loc
              sourceName <- L.sourceName loc
              let lineKey = sourceName ++ ":" ++ (show . M.name $ method) ++ ":" ++ show lineNum

              vars <- M.variables (L.method loc)
              inScope <- mapM (\x -> (Just `liftM` (SF.getValue frame x)) `catchError` (\_ -> return Nothing)) vars
              liftIO . (putMVar n) . pack  $ lineKey ++ "\n" ++ (show inScope)

          "printframe" -> do
              frame <- head <$> TR.allFrames mainThread
              loc <- SF.location frame
              let method = L.method loc
              let lineNum = L.lineNumber loc
              sourceName <- L.sourceName loc
              let lineKey = sourceName ++ ":" ++ (show . M.name $ method) ++ ":" ++ show lineNum

              vars <- M.variables (L.method loc)
              inScope <- mapM (\x -> (Just `liftM` (SF.getValue frame x)) `catchError` (\_ -> return Nothing)) vars
              liftIO . (putMVar n) . pack  $ lineKey ++ "\n" ++ (show inScope)
          _ -> do
              liftIO . putStrLn $ "unknown operation"
        loop
    loop



processMsg :: DataMessage -> WS.Connection -> MVar String -> MVar String -> IO ()
processMsg msg conn m done = do
    case msg of
      Text (parseWant -> Just cmd) -> do
        -- putMVar done "hello"
        putMVar m "hello"
        WS.sendTextData conn ("Cool" :: Text)
        WS.sendTextData conn $ A.encode $ StackFrame 
          { threadName = "James"
          , depthNumber = 2
          }
      Text "step" -> do
        putMVar m "step"
        WS.sendTextData conn ("performing step..." :: Text)
      Text "printframe" -> do
        putMVar m "printframe"
        WS.sendTextData conn ("printing frame..." :: Text)
      _ -> do
        WS.sendTextData conn ("Sorry, I don't understand" :: Text)


sendToClient :: WS.Connection -> MVar Text -> IO ()
sendToClient conn n = do
  let loop = do
      txt <- takeMVar n
      WS.sendTextData conn txt
      loop
  loop


parseWant :: LBS.ByteString -> Maybe Text
parseWant = stripPrefix "I want " . decodeUtf8 . LBS.toStrict


isMainClass ref = 
  RT.signature ref == "LTarget;"

pollEvents stopFunction = do
    VM.resume
    es <- ES.removeEvent
    let e = head $ ES.events es
    if stopFunction e
        then return e
        else pollEvents stopFunction
