# webjdb-haskell

The Haskell version of 
https://github.com/jameslawson/webjdb    
Lot's of cleaning up of code happening.

### Setup

**JDI**  
Install the JDI package to your local machine.     
`cd jdi`     
`runhaskell Setup.hs configure --user`    
`runhaskell Setup.hs build`     
`runhaskell Setup.hs install`    
Check JDI was installed by running the test: `sh runTest.sh`

**Websockets**  
Install the websockets package to your local machine:    
`cabal install websockets`

**Running** 

1. Run the target: `java -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5050`
2. Run the server: `runhaskell server.hs`
3. Open the client: `open client.html`
