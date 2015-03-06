# webjdb-haskell

The Haskell version of 
https://github.com/jameslawson/webjdb    
Lot's of cleaning up of code happening.

### Dependencies

- GHCI 
- JDK 1.8 (with tools.jar in your classpath)
- cabal >= 1.20.0.0 (depends on the `--allow-new` option)
- JDI v0.0.4 cabal package
- websockets cabal package

### Build

Clone the repository:
`git clone https://github.com/jameslawson/webjdb-haskell.git`

**JDI**  
The version of JDI on Hoogle is old (v0.0.3) - there is a newer version on GitHub (v0.0.4) - we will use that version. You will need to copy [JDI v0.0.4](https://github.com/VictorDenisov/jdi/tree/95a150f888ee8bf13c3e7d04aaa5d53df2de258f) to your local machine. 
At the project root:     
```
git clone https://github.com/VictorDenisov/jdi.git    
cd jdi   
cabal install --allow-new=base
cd ..
```

**Cabal Sandbox**  

Then create a cabal sandbox and add the github repo as a source. At the project root:     
```
cabal sandbox init
cabal sandbox add-source jdi/
cabal install --allow-new=base
cabal build
```
### Running
1. Run the target: `java -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5050`
2. Run the server: `cabal run server`
3. Open the client: `open client.html`
