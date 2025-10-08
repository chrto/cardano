# OnChain Validators
## Build
### Requirmets
Make sure You are using correct haskell and cabal version.

```
ζ cabal --version
cabal-install version 3.6.2.0
compiled using version 3.6.2.0 of the Cabal library
ζ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
```

If not, install them

```
ζ ghcup install ghc 8.10.7
ζ ghcup set ghc 8.10.7
ζ ghcup install cabal 3.6.2.0
ζ ghcup install hls 1.7.0.0
```

## Build project
Build project from onchain root directory

```
ζ cd ./onchain
ζ cabal update
ζ cabal build all
```

## Use cabal repl
Open specific project in cabal

```
ζ cd ./Vesting
ζ cabal repl
```