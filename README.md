# Harmonium
The idea is to have a tool to help with composing music, generate melodies, generate chord progressions, etc. 

The scope is not well defined yet; in early development.
## Usage

### Development

```
cabal install --only-dependencies
cabal repl
```

### Generating scales & modes

Iniside ghci
```
exportModeToFile Aeolian E "e_minor.mid"
```
