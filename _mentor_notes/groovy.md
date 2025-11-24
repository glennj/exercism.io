Groovy seems to want java v17:

```
ll $HOMEBREW_PREFIX/bin/java*
brew install openjdk@17
brew unlink openjdk # or specific version
brew link openjdk@17
```
