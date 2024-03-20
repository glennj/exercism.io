Groovy seems to want java v11:

```
ll $HOMEBREW_PREFIX/bin/java*
brew install openjdk@11
brew unlink openjdk # or specific version
brew link openjdk@11
```
