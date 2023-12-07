# VIM notes

[Learn Vimscript the Hard Way](http://learnvimscriptthehardway.stevelosh.com/)

## Comparisons

`:help expression-syntax`

### String comparison

**MUST** account for user settings

```vimscript
setlocal ignorecase
if "foo" == "FOO"
    echo "one"
else
    echo "two"
endif
"" prints "one"

setlocal noignorecase
"" case INSENSITIVE no matter what
if "foo" ==? "FOO"
    echo "one"
else
    echo "two"
endif
"" prints "one"

setlocal ignorecase
"" case SENSITIVE no matter what
if "foo" ==# "FOO"
    echo "one"
else
    echo "two"
endif
"" prints "two"
```

## Variables

`:he variable-scope`

## Functions

Convention dictates it starts with UpperCase
