# nim notes

## export a proc from a module

Add a `*` to the end of the proc name:
```nim
proc myProc*() =
    echo "this function is exported"
```

## var vs const vs let

## regex

```nim
import re
```

A Regex cannot be a const
