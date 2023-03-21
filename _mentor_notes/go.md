# Student notes on Go

## Mentored solutions

- https://exercism.org/tracks/go/exercises/census/mentor_discussions/29eec782be794170bd1b0aa11de1f530
- https://exercism.org/tracks/go/exercises/tournament/mentor_discussions/6b92568903fc43288289a728ac7d6e53
- https://exercism.org/tracks/go/exercises/logs-logs-logs/mentor_discussions/36e254919d1b4819acd2e431acd1c072
- https://exercism.org/tracks/go/exercises/hamming/mentor_discussions/71eddfe93b9f42b3b1d67fe1b9b74c3c

## Strings and Runes

```go
var s string
var rs []rune

s = "foobar"
rs = []rune(s)
s == string(rs)
```

## Struct methods

- should operator on a pointer
  ```go
  type Foo struct {...}
  func (f *Foo) methodName...
  ```

## Naked returns

- "Naked return statements should be used only in short functions, as with the example shown here. They can harm readability in longer functions."
  - https://go.dev/tour/basics/7

## Errors

- Wrap errors with `fmt.Errorf()` and the `%w` verb to maintains the original error's type.
  - This also allows us to add additional information; very helpful for tracing the error's source and progression.

- when the error string needs formatting, use `fmt.Errorf(fmt, args...)`
  not `error.New(fmt.Sprintf(...))`
  - https://staticcheck.io/docs/checks#S1028
  - https://pkg.go.dev/fmt@go1.20.2#Errorf

re: tournament review

>> in the case of a custom struct like I'm using, what is the zero value? Is it Teams{}? Is it nil?
>
> The zero value for structs is always MyStruct{} and not nil.
> It would only be nil if instead of an instance to a struct you were returning a pointer to the struct instance (\*MyStruct instead of MyStruct), in which case nil would be the zero value for the pointer.
>
> However, in your solution, actually returning nil would also work.
> The reason for this is because Teams is not a struct, but a type definition.
> Type definitions have an underlying type, which in this case is a slice of pointers to team ([]\*Team) and since the zero value for slices is nil, the zero value for Teams will actually be nil, so feel free to return nil.
> You can also return Teams{} in this case.
> The difference to nil is that Teams{} will create a non-nil slice that's empty.
> There's little difference between nil slices and non-nil empty slices - both are iterable, both can be appended to, and you can check the length of both with len, so I'd say you can return one or the other interchangeably.
> I have a slight preference of returning nil in this case, just because that way you are not actually creating a slice.
> Since this value will be returned in a case of error, the caller is expected to ignore it either way, so this decision matters even less.


## Conditional and variable scope

```go
x := somefunc()
if x { ...
```
versus
```go
if x := somefunc(); x { ...
```
which localizes `x` to the if statement.


## Using external packages

The Go test runner allows only these non-standard packages

- https://pkg.go.dev/golang.org/x/exp

```sh
$ go get golang.org/x/exp@v0.0.0-20221006183845-316c7553db56
go: downloading golang.org/x/exp v0.0.0-20221006183845-316c7553db56
go: downgraded golang.org/x/exp v0.0.0-20230315142452-642cacee5cc0 => v0.0.0-20221006183845-316c7553db56

$ go mod tidy

# ... edit/test ...

$ exercism submit nth-prime.go go.mod go.sum
```

**Only that specific package version is allowed.**

The `go.mod` and `go.sum` files must be submitted.

See

- https://github.com/exercism/go-test-runner/#external-go-packages
- https://github.com/exercism/go-test-runner/blob/main/external-packages/go.mod
