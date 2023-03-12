re-using my earlier solution:

```sh
mkdir -p lib/resistor-color
cp ../resistor-color/{go.mod,resistor-color.go} lib/resistor-color
go mod edit -replace resistorcolor=./lib/resistor-color 
go get resistorcolor
```
