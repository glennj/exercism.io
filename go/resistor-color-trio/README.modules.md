re-using my earlier solution:

```sh
mkdir -p lib/resistor-color{,-duo}
cp ../resistor-color/{go.mod,resistor-color.go} lib/resistor-color
cp ../resistor-color-duo/{go.mod,resistor-color-duo.go} lib/resistor-color-duo
go mod edit -replace resistorcolor=./lib/resistor-color 
go mod edit -replace resistorcolorduo=./lib/resistor-color-duo
go get resistorcolor
go get resistorcolorduo
```
