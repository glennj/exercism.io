Here are 3 different implementations:

1. all the values for each square are simply hardcoded
2. calling out to `bc` to perform the calculcations
    - optimized to minimize the number of `bc` invocations
3. performing the addition in bash, using string manipulation

#### hardcoded values
```bash
ln -f grains_precalculated.sh grains.sh
time for i in {1..10}; do 
    BATS_RUN_SKIPPED=true command bats grains_test.sh
done >/dev/null
```
```none
real	0m8.260s
user	0m3.437s
sys	0m3.671s
```
#### calling `bc`
```bash
ln -f grains_bc.sh grains.sh
time for i in {1..10}; do
    BATS_RUN_SKIPPED=true command bats grains_test.sh
done >/dev/null
```
```none
real	0m8.977s
user	0m3.613s
sys	0m4.079s
```
_Almost_ as fast as the hardcoded version, pretty good.
#### bash only
```bash
ln -f grains_bash_only.sh grains.sh
time for i in {1..10}; do
    BATS_RUN_SKIPPED=true command bats grains_test.sh
done >/dev/null
```
```none
real	0m8.977s
user	0m3.613s
sys	0m4.079s
```
Not surprising this one is very slow.
