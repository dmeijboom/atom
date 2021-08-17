# Benchmarks

Benchmarks based on the [Wren Benchmarks](https://github.com/wren-lang/wren/tree/main/test/benchmark).
Only for internal use, just to have some sort of baseline.

Note that this is a completely unfair benchmark as atom was tested on my Apple M1 and all the other programs are tested using an MacBook Pro 2.3 GHz Intel Core i7 with 16 GB of 1,600 MHz DDR3 RAM (see: https://wren.io/performance.html).
It's very likely that atom is an order of magnitude slower than presented here.

## Times

### Recursive Fibonacci

| Name | Time |
|------|------|
| luajit (-joff) | 0.10s |
| wren | 0.20s |
| ruby | 0.22s |
| lua | 0.28s |
| python | 0.51s |
| python3 | 0.57s |
| **atom** | 0.97s |


### Binary Trees


| Name | Time |
|------|------|
| luajit (-joff) | 0.11s |
| wren | 0.22s |
| ruby | 0.24s |
| python | 0.37s |
| python3 | 0.38s |
| lua | 0.52s |
| **atom** | 2.56s |
