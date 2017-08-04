<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{dbscan1d benchmark}
-->

dbscan1d benchmark
========================================================

This vignette benchmarks package functions.

Let's load the packages


```r
library(dbscan1d) #our library
library(microbenchmark) #benchmarking
```


Exercise 06.01 (mode)
-------------------------------------------------------

Let's prepare some data:



Now, let's benchmark it against our solution:


```r
#x=sample(100, replace=TRUE, 100)
#microbenchmark (mode(x), Rmode(x))

#x=sample(100, replace=TRUE, 1000)
#microbenchmark (mode(x), Rmode(x))

#x=sample(100, replace=TRUE, 10000)
#microbenchmark (mode(x), Rmode(x))
```

