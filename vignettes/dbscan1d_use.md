<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{dbscan1d - how to use}
-->

dbscan1d - how to use
========================================================

This vignette shows how to use the package.

First load the package


```r
library(dbscan1d)
```


Get some data and cluster it

dbscan1d simple example
-------------------------------------------------------

dbscan1d - call it with numeric vector, eps and minPoints



```r
dbscan1d(c(5,6,7,1,2,3,15,16,17), eps=1, minPoints=2)
```

```
## [1] 2 2 2 1 1 1 3 3 3
```


dbscan1d for sorted data
-------------------------------------------------------

If your data is already sorted you can get an edge in performance. The complexity of the alghorithm is linear in that case.

dbscan1d - call it with numeric vector, eps, minPoints and sort=FALSE 


```r
dbscan1d(c(1,2,3,5,6,7,15,16,17), eps=1, minPoints=2, sort=FALSE)
```

```
## [1] 1 1 1 2 2 2 3 3 3
```

