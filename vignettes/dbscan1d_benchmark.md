<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{dbscan1d benchmark}
-->

dbscan1d benchmark
========================================================

This vignette benchmarks dbscan1d vs fpc::dbscan and dbscan::dbscan packages

Let's load the packages


```r
library(dbscan1d) #our library
library(microbenchmark) #benchmarking
```

Let's load alternative dbscan implementations


```r
library(fpc) # fpc::dbscan, no K-D trees O(n^2)
library(dbscan) # dbscan::dbscan, fast K-D trees backed with C++ implementation
```

```
## 
## Attaching package: 'dbscan'
```

```
## The following object is masked from 'package:fpc':
## 
##     dbscan
```
Let's make a data preparation function.

Parameters:
- number of clusters
- average point distance in cluster
- number of points in cluster
- cluster separation ratio to cluster size


```r
test.data=function(clusters, points.distance, points, separation.ratio=1)
{
  cluster.radius=points.distance*points/2
  cluster.separation=cluster.radius*2*separation.ratio

  centers=seq(0, by=cluster.separation+2*cluster.radius, length.out=clusters)
  data=numeric()
  
  for(center in centers)
  {
    data=c(data, runif(points, center-cluster.radius, center+cluster.radius))
  }
  data=data[sample(length(data))]

  data
}    

data=test.data(4, 0.01, 100, 1)
head(data)
```

```
## [1] 5.998087 2.258712 5.660308 5.587705 2.405784 1.946936
```

And 1D plot function with jitter as y:

```r
plot1d=function(data, ...)
{
  x=data
  y=jitter(rep(1, length(data)), amount=1)
  plot(x, y, type="p", axes=FALSE, ylab="",xlab="", ...)
  axis(side=1, at=c(min(x), max(x)), labels=c('',''),lwd.ticks = c(0, 0), pos=0, line=1)
}    

plot1d(data)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Let's test the clustering methods.


fpc::dbscan
dbscan::dbscan
dbscan1d


```r
eps=0.1
minPoints=4

data=test.data(clusters = 4, points.distance = 0.01, points = 100, separation.ratio = 1)

res.dbscan1d=dbscan1d(data, eps=eps, minPoints = minPoints)
res.dbscan=dbscan::dbscan(as.matrix(data), eps=eps, minPts=minPoints)
res.fpc=fpc::dbscan(data, eps=eps, MinPts=minPoints)

par(mfrow=c(3,1))

plot1d(data, col=res.dbscan1d, main="dbscan1d")
plot1d(data, col=res.dbscan$cluster, main="dbscan")
plot1d(data, col=res.fpc$cluster, main="fpc")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Let's make simple benchmark


```r
data=test.data(clusters = 4, points.distance = 0.01, points = 100, separation.ratio = 1)
data.matrix=as.matrix(data) #for dbscan::dbscan

eps=0.1
minPoints=4

bench=microbenchmark(dbscan1d(data, eps, minPoints), dbscan::dbscan(data.matrix, eps, minPoints), fpc::dbscan(data, eps, minPoints), times=100)

print(bench)
```

```
## Unit: microseconds
##                                         expr       min        lq
##               dbscan1d(data, eps, minPoints)    38.761    43.265
##  dbscan::dbscan(data.matrix, eps, minPoints)   591.044   611.262
##            fpc::dbscan(data, eps, minPoints) 39290.634 39957.277
##         mean    median         uq       max neval
##     54.42256    52.065    68.6515     78.01   100
##   1291.21500   758.376   815.9580  57203.22   100
##  44383.28062 42645.675 43622.3655 178006.04   100
```

```r
plot(bench)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Cleary fpc::dbscan R straightforward O(n^2) implementation is out of competition.

Let's focus on dbscan::dbscan vs dbscan1d


```r
data=test.data(clusters = 4, points.distance = 0.01, points = 1000, separation.ratio = 1)
data.matrix=as.matrix(data) #for dbscan::dbscan

eps=0.1
minPoints=4

bench=microbenchmark(dbscan1d(data, eps, minPoints),dbscan::dbscan(data.matrix, eps, minPoints), times=100)

print(bench)
```

```
## Unit: microseconds
##                                         expr      min       lq      mean
##               dbscan1d(data, eps, minPoints)  432.231  445.674  493.7029
##  dbscan::dbscan(data.matrix, eps, minPoints) 4938.907 5083.438 5280.1166
##    median        uq      max neval
##   468.267  480.3485 3264.320   100
##  5367.575 5400.2600 5962.809   100
```

```r
plot(bench)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

1. Mean/median are representative of the running time
2. dbscan1d is clearly a winner

Let's investigate dependence on input size:


```r
#benchmark parameters
N=3
inputs=1000*2^(0:4)
times.dbscan1d=numeric(N)
times.dbscan=numeric(N)

#clustering parameters
eps=0.1
minPoints=4
#benchmark
for(i in 1:N)
{
  data=test.data(clusters = 4, points.distance = 0.01, points = inputs[i], separation.ratio = 1)
  data.matrix=as.matrix(data) #for dbscan::dbscan
  bench=microbenchmark(dbscan1d(data, eps, minPoints), dbscan::dbscan(data.matrix, eps, minPoints), times=100, unit="ms")
  time.ms=summary(bench)$mean
  times.dbscan1d[i]=time.ms[1]
  times.dbscan[i]=time.ms[2]
}

ylim=c(min(c(times.dbscan1d, times.dbscan)), max(c(times.dbscan1d, times.dbscan)))

plot(1:N, times.dbscan, type="p", col="red", ylim=ylim, ylab="ms")
points(1:N, times.dbscan1d, type="p", col="blue")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)



