## ------------------------------------------------------------------------
library(dbscan1d) #our library
library(microbenchmark) #benchmarking

## ------------------------------------------------------------------------
library(fpc) # fpc::dbscan, no K-D trees O(n^2)
library(dbscan) # dbscan::dbscan, fast K-D trees backed with C++ implementation


## ------------------------------------------------------------------------

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


## ------------------------------------------------------------------------

plot1d=function(data, ...)
{
  x=data
  y=jitter(rep(1, length(data)), amount=1)
  plot(x, y, type="p", axes=FALSE, ylab="",xlab="", ...)
  axis(side=1, at=c(min(x), max(x)), labels=c('',''),lwd.ticks = c(0, 0), pos=0, line=1)
}    

plot1d(data)


## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------

data=test.data(clusters = 4, points.distance = 0.01, points = 100, separation.ratio = 1)
data.matrix=as.matrix(data) #for dbscan::dbscan

eps=0.1
minPoints=4

bench=microbenchmark(dbscan1d(data, eps, minPoints), dbscan::dbscan(data.matrix, eps, minPoints), fpc::dbscan(data, eps, minPoints), times=100)

print(bench)
plot(bench)


## ------------------------------------------------------------------------
data=test.data(clusters = 4, points.distance = 0.01, points = 1000, separation.ratio = 1)
data.matrix=as.matrix(data) #for dbscan::dbscan

eps=0.1
minPoints=4

bench=microbenchmark(dbscan1d(data, eps, minPoints),dbscan::dbscan(data.matrix, eps, minPoints), times=100)

print(bench)
plot(bench)


## ------------------------------------------------------------------------
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



