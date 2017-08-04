#' @title Efficient specialization of DBSCAN clustering algorithm for 1D data
#'
#' @description
#' A O(nlogn) pessimistic complexity implementation of DBSCAN
#' (Density Based Clustering of Applications with Noise) optimized for 1D data.
#' If data is already sorted the algorithm runs in (n) time.
#'
#' @useDynLib dbscan1d
#' @name dbscan1d-package
#' @docType package
#' @importFrom Rcpp sourceCpp
invisible ( NULL )

