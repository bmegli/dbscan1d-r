# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title Efficent implementation of DBSCAN alghorithm for 1D data
#' @description Perfoms clustering with DBSCAN alghorithm optimized for one dimensional data.
#' For sorted data (pass FALSE to sort parameter) the complexity is O(n) where n is the length of the data vector.
#' If sorting is needed the algorithm complexity is O(nlogn).
#' 
#' @param data numeric vector with points to cluster
#' @param eps the epsilon-neighborhood value for clustering
#' @param minPoints the number of points for core-point criterion
#' @param sort whether to sort the data, pass FALSE if calling for sorted data
#' @return a vector of cluster labels
#'
#' @examples 
#' dbscan1d(c(9,8,7, 1,2,3, 15,16,17), eps=1, minPoints=2)
#'
#' @export
dbscan1d <- function(data, eps, minPoints, sort = TRUE) {
    .Call(`_dbscan1d_dbscan1d`, data, eps, minPoints, sort)
}

