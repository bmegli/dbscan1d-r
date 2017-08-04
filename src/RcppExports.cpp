// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// dbscan1d
IntegerVector dbscan1d(const NumericVector data, double eps, int minPoints, bool sort);
RcppExport SEXP _dbscan1d_dbscan1d(SEXP dataSEXP, SEXP epsSEXP, SEXP minPointsSEXP, SEXP sortSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< int >::type minPoints(minPointsSEXP);
    Rcpp::traits::input_parameter< bool >::type sort(sortSEXP);
    rcpp_result_gen = Rcpp::wrap(dbscan1d(data, eps, minPoints, sort));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dbscan1d_dbscan1d", (DL_FUNC) &_dbscan1d_dbscan1d, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_dbscan1d(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
