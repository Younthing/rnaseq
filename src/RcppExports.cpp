// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// download_data
std::string download_data(std::string url);
RcppExport SEXP _rnaseq_download_data(SEXP urlSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type url(urlSEXP);
    rcpp_result_gen = Rcpp::wrap(download_data(url));
    return rcpp_result_gen;
END_RCPP
}
// parallel_vector_addition
NumericVector parallel_vector_addition(NumericVector a, NumericVector b);
RcppExport SEXP _rnaseq_parallel_vector_addition(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(parallel_vector_addition(a, b));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rnaseq_download_data", (DL_FUNC) &_rnaseq_download_data, 1},
    {"_rnaseq_parallel_vector_addition", (DL_FUNC) &_rnaseq_parallel_vector_addition, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_rnaseq(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
