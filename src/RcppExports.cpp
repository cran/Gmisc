// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/Gmisc.h"
#include <Rcpp.h>

using namespace Rcpp;

// calculateLinesAndArrow
Rcpp::List calculateLinesAndArrow(NumericVector x, NumericVector y, NumericVector offset, double end_x, double end_y, double arrow_offset, int rm_intersect);
RcppExport SEXP Gmisc_calculateLinesAndArrow(SEXP xSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP end_xSEXP, SEXP end_ySEXP, SEXP arrow_offsetSEXP, SEXP rm_intersectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< double >::type end_x(end_xSEXP);
    Rcpp::traits::input_parameter< double >::type end_y(end_ySEXP);
    Rcpp::traits::input_parameter< double >::type arrow_offset(arrow_offsetSEXP);
    Rcpp::traits::input_parameter< int >::type rm_intersect(rm_intersectSEXP);
    rcpp_result_gen = Rcpp::wrap(calculateLinesAndArrow(x, y, offset, end_x, end_y, arrow_offset, rm_intersect));
    return rcpp_result_gen;
END_RCPP
}
// gnrlBezierPoints
NumericMatrix gnrlBezierPoints(SEXP& ctrl_points, int length_out);
RcppExport SEXP Gmisc_gnrlBezierPoints(SEXP ctrl_pointsSEXP, SEXP length_outSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type ctrl_points(ctrl_pointsSEXP);
    Rcpp::traits::input_parameter< int >::type length_out(length_outSEXP);
    rcpp_result_gen = Rcpp::wrap(gnrlBezierPoints(ctrl_points, length_out));
    return rcpp_result_gen;
END_RCPP
}
