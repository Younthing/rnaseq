// 包含 Rcpp 头文件
// system.file("include", package = "Rcpp")
#include <Rcpp.h>
using namespace Rcpp;

//' Multiply a number by two
//'
//' @param x A single integer.
//' @export
// [[Rcpp::export]]
// [[Rcpp::interfaces(r, cpp)]]
int timesTwo(int x)
{
   return x * 2;
}