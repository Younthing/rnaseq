#include <Rcpp.h>
#include <curl/curl.h>

using namespace Rcpp;

// 用于保存下载的数据
size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
    std::string data((const char *)ptr, (size_t)size * nmemb);
    *((std::stringstream *)stream) << data << std::endl;
    return size * nmemb;
}

// [[Rcpp::export]]
std::string download_data(std::string url)
{
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init();
    std::stringstream out;
    if (curl)
    {
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &out);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        if (res != CURLE_OK)
        {
            Rcpp::stop("Failed to download data from URL");
        }
    }
    return out.str();
}



#include <omp.h>

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
NumericVector parallel_vector_addition(NumericVector a, NumericVector b) {
  int n = a.size();
  if (n != b.size()) {
    Rcpp::stop("Vectors must be of the same length");
  }
  NumericVector result(n);
  
  #pragma omp parallel for
  for (int i = 0; i < n; ++i) {
    result[i] = a[i] + b[i];
  }
  return result;
}
