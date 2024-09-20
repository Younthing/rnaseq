#include <Rcpp.h>
using namespace Rcpp;

//' @title Fancy Print Messages
//' @description
//' Prints messages with fancy decorations.
//'
//' @param messages A character vector of messages to print.
//' @return None. Messages are printed to the console.
//' @examples
//' fancy_print(c("Hello", "World"))
//' @export
// [[Rcpp::export]]
void fancy_print(CharacterVector messages)
{
  // 将 R 的 CharacterVector 转换为标准的字符串向量
  std::vector<std::string> msgs = Rcpp::as<std::vector<std::string>>(messages);

  // 定义一个 lambda 表达式，用于装饰字符串
  auto decorate = [](const std::string &msg)
  {
    return "✨ " + msg + " ✨";
  };

  // 使用范围循环遍历消息
  for (const auto &msg : msgs)
  {
    // 装饰当前消息
    std::string decorated_msg = decorate(msg);

    // 输出装饰后的消息到控制台
    Rcpp::Rcout << decorated_msg << std::endl;
  }
}
