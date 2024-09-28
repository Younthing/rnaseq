#' Perform Analysis on Signature Scores
#'
#' @param signature_scores A data frame containing signature scores.
#' @param group_comparisons List of group comparisons.
#' @param cols Columns to keep in the long format transformation. Default is c("ID", "group").
#' @param test_method A function from rstatix package for statistical testing. Default is rstatix::wilcox_test.
#' @param adjust_method Method for p-value adjustment. Default is "BH".
#' @param padjust_threshold Optional threshold for filtering results based on adjusted p-value.
#'
#' @return A list containing long-format data and statistical test results.
#' @export
#'
#' @examples
#' # Example usage will go here
stat_test_group <- function(signature_scores,
                            group_comparisons,
                            cols = c("ID", "group"),
                            test_method = rstatix::wilcox_test,
                            adjust_method = "BH",
                            padjust_threshold = NULL) {
  # 验证 group_comparisons
  all_groups <- unique(signature_scores$group)
  if (!all(unlist(group_comparisons) %in% all_groups)) {
    stop("Invalid group_comparisons. All groups must exist in the data.")
  }

  # 验证 test_method
  if (!is.function(test_method) || !("formula" %in% names(formals(test_method)))) {
    stop("Invalid test_method. It must be a function that accepts a formula.")
  }

  # 数据预处理
  data_long <- signature_scores %>%
    tidyr::pivot_longer(
      cols = -tidyr::all_of(cols),
      names_to = "Variable",
      values_to = "Score"
    )
  message("数据预处理完成。")

  # 统计检验
  message("进行统计检验...")
  stat_tests <- data_long %>%
    dplyr::group_by(Variable) %>%
    test_method(Score ~ group, comparisons = group_comparisons) %>%
    rstatix::adjust_pvalue(method = adjust_method) %>%
    rstatix::add_significance("p.adj") %>%
    dplyr::arrange(p.adj)
  message("统计检验完成。")

  # 根据 padjust_threshold 过滤 stat_tests
  if (!is.null(padjust_threshold)) {
    message("根据 padjust_threshold 过滤统计检验结果...")
    stat_tests <- stat_tests %>%
      dplyr::filter(p.adj < padjust_threshold)
    message("过滤完成。")
  }

  # 返回结果
  return(stat_tests)
}
