#' 创建带有统计显著性的分组比较图
#'
#' @description
#' 该函数接受表达数据和分组信息，将数据转换为长格式，
#' 并创建一个带有统计显著性指标的箱线图。
#'
#' @param data 包含表达数据和分组信息的数据框。
#' @param group_col 指定分组列名的字符串。
#' @param group_order 指定所需的分组水平顺序的字符向量。
#'        如果为NULL，则使用现有顺序。
#' @param fill_colors 用于分组的颜色向量。如果为NULL，则自动生成。
#'        颜色数量应与分组水平数量相匹配。
#'
#' @return 表示分组比较图的ggplot对象。
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer all_of
#' @importFrom rlang .data
#' @importFrom stats wilcox.test
#' @importFrom ggpubr stat_compare_means
#'
#' @export
plot_compare_group <- function(data, group_col,
                               group_order = NULL,
                               fill_colors = NULL) {
  if (!group_col %in% colnames(data)) {
    stop("指定的分组列不存在于数据框中。")
  }

  data[[group_col]] <- factor(data[[group_col]], levels = if (!is.null(group_order)) group_order else unique(data[[group_col]]))

  group_levels <- levels(data[[group_col]])

  if (is.null(fill_colors)) {
    fill_colors <- scales::hue_pal()(length(group_levels))
  } else if (length(fill_colors) != length(group_levels)) {
    stop("fill_colors的长度必须与分组水平的数量相匹配")
  }

  names(fill_colors) <- group_levels

  long_data <- tidyr::pivot_longer(data,
    cols = -tidyr::all_of(group_col),
    names_to = "gene",
    values_to = "expression"
  )

  significance_levels <- c("****" = 0.0001, "***" = 0.001, "**" = 0.01, "*" = 0.05, "ns" = 1)

  p <- ggplot(long_data, aes(
    x = .data[["gene"]],
    y = .data[["expression"]],
    fill = .data[[group_col]]
  )) +
    geom_boxplot() +
    ggpubr::stat_compare_means(aes(group = .data[[group_col]]),
      method = "wilcox.test",
      label = "p.signif",
      symnum.args = list(
        cutpoints = sort(significance_levels, decreasing = TRUE),
        symbols = names(significance_levels)
      )
    ) +
    scale_fill_manual(values = fill_colors) +
    labs(x = NULL, y = "expression", fill = "group") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}
