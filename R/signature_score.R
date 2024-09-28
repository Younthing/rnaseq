#' @title 计算基因表型得分并进行统计检验
#'
#' @description
#' 该函数计算基因表型（signature）得分，并在不同组之间进行统计检验。用户可以选择返回或保存结果。
#'
#' @usage
#' signature_score(
#'   expression_data= expr_fpkm,
#'   group_data = group_table,
#'   sample_column = "sample",
#'   gene_sets = NULL,
#'   gene_set_name = "HALLMARK",
#'   method = "ssgsea",
#'   num_cores = 20,
#'   min_gene_count = 5,
#'   group_comparisons = list(c("Normal", "Disorder")),
#'   output_dir = NULL,
#'   padjust_threshold = NULL
#' )
#'
#' @param expression_data 数据框，基因表达数据，行为基因，列为样本（例如经过标准化处理的 TPM、FPKM 等数据）。
#' @param group_data 数据框，包含样本信息，需包含样本标识列和分组列。
#' @param sample_column 字符串，表示在 `group_data` 中作为样本标识的列名（默认“sample”）。
#' @param gene_sets 列表，可选提供自定义基因集（默认为 NULL）。
#' @param gene_set_name 字符串，用于标识要使用的内置基因集的名称前缀（默认为“HALLMARK”）。
#' @param method 字符串，计算表型得分的方法（默认为“ssgsea”）, 'pca'(矩阵不能包含方差为0的变量), 'ssgsea', 'zscore','integration'。
#' @param num_cores 数值，使用的核心数（默认为 20）。
#' @param min_gene_count 数值，最少基因数量（默认为 5）。
#' @param group_comparisons 列表，指定进行统计检验的分组比较（默认为 list(c("Normal", "Disorder")）。
#' @param output_dir 字符串，结果保存的目录（默认为 NULL，表示不保存）。
#' @param padjust_threshold 数值，用于根据调整后的 p 值过滤统计检验结果（默认为 NULL）。
#'
#' @return 列表，包含表型得分和统计检验结果。
#'
#' @keywords 基因表型, 统计检验, 数据分析
#'
#' @import rstatix
#' @importFrom IOBR calculate_sig_score
#' @importFrom readr write_csv
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
signature_score <- function(
    expression_data,
    group_data,
    sample_column = "sample",
    gene_sets = NULL,
    gene_set_name = "HALLMARK",
    method = "ssgsea",
    num_cores = 20,
    min_gene_count = 5,
    group_comparisons = list(c("Normal", "Disorder")),
    output_dir = NULL,
    padjust_threshold = NULL) {
  # 内置数据映射
  do_list <- list(
    GO = gene_ontology_sets,
    KEGG = kegg_pathway_sets,
    HALLMARK = msigdb_hallmark_sets,
    SIGNATURE_TME = tme_signature_sets,
    SIGNATURE_METABOLISM = metabolism_signature_sets,
    SIGNATURE_COLLECTION = misc_signature_collection,
    SIGNATURE_TUMOR = tumor_signature_sets
  )

  # 如果没有提供 gene_sets，则根据 gene_set_name 查找内置数据
  if (is.null(gene_sets) && !is.null(gene_set_name)) {
    gene_sets <- do_list[[gene_set_name]]
    if (is.null(gene_sets)) {
      stop(paste(
        "未找到与给定 gene_set_name", gene_set_name,
        "相关的基因集。可用选项包括：",
        paste(names(do_list), collapse = ", "), "。"
      ))
    }
  } else if (!is.null(gene_sets) && !is.null(gene_set_name)) {
    message("用户提供了 gene_sets，忽略 gene_set_name 的查找。")
  }

  # 参数验证
  if (!is.null(padjust_threshold)) {
    if (!is.numeric(padjust_threshold) || padjust_threshold <= 0 || padjust_threshold >= 1) {
      stop("padjust_threshold 必须是介于 0 和 1 之间的数值。")
    }
  }

  # 计算表型得分
  message("计算表型得分...")
  signature_scores <- IOBR::calculate_sig_score(
    pdata = group_data,
    column_of_sample = sample_column,
    eset = expression_data,
    signature = gene_sets,
    method = method,
    parallel.size = num_cores,
    mini_gene_count = min_gene_count
  )
  message("表型得分计算完成。")

  # 数据预处理
  message("数据预处理...")
  data_long <- signature_scores %>%
    tidyr::pivot_longer(
      cols = -c(ID, group),
      names_to = "GeneSet",
      values_to = "Score"
    )
  message("数据预处理完成。")

  # 统计检验
  message("进行统计检验...")
  stat_tests <- data_long %>%
    dplyr::group_by(GeneSet) %>%
    rstatix::wilcox_test(Score ~ group, comparisons = group_comparisons) %>%
    rstatix::adjust_pvalue(method = "BH") %>%
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

  # 构建结果列表
  result <- list(
    signature_scores = signature_scores,
    stat_tests = stat_tests
  )

  # 保存结果到文件
  if (!is.null(output_dir)) {
    message("保存结果到文件...")
    # 创建输出目录（如果不存在）
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    # 二级目录是 method 参数
    method_dir <- file.path(output_dir, method)
    if (!dir.exists(method_dir)) {
      dir.create(method_dir)
    }

    # 保存 signature_scores 到 CSV，文件名中包含 gene_set_name
    gene_set_label <- ifelse(is.null(gene_set_name), "custom", gene_set_name)
    signature_scores_file <- file.path(method_dir, paste0("signature_scores_", gene_set_label, ".csv"))
    readr::write_csv(signature_scores, signature_scores_file)

    # 保存 stat_tests 到 CSV，文件名中包含 gene_set_name
    stat_tests_file <- file.path(method_dir, paste0("stat_tests_", gene_set_label, ".csv"))
    readr::write_csv(stat_tests, stat_tests_file)

    message("结果保存完成。")
  }

  # 返回结果
  return(result)
}



#' @title 单个基因集的表型评分箱线图
#' @description 为单个基因集创建表型评分的箱线图。
#'
#' @param df_long 一个长格式的数据框，包含表型评分。应包含以下列："ID"、"group"、"GeneSet"、"Score"。
#' @param gene_set 要绘制的基因集的名称。
#' @param x x轴变量的名称（默认是"group"）。
#' @param y y轴变量的名称（默认是"Score"）。
#' @param fill 填充变量的名称（默认是"group"）。
#' @param palette 用于绘图的颜色向量。
#' @param group_levels 一个字符串向量，指定 group 的排序。
#' @param ... 传递给 \code{\link[ggpubr]{ggboxplot}} 的其他参数。
#'
#' @return 一个代表箱线图的 ggplot 对象。
#'
#' @details 该函数使用 \code{ggpubr::ggboxplot} 为指定的基因集创建表型评分的箱线图。可以通过传递额外的参数来自定义图形。
#'
#' @examples
#' # 示例用法：
#' \dontrun{
#' # 假设 df_long 是长格式的数据框
#' p <- plot_signature(df_long, gene_set = "GeneSet1", group_levels = c("A", "B", "C"))
#' print(p)
#' }
#'
#' @import ggplot2
#' @importFrom ggpubr ggboxplot stat_compare_means
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom forcats fct_relevel
#' @export
plot_signature <- function(df_long, gene_set, x = "group", y = "Score",
                           fill = "group", palette = c("#2E86C1", "#E74C3C", "#27AE60"),
                           group_levels = NULL, ...) {
  # 过滤出特定基因集的数据
  df_plot <- df_long %>%
    dplyr::filter(.data$GeneSet == gene_set)

  # 如果提供了 group_levels，重新排序 group 变量
  if (!is.null(group_levels)) {
    df_plot[[x]] <- forcats::fct_relevel(df_plot[[x]], group_levels)
  }

  # 使用增强的美学创建箱线图
  p <- ggpubr::ggboxplot(
    df_plot,
    x = x,
    y = y,
    fill = fill,
    palette = palette,
    ...
  ) +
    ggpubr::stat_compare_means(
      method = "wilcox.test",
      label = "p.signif", # 显示格式化的p值
      label.y = max(df_plot[[y]], na.rm = TRUE) * 1.1,
      label.x = 1.5,
      size = 5
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.15))) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "grey80"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
      legend.position = "none"
    ) +
    ggplot2::labs(
      x = "",
      y = "Signature Score",
      title = gene_set
    )
  return(p)
}



#' @title 并行绘制多个基因集的表型评分箱线图
#' @description 并行创建并保存多个基因集的表型评分箱线图。
#'
#' @param df_long 一个长格式的数据框，包含表型评分。默认包含以下列："ID"、"group"、"GeneSet"、"Score"。
#' @param gene_sets 要绘制的基因集名称向量。
#' @param output_dir 保存绘图的目录（默认是"results"）。
#' @param x x轴变量的名称（默认是"group"）。
#' @param y y轴变量的名称（默认是"Score"）。
#' @param fill 填充变量的名称（默认是"group"）。
#' @param palette 用于绘图的颜色向量。
#' @param num_cores 并行运行的核心数量（默认是 40）。
#' @param ... 传递给 \code{\link{plot_signature}} 的其他参数。
#'
#' @return 返回不可见的NULL。
#'
#' @details 该函数使用 \code{furrr::future_map} 并行创建多个基因集的表型评分箱线图，并将图形保存到指定的输出目录。
#'
#' @examples
#' # 示例用法：
#' \dontrun{
#' df_long <- tidyr::pivot_longer(score_res$signature_scores, -c(ID, group), names_to = "GeneSet", values_to = "Score")
#'
#' plot_signature_parallel(
#'   df_long = df_long,
#'   gene_sets = score_res$stat_tests$GeneSet,
#'   group_levels = c("Control", "Disorder")
#' )
#' }
#' @importFrom fs dir_create
#' @importFrom furrr future_map
#' @importFrom future plan multisession availableCores
#' @importFrom ggplot2 ggsave
#' @export
plot_signature_parallel <- function(df_long, gene_sets, output_dir = "results", x = "group", y = "Score",
                                    fill = "group", palette = c("#2E86C1", "#E74C3C", "#27AE60"), group_levels = NULL, ...) {
  # 确保输出目录存在
  fs::dir_create(output_dir)
  future::plan(
    future::multicore(
      workers = ifelse(future::availableCores(constraints = "multicore") < 128,
        (future::availableCores(constraints = "multicore") / 2 + 2), 80
      )
    )
  )

  # 创建并保存单个基因集的图形
  save_plot <- function(gene_set) {
    p <- plot_signature(
      df_long = df_long,
      gene_set = gene_set,
      x = x,
      y = y,
      fill = fill,
      palette = palette,
      group_levels = group_levels,
      ...
    )
    # 使用一致的文件路径保存图形
    output_file <- file.path(output_dir, paste0(gene_set, ".pdf"))
    ggplot2::ggsave(filename = output_file, plot = p, dpi = 300, width = 8, height = 6)
  }

  # 使用 future_map 并行处理
  furrr::future_map(
    gene_sets,
    save_plot,
    .progress = TRUE
  )

  message("所有图形已生成并保存。")
  invisible(NULL)
}
