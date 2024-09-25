#' @title 检测表达矩阵中的离群样本
#' @description 该函数用于识别表达矩阵数据中可能的离群样本，并可选择性地绘制和保存相关的图形。
#' @param expression_data 数据框，tpm or fpkm表达矩阵，行为基因，列为样本。
#' @param z_threshold 数值，用于确定离群样本的 Z 得分阈值，默认值为 -3。
#' @param plot_hclust 逻辑值，是否绘制样本的层次聚类图，默认 FALSE。
#' @param show_plot 逻辑值，是否在屏幕上显示绘制的图形，默认 TRUE。
#' @param hclust_filename 字符串，保存层次聚类图的文件名（包括路径），如果为 NULL 则不保存，默认 NULL。
#' @param connectivity_filename 字符串，保存样本连接度图的文件名（包括路径），如果为 NULL 则不保存，默认 NULL。
#' @return 返回可能的离群样本的名称向量。
#' @examples
#' \dontrun{
#' # 示例代码
#' outliers <- find_outliers(
#'   expression_data = expr_fpkm,
#'   z_threshold = -3,
#'   plot_hclust = TRUE,
#'   show_plot = TRUE,
#'   hclust_filename = NULL,
#'   connectivity_filename = NULL
#' )
#' expr_tpm <- expr_tpm[, !colnames(expr_tpm) %in% outline_sample]
#' }
#' @details
#' 该函数首先计算表达矩阵中样本之间的双相关系数，然后基于此构建样本之间的连接度网络。
#' 通过计算每个样本的连接度的 Z 得分，可以识别出可能的离群样本，即那些 Z 得分低于指定阈值（`z_threshold`）的样本。
#' 可以选择绘制并保存层次聚类图和样本连接度图，以便进一步分析。
#' @keywords 离群样本检测 表达矩阵 WGCNA
#' @import ggplot2
#' @importFrom WGCNA bicor fundamentalNetworkConcepts
#' @importFrom rlang .data
#' @export
find_outliers <- function(
    expression_data,
    z_threshold = -2,
    plot_hclust = FALSE,
    show_plot = TRUE,
    hclust_filename = NULL,
    connectivity_filename = NULL) {
  # 如果需要，绘制并保存层次聚类图
  if (plot_hclust) {
    sample_tree <- hclust(dist(t(expression_data)), method = "average")
    if (show_plot) {
      plot(sample_tree, main = "样本层次聚类树")
    }
    if (!is.null(hclust_filename)) {
      pdf(hclust_filename, width = 20, height = 10)
      plot(sample_tree, main = "样本层次聚类树")
      dev.off()
    }
  }

  # 使用 WGCNA 计算样本连接度的 Z 得分
  normalized_adjacency <- (0.5 + 0.5 * WGCNA::bicor(expression_data))^2
  network_summary <- WGCNA::fundamentalNetworkConcepts(normalized_adjacency)
  connectivity <- network_summary$Connectivity

  # 手动计算 Z 得分，保留名称
  connectivity_mean <- mean(connectivity, na.rm = TRUE)
  connectivity_sd <- sd(connectivity, na.rm = TRUE)
  connectivity_zscore <- (connectivity - connectivity_mean) / connectivity_sd

  # 准备绘图数据
  connectivity_plot_data <- data.frame(
    Sample_Name = names(connectivity_zscore),
    Z_score = as.vector(connectivity_zscore),
    Sample_Num = 1:length(connectivity_zscore)
  )
  Sample_Name <- Z_score <- Sample_Num <- NULL
  # 创建样本连接度图
  connectivity_plot <- ggplot(connectivity_plot_data, aes(x = Sample_Num, y = Z_score, label = Sample_Name)) +
    geom_text(size = 4, colour = "red") +
    geom_hline(yintercept = z_threshold) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    ) +
    xlab("sample") +
    ylab("Z score") +
    ggtitle("sample connectivity")

  # 显示或保存样本连接度图
  if (show_plot) {
    print(connectivity_plot)
  }
  if (!is.null(connectivity_filename)) {
    ggsave(filename = connectivity_filename, plot = connectivity_plot, width = 8, height = 8)
  }

  # 识别可能的离群样本
  outlier_samples <- colnames(expression_data)[connectivity_zscore < z_threshold]
  message(paste0("--- 当 Z 得分阈值为 ", z_threshold, " 时"))
  message("--- 可能的离群样本：")
  print(outlier_samples)
  return(outlier_samples)
}



#' @title 转换计数矩阵为 TPM（每百万转录本）
#' @description 该函数封装了 `count2tpm` 函数，用于将基因表达的计数矩阵转换为 TPM（每百万转录本，Transcripts Per Million）。
#' 用户只需提供表达计数矩阵和物种信息，函数会自动选择适当的参数进行转换。
#'
#' @param counts 数据框，基因表达的计数矩阵，行为基因，列为样本。每个值应为非负整数，表示每个基因在每个样本中的计数值。
#' @param org 字符串，指定物种。可选值为 "hsa"（人类，默认）或 "mmus"（小鼠）。根据物种选择，函数会自动设置 `idType` 和 `source` 参数。
#'
#' @return 返回一个数据框，包含转换后的 TPM 矩阵。行对应基因，列对应样本。每个值表示每个基因在每个样本中的 TPM 值。
#'
#' @usage
#' tpm_matrix <- counts_to_tpm(counts = your_counts_matrix, org = "hsa")
#'
#' @examples
#' \dontrun{
#' # 示例代码
#' # 假设您有一个基因表达的计数矩阵 your_counts_matrix
#'
#' # 将计数矩阵转换为 TPM（默认物种为人类）
#' tpm_matrix_human <- counts_to_tpm(counts = your_counts_matrix)
#'
#' # 将计数矩阵转换为 TPM（物种为小鼠）
#' tpm_matrix_mouse <- counts_to_tpm(counts = your_counts_matrix, org = "mmus")
#' }
#'
#' @details
#' 该函数是对 `count2tpm` 函数的封装，旨在简化用户的操作。用户只需提供表达计数矩阵和物种信息，函数会根据物种自动选择适当的 `idType` 和 `source` 参数。
#'
#' - 对于人类（"hsa"），使用 `idType = "symbol"` 和 `source = "local"`。
#' - 对于小鼠（"mmus"），同样使用 `idType = "symbol"` 和 `source = "local"`。
#'
#' 如果物种不同，用户可以根据需要进一步调整 `idType` 和 `source` 参数。
#'
#' @keywords TPM转换 基因表达计数 物种选择
#' @importFrom IOBR count2tpm
#'
#' @export
counts_to_tpm <- function(counts, org = "hsa") {
  # 检查 org 参数是否有效
  if (!org %in% c("hsa", "mmus")) {
    stop("org 参数必须是 'hsa' 或 'mmus'")
  }

  # 根据 org 设置 idType 和 source
  if (org == "hsa") {
    idType <- "symbol"
    source <- "local"
  } else if (org == "mmus") {
    idType <- "symbol"
    source <- "local"
  }

  # 调用 count2tpm 函数
  tpm_matrix <- IOBR::count2tpm(
    countMat = counts,
    idType = idType,
    org = org,
    source = source
  )

  return(tpm_matrix)
}
