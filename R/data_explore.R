#' @title PCA分析并绘制带有分组变量的PCA图
#'
#' @description 对提供的数据执行主成分分析（PCA），并根据指定的分组变量（如治疗、条件等）生成PCA图。
#'
#' @param data 数值型矩阵或数据框，样本在行，变量在列。
#' @param samples_in_rows 逻辑值。如果为\code{TRUE}，则数据中样本在行，变量在列；如果为\code{FALSE}，数据将被转置。
#' @param scale_data 逻辑值。如果为\code{TRUE}，在执行PCA之前对数据进行缩放。
#' @param log_transform 逻辑值。如果为\code{TRUE}，对数据应用\code{log2}转换。
#' @param group_data 包含样本元数据的数据框，包括样本ID和分组变量。
#' @param id_column 字符串。在\code{group_data}中包含样本ID的列名。
#' @param group_column 字符串。在\code{group_data}中用于分组样本的列名。
#' @param geom_individuals 字符串。在PCA图中表示个体的几何形状（例如，"point"）。
#' @param color_palette 字符串。用于着色组的RColorBrewer调色板的名称。
#' @param repel_labels 逻辑值。如果为\code{TRUE}，使用\code{ggrepel}避免文本标签重叠。
#' @param n_components 整数。要计算的主成分数量。
#' @param axes 整数向量。要绘制的主成分。
#' @param add_ellipses 逻辑值。如果为\code{TRUE}，在组周围添加置信椭圆。
#'
#' @return ggplot对象，表示PCA图。
#'
#' @usage
#' plot_groups_pca(
#'     data = expr_tpm,
#'     group_data = group_table,
#'     samples_in_rows = FALSE,
#'     scale_data = TRUE,
#'     log_transform = FALSE,
#'     id_column = "sample",
#'     group_column = "group",
#'     geom_individuals = "point",
#'     color_palette = "Set1",
#'     repel_labels = FALSE,
#'     n_components = 50,
#'     axes = c(1, 2),
#'     add_ellipses = TRUE
#' )
#'
#' @examples
#' \dontrun{
#' # 假设 'expr_tpm' 是您的数据矩阵，'group_table' 是您的分组数据
#' plot_groups_pca(
#'   data = expr_tpm,
#'   group_data = group_table,
#'   samples_in_rows = FALSE,
#'   scale_data = TRUE,
#'   log_transform = FALSE,
#'   id_column = "sample",
#'   group_column = "group",
#'   geom_individuals = "point",
#'   color_palette = "Set1",
#'   repel_labels = FALSE,
#'   n_components = 50,
#'   axes = c(1, 2),
#'   add_ellipses = TRUE
#' )
#' }
#'
#' @details
#' 该函数使用\code{FactoMineR}包执行PCA，并使用\code{factoextra}包生成PCA图。样本根据指定的分组变量着色。请确保数据和表型数据中的样本ID匹配。
#'
#' @keywords PCA 主成分分析 可视化 分组
#'
#' @importFrom FactoMineR PCA
#' @importFrom factoextra fviz_pca_ind
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggplot2 ggplot
#' @export
plot_groups_pca <- function(
    data,
    group_data,
    samples_in_rows = FALSE,
    scale_data = TRUE,
    log_transform = FALSE,
    id_column = "sample",
    group_column = NULL,
    geom_individuals = "point",
    color_palette = "Set1",
    repel_labels = FALSE,
    n_components = 50,
    axes = c(1, 2),
    add_ellipses = TRUE) {
  # 验证输入数据
  if (is.null(group_data)) {
    stop("group_data 不能为空。请提供包含样本元数据的数据框。")
  }
  if (is.null(group_column)) {
    stop("group_column 不能为空。请指定用于分组的列名。")
  }
  if (!is.data.frame(group_data)) {
    stop("group_data 必须是数据框。")
  }
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data 必须是矩阵或数据框。")
  }
  if (!(id_column %in% colnames(group_data))) {
    stop(paste("id_column '", id_column, "' 不存在于 group_data 中。", sep = ""))
  }
  if (!(group_column %in% colnames(group_data))) {
    stop(paste("group_column '", group_column, "' 不存在于 group_data 中。", sep = ""))
  }

  # 如果样本不在行，将数据转置
  if (!samples_in_rows) {
    data <- t(data)
  }

  # 确保数据是数值矩阵
  data <- as.matrix(data)
  if (!is.numeric(data)) {
    stop("data 必须是数值型。")
  }

  # 如果需要对数转换，应用log2转换
  if (log_transform) {
    data <- log2(data + 1)
  }

  # 检查并移除方差为零的列
  zero_variance_cols <- apply(data, 2, function(x) var(x, na.rm = TRUE) == 0)
  if (any(zero_variance_cols)) {
    warning("数据中存在方差为零的变量，这些变量将在PCA分析中被移除。")
    data <- data[, !zero_variance_cols, drop = FALSE]
  }

  # 如果需要，缩放数据
  if (scale_data) {
    data <- scale(data)
  }

  # 检查并处理数据中的缺失值和无限值
  if (any(is.na(data)) || any(!is.finite(data))) {
    warning("数据中存在缺失值或无限值，将使用列的平均值进行填充。")
    data[!is.finite(data)] <- NA
    data <- apply(data, 2, function(x) {
      x[is.na(x)] <- mean(x, na.rm = TRUE)
      return(x)
    })
    data <- as.matrix(data)
  }

  # 准备 group_data
  sample_ids <- rownames(data)
  group_data <- group_data[group_data[[id_column]] %in% sample_ids, ]
  group_data <- group_data[match(sample_ids, group_data[[id_column]]), ]

  # 检查是否有缺失值
  if (any(is.na(group_data[[group_column]]))) {
    stop("在 group_data 中，一些样本的 group_column 值缺失。")
  }

  # 执行PCA
  pca_result <- FactoMineR::PCA(data, ncp = n_components, graph = FALSE)

  # 生成用于绘图的颜色
  num_groups <- length(unique(group_data[[group_column]]))

  # 检查调色板是否足够
  if (num_groups <= 9) {
    # 如果组数少于3，使用至少3种颜色
    if (num_groups < 3) {
      colors <- RColorBrewer::brewer.pal(3, color_palette)[1:num_groups]
    } else {
      colors <- RColorBrewer::brewer.pal(num_groups, color_palette)
    }
  } else {
    # 如果组超过9，生成更多颜色
    colors <- colorRampPalette(RColorBrewer::brewer.pal(9, color_palette))(num_groups)
  }

  # 绘制PCA图
  pca_plot <- factoextra::fviz_pca_ind(
    pca_result,
    axes = axes,
    geom.ind = geom_individuals,
    col.ind = group_data[[group_column]],
    palette = colors,
    repel = repel_labels,
    addEllipses = add_ellipses,
    legend.title = group_column
  )

  return(pca_plot)
}
