# 加载测试包
library(testthat)

# 创建模拟数据
set.seed(123)
expr_tpm <- matrix(rnorm(1000), nrow = 100, ncol = 10)
rownames(expr_tpm) <- paste0("Sample", 1:100)
colnames(expr_tpm) <- paste0("Gene", 1:10)

group_table <- data.frame(
  sample = paste0("Sample", 1:100),
  group = rep(c("A", "B"), each = 50)
)

test_that("plot_groups_pca works correctly with valid inputs", {
  pca_plot <- plot_groups_pca(
    data = expr_tpm,
    group_data = group_table,
    samples_in_rows = TRUE,
    scale_data = TRUE,
    log_transform = FALSE,
    id_column = "sample",
    group_column = "group",
    geom_individuals = "point",
    color_palette = "Set1",
    repel_labels = FALSE,
    n_components = 5,
    axes = c(1, 2),
    add_ellipses = TRUE
  )

  expect_s3_class(pca_plot, "gg")
})

test_that("plot_groups_pca handles missing group_data", {
  expect_error(
    plot_groups_pca(
      data = expr_tpm,
      group_data = NULL,
      samples_in_rows = TRUE,
      scale_data = TRUE,
      log_transform = FALSE,
      id_column = "sample",
      group_column = "group"
    ),
    "group_data 不能为空"
  )
})

test_that("plot_groups_pca handles missing group_column", {
  expect_error(
    plot_groups_pca(
      data = expr_tpm,
      group_data = group_table,
      samples_in_rows = TRUE,
      scale_data = TRUE,
      log_transform = FALSE,
      id_column = "sample",
      group_column = NULL
    ),
    "group_column 不能为空"
  )
})

test_that("plot_groups_pca handles non-numeric data", {
  non_numeric_data <- expr_tpm
  non_numeric_data[1, 1] <- "non-numeric"

  expect_error(
    plot_groups_pca(
      data = non_numeric_data,
      group_data = group_table,
      samples_in_rows = TRUE,
      scale_data = TRUE,
      log_transform = FALSE,
      id_column = "sample",
      group_column = "group"
    ),
    "data 必须是数值型"
  )
})

test_that("plot_groups_pca handles zero variance columns", {
  zero_variance_data <- expr_tpm
  zero_variance_data[, 1] <- 0

  expect_warning(
    plot_groups_pca(
      data = zero_variance_data,
      group_data = group_table,
      samples_in_rows = TRUE,
      scale_data = TRUE,
      log_transform = FALSE,
      id_column = "sample",
      group_column = "group"
    ),
    "数据中存在方差为零的变量"
  )
})

test_that("plot_groups_pca handles missing values", {
  missing_value_data <- expr_tpm
  missing_value_data[1, 1] <- NA

  expect_warning(
    plot_groups_pca(
      data = missing_value_data,
      group_data = group_table,
      samples_in_rows = TRUE,
      scale_data = TRUE,
      log_transform = FALSE,
      id_column = "sample",
      group_column = "group"
    ),
    "数据中存在缺失值或无限值"
  )
})
