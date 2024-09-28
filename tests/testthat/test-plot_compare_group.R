# 加载必要的包
library(testthat)
library(ggplot2)
library(tidyr)
library(dplyr)

# 创建测试数据
set.seed(123)
test_data <- data.frame(
  group = rep(c("A", "B"), each = 10),
  gene1 = rnorm(20),
  gene2 = rnorm(20)
)

test_that("函数能正确处理缺失的分组列", {
  expect_error(
    plot_compare_group(test_data, "nonexistent_column"),
    "指定的分组列不存在于数据框中。"
  )
})

test_that("函数能正确处理分组顺序", {
  p <- plot_compare_group(test_data, "group", group_order = c("B", "A"))
  expect_equal(levels(p$data$group), c("B", "A"))
})

test_that("函数能正确处理填充颜色数量不匹配", {
  expect_error(
    plot_compare_group(test_data, "group", fill_colors = c("red")),
    "fill_colors的长度必须与分组水平的数量相匹配"
  )
})

test_that("函数能正确生成图形对象", {
  p <- plot_compare_group(test_data, "group")
  expect_s3_class(p, "ggplot")
  expect_true("GeomBoxplot" %in% sapply(p$layers, function(layer) class(layer$geom)[1]))
})

test_that("函数能正确处理长格式数据", {
  p <- plot_compare_group(test_data, "group")
  long_data <- tidyr::pivot_longer(test_data, cols = -group, names_to = "gene", values_to = "expression")
  expect_equal(nrow(p$data), nrow(long_data))
  expect_equal(colnames(p$data), c("group", "gene", "expression"))
})

test_that("函数能正确添加统计显著性标记", {
  p <- plot_compare_group(test_data, "group")
  expect_true("GeomText" %in% sapply(p$layers, function(layer) class(layer$geom)[1]))
})
