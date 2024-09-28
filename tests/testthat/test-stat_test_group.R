library(testthat)
library(dplyr)
library(tidyr)
library(rstatix)

# 创建模拟数据
set.seed(123)
test_data <- data.frame(
  ID = 1:100,
  group = rep(c("A", "B"), each = 50),
  Variable1 = rnorm(100),
  Variable2 = rnorm(100)
)

test_that("test_group works correctly", {
  result <- test_group(
    signature_scores = test_data,
    group_comparisons = list(c("A", "B")),
    cols = c("ID", "group"),
    test_method = rstatix::wilcox_test,
    adjust_method = "BH",
    padjust_threshold = NULL
  )

  # 检查返回值的结构
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Variable", "group1", "group2", "p", "p.adj", "p.adj.signif") %in% names(result)))

  # 检查结果数量
  expect_equal(nrow(result), 2) # 应该有两行，对应两个Variable

  # 检查 p 值和调整后的 p 值
  expect_true(all(result$p >= 0 & result$p <= 1))
  expect_true(all(result$p.adj >= 0 & result$p.adj <= 1))

  # 测试 padjust_threshold
  result_filtered <- test_group(
    signature_scores = test_data,
    group_comparisons = list(c("A", "B")),
    padjust_threshold = 0.05
  )
  expect_true(all(result_filtered$p.adj < 0.05))

  # 测试自定义检验方法
  result_custom <- test_group(
    signature_scores = test_data,
    group_comparisons = list(c("A", "B")),
    test_method = rstatix::t_test
  )
  expect_true("statistic" %in% names(result_custom)) # t-test 特有的输出

  # 测试自定义列名
  result_custom_cols <- test_group(
    signature_scores = test_data,
    group_comparisons = list(c("A", "B")),
    cols = c("ID", "group")
  )
  expect_equal(nrow(result_custom_cols), 2) # 仍然应该有两行，对应两个Variable
})

test_that("test_group handles errors", {
  # 测试缺少必要参数
  expect_error(test_group())

  # 测试无效的 group_comparisons
  expect_error(
    test_group(test_data, group_comparisons = list(c("C", "D"))),
    "Invalid group_comparisons. All groups must exist in the data."
  )

  # 测试无效的检验方法
  expect_error(
    test_group(test_data, group_comparisons = list(c("A", "B")), test_method = mean),
    "Invalid test_method. It must be a function that accepts a formula."
  )
})

test_that("test_group messages are correct", {
  expect_message(
    test_group(test_data, group_comparisons = list(c("A", "B"))),
    "数据预处理完成。"
  )
  expect_message(
    test_group(test_data, group_comparisons = list(c("A", "B"))),
    "进行统计检验..."
  )
  expect_message(
    test_group(test_data, group_comparisons = list(c("A", "B"))),
    "统计检验完成。"
  )
  expect_message(
    test_group(test_data, group_comparisons = list(c("A", "B")), padjust_threshold = 0.05),
    "根据 padjust_threshold 过滤统计检验结果..."
  )
  expect_message(
    test_group(test_data, group_comparisons = list(c("A", "B")), padjust_threshold = 0.05),
    "过滤完成。"
  )
})
