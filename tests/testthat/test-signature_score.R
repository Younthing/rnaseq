library(testthat)
library(dplyr)

test_that("signature_score 函数正确处理有效输入", {
  # 准备测试数据
  # 执行函数
  result <- signature_score(
    expression_data = expr_fpkm,
    group_data = group_table,
    sample_column = "sample",
    gene_set_name = "HALLMARK",
    method = "ssgsea",
    num_cores = 1,
    group_comparisons = list(c("Normal", "Disorder"))
  )

  # 验证结果
  expect_type(result, "list")
  expect_true("signature_scores" %in% names(result))
  expect_true("stat_tests" %in% names(result))
})
