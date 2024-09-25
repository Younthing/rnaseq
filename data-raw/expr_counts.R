# data-raw/expr_counts.R

# 读取原始数据
expr_counts <- read.csv("inst/extdata/数据去重-counts.csv", row.names = 1)
expr_fpkm <- read.csv("inst/extdata/数据去重-fpkm.csv", row.names = 1)
group_table <- read.csv("inst/extdata/group_169304.csv")

# 保存为 .rda 文件

expr_fpkm

usethis::use_data(expr_counts, group_table, expr_fpkm, overwrite = TRUE)
