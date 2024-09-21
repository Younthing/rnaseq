stop("This script is not meant to be run directly.")
# 设置工作目录到你的包的根目录
setwd("rnaseq") # 请将 "bioranger" 替换为您的包目录

# 加载必要的包
library(usethis)
library(devtools)
library(pkgdown)
library(styler)
library(lintr)

# 初始化包
fields_ <- list(
  Package = basename(here::here()),
  Version = "0.0.1",
  Title = "A Package for Rna-seq Analysis",
  Description = "A package for Rna-seq Analysis",
  `Authors@R` = person(
    given = "Fan",
    family = "Xingfu",
    email = "fanxingfu3344@gmail.com",
    role = c("aut", "cre")
  )
)

usethis::create_package(
  path = ".",
  fields = fields_
)


# 初始化 RStudio 项目（这里会新建一个Rstudio的项目文件，如果在vscode则不需要）
usethis::use_rstudio()

# 初始化 Git 仓库（如果尚未初始化）
usethis::use_git(message = "Initial commit")

# 连接到 GitHub（创建 GitHub 仓库并链接到本地仓库）
usethis::use_github()

# 设置许可证（例如，MIT 许可证）
usethis::use_mit_license(copyright_holder = "Fan Xingfu") # 替换为您的姓名

# 设置代码行为准则
usethis::use_code_of_conduct(contact = "fanxingfu3344@gmail.com") # 替换为您的联系邮箱

# 创建 README.Rmd，以便在 README 中包含 R 代码和输出
usethis::use_readme_rmd()

# 启用 Roxygen2 文档，并使用 Markdown 格式
usethis::use_roxygen_md()

# 创建 NEWS.md 文件，用于记录每次版本更新的变更
usethis::use_news_md()


usethis::use_pipe() # 在你的包中使用 magrittr 的管道
# 创建 R 脚本
usethis::use_r("my_function") # 将 "my_function" 替换为您的函数名

# 创建 inst/extdata/ 目录
usethis::use_directory("inst/extdata")
# raw_data <- read.csv("inst/extdata/my_data_raw.csv") # nolintr
usethis::use_data_raw(name = "my_dataset") # 将 "my_dataset" 替换为您的数据集名称

# 包含R对象数据data
usethis::use_data(mydata, overwrite = TRUE)
# 记得文档化数据
#' 数据集：my_data
#'
#' 对数据集的简要描述。
#'
#' @format 数据框（data.frame）包含 X 行和 Y 列：
#' \describe{
#'   \item{变量1}{变量1的描述}
#'   \item{变量2}{变量2的描述}
#'   ...
#' }
#' @source 数据来源或参考文献
"my_data"
# 将数据保存为内部数据，不导出
usethis::use_data(my_data, overwrite = TRUE, internal = TRUE)

# 设置测试环境（使用 testthat 包）
usethis::use_testthat()

# 创建示例测试文件
usethis::use_test("my_function") # 将 "my_function" 替换为您的函数名

# 设置代码覆盖率服务（例如，Codecov）
usethis::use_coverage(type = "codecov")

# 创建 vignette，用于编写长格式的包文档
usethis::use_vignette("introduction") # 使用英文名称，替换为合适的 vignettes 名称

# 构建 vignette（在添加内容后）
devtools::build_vignettes()

# 初始化 pkgdown 网站
usethis::use_pkgdown()

# 配置 pkgdown 网站（可选，自定义网站主题、导航等）
# 可以编辑 _pkgdown.yml 文件以自定义网站

# 本地构建 pkgdown 网站
pkgdown::build_site()

# 设置 GitHub Actions 以进行持续集成（CI）
# 添加 R CMD check 工作流程
usethis::use_github_action("check-standard")

# 添加测试覆盖率工作流程
usethis::use_github_action("test-coverage")

# 添加代码风格检查工作流程（使用 lintr）
usethis::use_github_action("lint")

# 设置推荐的 GitHub Actions（包括 pkgdown 网站部署等）
usethis::use_tidy_github_actions()

# 代码风格格式化（遵循 tidyverse 风格指南）
styler::style_pkg()

# 代码风格检查，查找不符合风格指南的代码
lintr::lint_package()

# 修复 styler 和 lintr 发现的问题后，重新运行以确保所有问题已解决

# 构建并检查包
devtools::document() # 生成文档（从 roxygen2 注释）
devtools::build_readme() # 生成 README.md（从 README.Rmd）
devtools::check() # 运行 R CMD check，检查包的完整性

# 在提交到 CRAN 之前，确保包通过所有检查，没有错误、警告或提示

# 准备发布新版本时
# 使用语义化版本控制，更新版本号
usethis::use_version(which = "minor") # "major", "minor", "patch", "dev"

# 在 NEWS.md 中记录新版本的变更

# 提交并推送所有更改到 GitHub
# git add .
# git commit -m "Prepare for version x.y.z release"
# git push
usethis::use_git("chore: renew base") # 提交所有更改到git
usethis::git_sitrep() # 查看git状态
# 如果包已准备好提交到 CRAN
usethis::use_cran_comments() # 创建 cran-comments.md，记录提交时的注意事项

# 在不同平台和 R 版本上检查包(github actions 中已经配置了)
# devtools::check_win_devel() # Windows 开发版（需要 Rtools）
# devtools::check_rhub() # 使用 R-hub 检查
# devtools::check_mac_release() # macOS 版本

# 提交到 CRAN
devtools::submit_cran()

# 提交后，添加 CRAN 徽章到 README
usethis::use_cran_badge()

# 添加生命周期徽章，指示包的开发阶段
usethis::use_lifecycle_badge(stage = "stable") # "experimental", "stable" 等


# 可选：拼写检查，发现文档中的拼写错误
devtools::spell_check()


# 如果有需要编译的代码，设置必要的基础设施，需要include
# 例如，使用 Rcpp
usethis::use_rcpp("test")
Rcpp::compileAttributes()
# 记得添加Rcpp的文档
#' @useDynLib rnaseq, .registration = TRUE
#' @importFrom Rcpp sourceCpp


# 确保所有依赖在 DESCRIPTION 文件中正确指定
# 将包添加到 Imports 或 Suggests 中
usethis::use_package("dplyr") # 添加到 Imports
usethis::use_package("ggplot2") # 添加到 Suggests
usethis::use_package("forcats")

# 定期更新文档，确保函数的文档与代码保持同步
devtools::document() # 还能构建Cpp的文档
usethis::use_directory("inst/manual")
devtools::build_manual(path = "inst/manual") # 默认上级目录中生成pdf

# 如果 README.Rmd 有更改，重新生成 README.md
devtools::build_readme()

# 在重大更改后，重新构建 pkgdown 网站
pkgdown::build_site()

# 设置贡献指南和行为准则，鼓励他人参与
usethis::use_tidy_contributing()
usethis::use_code_of_conduct()

# 在 GitHub 上添加问题模板
usethis::use_tidy_issue_template()

# 添加常用服务的徽章
usethis::use_circleci_badge()

# 确保包在所有目标平台和 R 版本上通过 R CMD check
devtools::check()

# 在每次更改后，始终重新运行检查,构建ing
devtools::document()
devtools::build()
devtools::install()
devtools::clean_dll() # Removes any previously compiled DLLs

# 记录提交信息，确保版本控制历史清晰可读
# 使用有意义的提交信息，例如：
# git commit -m "Add new feature: XYZ"
# git commit -m "Fix bug in ABC function"
usethis::use_git()
# 推送更改到远程仓库
# git push origin main  # 或者您的默认分支

# 如果需要在包中包含某些文件，但不想在构建包时包含
# 可以使用 usethis::use_build_ignore()
usethis::use_build_ignore("./.aider.tags/cache.v3") # 忽略构建时的特定文件
usethis::use_build_ignore(
  c(
    ".aider.chat.history.md",
    ".aider.input.history",
    ".lintr"
  )
)


# 添加反向依赖检查，确保更新不会破坏依赖您的包的其他包
# 可以使用 devtools 提供的工具进行检查

# 定期更新您的包依赖
# 检查是否有依赖包的新版本，并确定是否需要更新
