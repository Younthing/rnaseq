#' @title Example Gene Expression Counts Data
#' @description 这是一个示例基因表达计数矩阵，包含去重后的数据。
#' @format 一个数据框，行名为基因，列名为样本。
#' @source 来自GSE169304的公开数据集。
#' @examples
#' \dontrun{
#' # 加载示例数据
#' expr_counts <- read.csv(system.file("extdata", "数据去重-counts.csv", package = "rnaseq"), row.names = 1)
#' }
"expr_counts"

#' @title Example Group Table Data
#' @description 这是一个示例分组表，包含样本分组信息。
#' @format 一个数据框，包含样本的分组信息。
#' @source 来自GSE169304的公开数据集。
#' @examples
#' \dontrun{
#' # 加载示例数据
#' group_table <- read.csv(system.file("extdata", "group_169304.csv", package = "rnaseq"))
#' }
"group_table"


#' @title Example Gene Expression FPKM Data
#' @description 这是一个示例基因表达FPKM矩阵，包含去重后的数据。
#' @format 一个数据框，行名为基因，列名为样本。
#' @source 来自GSE169304的公开数据集。
#' @examples
#' \dontrun{
#' # 加载示例数据
#' expr_counts <- read.csv(system.file("extdata", "数据去重-fpkm.csv", package = "rnaseq"), row.names = 1)
#' }
"expr_fpkm"

#' @title Example Gene Expression FPKM Data
#' @description 这是一个示例基因表达tpm矩阵，包含去重后的数据。
#' @format 一个数据框，行名为基因，列名为样本。
#' @source 来自GSE169304的公开数据集。
#' @examples
#' \dontrun{
#' # 加载示例数据
#' expr_counts <- read.csv(system.file("extdata", "数据去重-fpkm.csv", package = "rnaseq"), row.names = 1)
#' }
"expr_tpm"

#' Gene Ontology (GO) 数据
#'
#' 包含Gene Ontology的生物学过程(BP)、细胞组分(CC)和分子功能(MF)数据。
#'
#' @format 一个列表，其中每个元素都是一个字符向量：
#' \describe{
#'   \item{名称}{GO术语名称}
#'   \item{内容}{与该GO术语相关的基因名称向量}
#' }
#' @source Gene Ontology Consortium
"gene_ontology_sets"

#' KEGG通路数据
#'
#' 包含KEGG（Kyoto Encyclopedia of Genes and Genomes）通路信息的数据集。
#'
#' @format 一个列表，其中每个元素都是一个字符向量：
#' \describe{
#'   \item{名称}{KEGG通路名称}
#'   \item{内容}{与该通路相关的基因名称向量}
#' }
#' @source KEGG数据库
"kegg_pathway_sets"

#' Hallmark基因集
#'
#' 包含MSigDB的Hallmark基因集数据。
#'
#' @format 一个列表，其中每个元素都是一个字符向量：
#' \describe{
#'   \item{名称}{Hallmark基因集名称}
#'   \item{内容}{属于该基因集的基因名称向量}
#' }
#' @source Molecular Signatures Database (MSigDB)
"msigdb_hallmark_sets"

#' 肿瘤微环境(TME)相关表型
#'
#' 包含与肿瘤微环境相关的基因表型数据。
#'
#' @format 一个列表，其中每个元素都是一个字符向量：
#' \describe{
#'   \item{名称}{TME相关表型名称}
#'   \item{内容}{构成该表型的基因名称向量}
#' }
#' @source 各种生物医学研究文献
"tme_signature_sets"

#' 代谢相关表型
#'
#' 包含与细胞代谢相关的基因表型数据。
#'
#' @format 一个列表，其中每个元素都是一个字符向量：
#' \describe{
#'   \item{名称}{代谢相关表型名称}
#'   \item{内容}{构成该表型的基因名称向量}
#' }
#' @source 各种生物医学研究文献
"metabolism_signature_sets"

#' 表型集合
#'
#' 包含各种生物学过程和疾病相关的基因表型集合。
#'
#' @format 一个列表，其中每个元素都是一个字符向量：
#' \describe{
#'   \item{名称}{表型集合名称}
#'   \item{内容}{属于该集合的基因名称向量}
#' }
#' @source 各种生物医学研究文献和数据库
"misc_signature_collection"

#' 肿瘤相关表型
#'
#' 包含与各种肿瘤类型和肿瘤生物学过程相关的基因表型。
#'
#' @format 一个列表，其中每个元素都是一个字符向量：
#' \describe{
#'   \item{名称}{肿瘤相关表型名称}
#'   \item{内容}{构成该表型的基因名称向量}
#' }
#' @source 各种肿瘤研究文献和数据库
"tumor_signature_sets"


#' anno_grch38 数据框
#'
#' 这个数据框包含了 GRCh38 基因组注释信息，共有 66533 行和 11 列。
#'
#' @format 一个包含 66533 行和 11 列的数据框，具体变量如下：
#' \describe{
#'   \item{id}{基因的唯一标识符 (字符型)}
#'   \item{eff_length}{基因的有效长度 (数值型)}
#'   \item{gc}{基因的GC含量 (数值型)}
#'   \item{entrez}{基因的Entrez ID (数值型)}
#'   \item{symbol}{基因的符号名称 (字符型)}
#'   \item{chr}{基因所在的染色体 (字符型)}
#'   \item{start}{基因的起始位置 (数值型)}
#'   \item{end}{基因的结束位置 (数值型)}
#'   \item{strand}{基因所在的链 (数值型，1表示正链，-1表示负链)}
#'   \item{biotype}{基因的生物类型 (字符型)}
#'   \item{description}{基因的描述 (字符型)}
#' }
#' @source by function `getGeneLengthAndGCContent` from EDASeq package with default parameters at 2023-02-10
"anno_grch38"
