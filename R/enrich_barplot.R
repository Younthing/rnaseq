#' @title Enrichment Analysis Bar Plot
#' @description This function creates a bar plot for gene enrichment analysis results, displaying the counts or gene ratios of enriched terms across different ontology categories. The plot is faceted by ontology categories, and the number of items displayed per facet can be limited using the `topn` parameter.
#'
#' @param data A data frame containing the enrichment analysis results. It must include the following columns:
#'   \describe{
#'     \item{Description}{Character vector, descriptions of the enriched terms.}
#'     \item{GeneRatio}{Numeric vector, gene ratios (optional if \code{use_gene_ratio} is \code{FALSE}).}
#'     \item{Count}{Numeric vector, counts of genes in each term (optional if \code{use_gene_ratio} is \code{TRUE}).}
#'     \item{p.adjust}{Numeric vector, the adjusted p-values for multiple testing.}
#'     \item{ONTOLOGY}{Character vector, ontology categories, such as "Biological Process", "Molecular Function", or "Cellular Component".}
#'   }
#' @param output_file A string specifying the file path to save the plot. If provided, the plot will be saved to this file. Default is \code{NULL}.
#' @param width Numeric, the width of the saved plot in inches. Default is 6.
#' @param height Numeric, the height of the saved plot in inches. Default is 8.
#' @param dpi Numeric, the resolution of the saved plot in dots per inch. Default is 300.
#' @param topn Integer, the maximum number of items to display per ontology category. If an ontology category has more than \code{topn} items, only the items with the lowest \code{p.adjust} values will be displayed. Default is 10.
#' @param use_gene_ratio Logical, whether to use \code{GeneRatio} for the x-axis. If \code{FALSE}, \code{Count} will be used. Default is \code{FALSE}.
#'
#' @return If \code{output_file} is provided, the function saves the plot to the specified path and returns \code{NULL}. Otherwise, it returns the ggplot object.
#'
#' @usage
#' enrichment_barplot(
#'   data,
#'   output_file = NULL,
#'   width = 5,
#'   height = 4,
#'   dpi = 300,
#'   topn = 10,
#'   use_gene_ratio = FALSE
#' )
#'
#' @examples
#' # Create example data frame with necessary columns
#' df <- data.frame(
#'   Description = paste0("Term", 1:15),
#'   GeneRatio = runif(15, 0.1, 0.3),
#'   Count = sample(10:50, 15, replace = TRUE),
#'   p.adjust = 10^(-runif(15, 1, 5)),
#'   ONTOLOGY = rep(c("Biological Process", "Molecular Function", "Cellular Component"), each = 5)
#' )
#' # Generate bar plot without saving to file
#' p <- enrichment_barplot(df, topn = 3)
#' # Save plot to specified path
#' enrichment_barplot(df, output_file = "enrichment_barplot.pdf", topn = 5)
#'
#' @details
#' The function uses \code{ggplot2} to create a faceted bar plot of enrichment results. By default, it plots the \code{Count} of genes in each enriched term on the x-axis. Setting \code{use_gene_ratio = TRUE} will plot the \code{GeneRatio} instead. The bars are ordered within each facet based on the \code{p.adjust} values.
#'
#' The \code{topn} parameter allows you to limit the number of terms displayed per ontology category to the most significant ones based on the adjusted p-values.
#'
#' When \code{output_file} is provided, the plot is saved to the specified file, and the function returns \code{NULL}. Otherwise, the ggplot object is returned for further customization or immediate display.
#'
#' @keywords enrichment plot gene ontology barplot
#' @import ggplot2 forcats dplyr
#' @export
enrichment_barplot <- function(
    data,
    output_file = NULL,
    width = 6,
    height = 8,
    dpi = 300,
    topn = 10,
    use_gene_ratio = FALSE) {
  # Load required package
  require(ggplot2)
  require(dplyr)
  require(forcats)
  Description <- Percentage <- Count <- ONTOLOGY <- GeneRatio <- p.adjust <- pvalue <- NULL # nolintr, to satisfy codetools
  # Ensure necessary columns are present
  required_cols <- c("Description", "p.adjust", "ONTOLOGY")
  if (use_gene_ratio) {
    required_cols <- c(required_cols, "GeneRatio")
  } else {
    required_cols <- c(required_cols, "Count")
  }
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from the data: ", paste(missing_cols, collapse = ", "))
  }

  # Limit the number of items per ONTOLOGY category to topn
  data <- data %>%
    dplyr::group_by(ONTOLOGY) %>%
    dplyr::arrange(p.adjust, .by_group = TRUE) %>%
    dplyr::slice_head(n = topn) %>%
    dplyr::ungroup()

  # Reorder Description factor levels based on p.adjust within each ONTOLOGY
  data <- data %>%
    dplyr::mutate(
      Description = factor(Description),
      Description = forcats::fct_reorder(Description, p.adjust)
    )

  # Create the ggplot object
  p <- ggplot(data, aes(
    x = if (use_gene_ratio) GeneRatio else Count,
    y = Description,
    fill = -log10(p.adjust)
  )) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(
      low = "blue", high = "red",
      name = "-log10(P adjust)"
    ) +
    labs(
      x = if (use_gene_ratio) "Gene Ratio" else "Count",
      y = NULL,
      title = "Enrichment Bar Plot"
    ) +
    facet_grid(
      ONTOLOGY ~ .,
      scales = "free_y",
      space = "free_y"
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.margin = margin(b = 0.05, t = 0.1, l = 0.25, r = 0.25, unit = "cm"),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
      axis.text.y = element_text(size = 8, colour = "black", hjust = 1, vjust = 0.5),
      axis.text.x = element_text(size = 8, colour = "black", angle = 0, hjust = 0.5, vjust = 0.5),
      axis.line = element_line(size = 0.5, colour = "black"),
      axis.ticks = element_line(size = 0.5, colour = "black"),
      strip.background = element_rect(fill = "grey85", color = "black"),
      strip.text = element_text(size = 10, face = "bold")
    )

  # If output_file is provided, save the plot
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = width, height = height, dpi = dpi)
    message(paste("The plot has been saved to:", output_file))
    return(NULL)
  }

  # Return the ggplot object
  return(p)
}
