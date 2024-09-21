#' @title Enrichment Analysis Dot Plot
#' @description This function creates a dot plot for gene enrichment analysis results, displaying the gene ratios or counts of enriched terms across different ontology categories. The plot is faceted by ontology categories, and the number of items displayed per facet can be limited using the `topn` parameter.
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
#' @param low_color A string specifying the color representing low values in the color gradient. Default is "blue".
#' @param high_color A string specifying the color representing high values in the color gradient. Default is "red".
#' @param use_gene_ratio Logical, whether to use \code{GeneRatio} for the x-axis. If \code{FALSE}, \code{Count} will be used. Default is \code{TRUE}.
#'
#' @return If \code{output_file} is provided, the function saves the plot to the specified path and returns \code{NULL}. Otherwise, it returns the ggplot object.
#'
#' @usage
#' enrichment_dotplot(
#'   data,
#'   output_file = NULL,
#'   width = 6,
#'   height = 8,
#'   dpi = 300,
#'   topn = 10,
#'   low_color = "blue",
#'   high_color = "red",
#'   use_gene_ratio = TRUE
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
#' # Generate dot plot without saving to file
#' p <- enrichment_dotplot(df, topn = 3)
#' print(p)
#'
#' @details
#' The function uses \code{ggplot2} to create a faceted dot plot of enrichment results. By default, it plots the \code{GeneRatio} on the x-axis. Setting \code{use_gene_ratio = FALSE} will plot the \code{Count} instead. The points are ordered within each facet based on the \code{p.adjust} values.
#'
#' The \code{topn} parameter allows you to limit the number of terms displayed per ontology category to the most significant ones based on the adjusted p-values.
#'
#' When \code{output_file} is provided, the plot is saved to the specified file, and the function returns \code{NULL}. Otherwise, the ggplot object is returned for further customization or immediate display.
#'
#' @keywords enrichment plot gene ontology dotplot
#' @import ggplot2
#' @import forcats
#' @import dplyr
#' @export
enrichment_dotplot <- function(
    data,
    output_file = NULL,
    width = 6,
    height = 8,
    dpi = 300,
    topn = 10,
    low_color = "blue",
    high_color = "red",
    use_gene_ratio = TRUE) {
  Description <- Percentage <- Count <- ONTOLOGY <- GeneRatio <- p.adjust <- pvalue <- NULL # nolintr, to satisfy codetools

  # Ensure necessary columns are present
  required_cols <- c("Description", "p.adjust", "ONTOLOGY")
  if (use_gene_ratio) {
    required_cols <- c(required_cols, "GeneRatio")
  }
  if (use_gene_ratio) {
    required_cols <- c(required_cols, "Count")
  }
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from the data: ", paste(missing_cols, collapse = ", "))
  }

  # Limit the number of items per ONTOLOGY category to topn
  data <- data %>%
    group_by(ONTOLOGY) %>%
    arrange(p.adjust, .by_group = TRUE) %>%
    slice_head(n = topn) %>%
    ungroup()

  # Reorder Description factor levels based on p.adjust within each ONTOLOGY
  data <- data %>%
    mutate(
      Description = factor(Description),
      Description = forcats::fct_reorder(Description, p.adjust)
    )

  # Truncate long descriptions if necessary
  data <- data %>%
    mutate(
      Description = ifelse(nchar(as.character(Description)) > 30,
        paste0(substr(Description, 1, 27), "..."),
        as.character(Description)
      )
    )

  # Create the ggplot object
  p <- ggplot(data, aes(
    x = if (use_gene_ratio) GeneRatio else Count,
    y = Description,
    size = Count,
    color = -log10(p.adjust)
  )) +
    geom_point() +
    scale_color_gradient(
      low = low_color, high = high_color,
      name = "-log10(P adjust)"
    ) +
    scale_size(
      range = c(3, 8),
      name = "Count"
    ) +
    labs(
      x = if (use_gene_ratio) "Gene Ratio" else "Count",
      y = NULL,
      title = "Enrichment Dot Plot"
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
      panel.border = element_rect(linewidth = 0.5, fill = NA, color = "black"),
      axis.text.y = element_text(size = 8, colour = "black", hjust = 1, vjust = 0.5),
      axis.text.x = element_text(
        size = 8, colour = "black", angle = 0,
        hjust = 0.5, vjust = 0.5
      ),
      axis.line = element_line(linewidth = 0.5, colour = "black"),
      axis.ticks = element_line(linewidth = 0.5, colour = "black"),
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
