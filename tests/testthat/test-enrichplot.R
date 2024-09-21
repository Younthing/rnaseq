library(testthat)
library(ggplot2)
library(dplyr)
library(forcats)
test_that("enrichment_barplot works correctly", {
    # Create example data frame with necessary columns
    df <- data.frame(
        Description = paste0("Term", 1:15),
        GeneRatio = runif(15, 0.1, 0.3),
        Count = sample(10:50, 15, replace = TRUE),
        p.adjust = 10^(-runif(15, 1, 5)),
        ONTOLOGY = rep(c("Biological Process", "Molecular Function", "Cellular Component"), each = 5)
    )

    # Test that the function returns a ggplot object when output_file is NULL
    p <- enrichment_barplot(df, topn = 3)
    expect_s3_class(p, "ggplot")

    # Test that the function saves a file when output_file is provided
    output_file <- tempfile(fileext = ".png")
    enrichment_barplot(df, output_file = output_file, topn = 3)
    expect_true(file.exists(output_file))

    # Test that the function handles missing required columns
    df_missing <- df[, -which(names(df) == "Count")]
    expect_error(enrichment_barplot(df_missing, topn = 3), "The following required columns are missing from the data: Count")

    # Test that the function limits the number of items per ontology category to topn
    p_limited <- enrichment_barplot(df, topn = 2)
    data_limited <- ggplot_build(p_limited)$data[[1]]
    expect_true(all(table(data_limited$PANEL) <= 2))

    # Test that the function uses GeneRatio when use_gene_ratio is TRUE
    p_gene_ratio <- enrichment_barplot(df, topn = 3, use_gene_ratio = TRUE)
    data_gene_ratio <- ggplot_build(p_gene_ratio)$data[[1]]
    expect_true("x" %in% names(data_gene_ratio))

    # Clean up
    unlink(output_file)
})


test_that("enrichment_dotplot works correctly", {
    # Create example data frame with necessary columns
    df_dotplot <- data.frame(
        Description = paste0("Term", 1:15),
        GeneRatio = runif(15, 0.1, 0.3),
        Count = sample(10:50, 15, replace = TRUE),
        p.adjust = 10^(-runif(15, 1, 5)),
        ONTOLOGY = rep(c("Biological Process", "Molecular Function", "Cellular Component"), each = 5)
    )

    # Test that the function returns a ggplot object when output_file is NULL
    p <- enrichment_dotplot(df_dotplot, topn = 3)
    expect_s3_class(p, "ggplot")

    # Test that the function saves a file when output_file is provided
    output_file <- tempfile(fileext = ".png")
    enrichment_dotplot(df_dotplot, output_file = output_file, topn = 3)
    expect_true(file.exists(output_file))

    # Test that the function handles missing required columns
    df_missing <- df_dotplot[, -which(names(df_dotplot) == "Count")]
    expect_error(enrichment_dotplot(df_missing, topn = 3), "The following required columns are missing from the data: Count")

    # Test that the function limits the number of items per ontology category to topn
    p_limited <- enrichment_dotplot(df_dotplot, topn = 2)
    data_limited <- ggplot_build(p_limited)$data[[1]]
    expect_true(all(table(data_limited$PANEL) <= 2))

    # Test that the function uses GeneRatio when use_gene_ratio is TRUE
    p_gene_ratio <- enrichment_dotplot(df_dotplot, topn = 3, use_gene_ratio = TRUE)
    data_gene_ratio <- ggplot_build(p_gene_ratio)$data[[1]]
    expect_true("x" %in% names(data_gene_ratio))

    # Clean up
    unlink(output_file)
})
