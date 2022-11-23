#' create a 3D plotly plot from multiple data sources
#'
#' @param data some form of data object as handled by grabData() 
#' (a data.frame, RcppML::nmf, SCE, or Seurat)
#' @param use dimred name to use if needed ("umap")
#' @param dims_by dim columns to use. Can be numbered or named columns (1:3)
#' @param color_by metadata to color by ("group")
#' @param ... 
#'
#' @return a plot_ly object
#' @export
#'
#' @examples
staticplotfly <- function(data, use = "umap", dims_by = 1:3,
                          color_by = "group", ...) {
  
  df <- grabData(data, use = use, color_by = color_by, dims = dims_by)
  plot <- plotdfly(df, dimcols = dims_by, groupcols = color_by)
}
