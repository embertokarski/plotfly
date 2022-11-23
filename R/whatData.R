#' determine what kind of object grabData should expect to process and how
#' 
#' Currently we try data.frame, SingleCellExperiment, and Seurat, in that order
#' 
#' @param x         an object (a data.frame, matrix, SCE, or Seurat) 
#' @param use       name of dimred to use, if x is not a df or matrix ("UMAP")
#' @param color_by  column name of metadata (or x itself) to color by ("study")
#' @param dims      what dimred columns (or plain old columns) in x to use (1:3)
#'
#' @return          a string with the (sanity checked) data type, or an error
#'
#' @export
whatData <- function(x, use="UMAP", color_by="study", dims=1:3) {

  params <- list() 
  columns <- color_by

  if (is(x, "data.frame")) { 

    # we ignore `use` for this
    params$what <- "data.frame"
    eligible <- setdiff(colnames(x), colnames(x)[dims])
    stopifnot(all(columns %in% eligible))
    params$rd <- as.data.frame(x[, dims])
    params$cd <- as.data.frame(x[, columns])
    colnames(params$cd) <- columns

  } else if (is(x, "Seurat")) {

    params$what <- "Seurat"
    stopifnot(use %in% names(x@reductions))
    stopifnot(ncol(x@reductions[[use]]@cell.embeddings) >= length(dims))
    params$rd <- as.data.frame(x@reductions[[use]]@cell.embeddings[, dims])
    stopifnot(all(columns %in% colnames(x@meta.data)))
    params$cd <- as.data.frame(x@meta.data[, columns])
    colnames(params$cd) <- columns
    

  } else if (is(x, "SingleCellExperiment")) {
 
    params$what <- "SingleCellExperiment"
    stopifnot(use %in% colnames(x@int_colData$reducedDims))
    stopifnot(ncol(x@int_colData$reducedDims[[use]]) >= length(dims))
    params$rd <- as.data.frame(x@int_colData$reducedDims[["use"]][, dims])
    stopifnot(all(columns %in% colnames(x@colData)))
    params$cd <- as.data.frame(x@colData[, columns])
    colnames(params$cd) <- columns

  } else if (is(x, "nmf")) {
    
    params$what <- "nmf"
    stopifnot(ncol(x@h) >= length(dims))
    params$rd <- as.data.frame(t(x@h[dims, ]))
    if(!"colData" %in% names(x@misc)) stop("nmf objects need @misc$colData")
    stopifnot(all(columns %in% colnames(x@misc$colData)))
    params$cd <- as.data.frame(x@misc$colData[, columns])
    colnames(params$cd) <- columns

  } else { 

    warning("Can't figure out what `x` is... !")
    params$what <- "unknown"

  }
  
  return(params) 

}
