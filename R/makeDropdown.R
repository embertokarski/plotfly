#' create dropdown lists for changing aspects of a plot_ly plot
#' 
#' @param cols        the names of the grouping columns in the data
#' @param shared      an R data.frame or a crosstalk::SharedData object 
#'
#' @return            a dropdown menu represented as a list of lists
#' 
#' @seealso           plotdfly
#'
#' @import            crosstalk
#' @import            plotly
#'
#' @export
#'
makeDropdown <- function(cols, shared) {

  names(cols) <- cols
  ncols <- length(cols)
  if (!is(shared, "SharedData")) shared <- SharedData$new(shared)
  stopifnot(all(cols %in% names(shared$data())))
  message(ncols, " candidate grouping", ifelse(ncols > 1, "s", ""))

  # keep track of visibility for each associated trace
  vis_levels <- list()
  #improve loop
  for (column in cols){
    vis_levels <- append(vis_levels, nlevels(factor(shared$data()[[column]])))
  }
  
  names(vis_levels) <- cols
  vis <- as.list(rep(FALSE, Reduce('+', vis_levels)))
  ###########
  
  vis_start <- 1
  # create & populate menu
  dropdown_list <- list()
  for (column in cols) { 
    
    # visibility
    vis[] <- FALSE
    group <- as.numeric(vis_levels[column]) 
    vis[seq(vis_start, vis_start + (group) - 1)] <- TRUE
    vis_start <- vis_start + group
    
    # add this new entry to dropdown_list 
    dropdown_list <- append(dropdown_list, 
                            list(list(method = "restyle",
                                      args = list("visible", vis),
                                      label = column)))
  } 
  return(dropdown_list)

}
