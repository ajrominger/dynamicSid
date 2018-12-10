#' @title Expand an index
#'  
#' @description A function for indexing subfigures
#' 
#' @details Adds the appropriate number of 00 before an integer to properly sort them 
#' when converted to a string
#' 
#' @param i first index
#' @param j second index
#' @param imax the maximum value i can attain
#' @param jmax the maximum value j can attain
#' 
#' @export

indexExpand <- function(i, j, imax, jmax) {
    paste(paste0(c(rep(0, nchar(as.character(imax)) -
                           nchar(as.character(i))), i),
                 collapse = ''), 
          paste0(c(rep(0, nchar(as.character(jmax)) -
                           nchar(as.character(j))), j),
                 collapse = ''), 
          sep = '-')
}
