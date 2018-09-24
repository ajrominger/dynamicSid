# function for indexing subfigures
indexExpand <- function(i, j, imax, jmax) {
    paste(paste0(c(rep(0, nchar(as.character(imax)) -
                           nchar(as.character(i))), i),
                 collapse = ''), 
          paste0(c(rep(0, nchar(as.character(jmax)) -
                           nchar(as.character(j))), j),
                 collapse = ''), 
          sep = '-')
}
