#' @title Animate
#'  
#' @description Animate a series of plots (depricated)
#' 
#' @details This function is in the process of being remade with `saveVideo`
#' 
#' @param dir the directory to save the animation
#' @param gifName name for the saved gif
#' @param cleanUp whether to remove temporary files
#' 
#' @export

animate <- function(dir, gifName, cleanUp = FALSE) {
    # make gif using ImageMagick and clean up temp files
    system(sprintf('convert %s/temp/*.png -delay 3 -loop 0 %s/%s.gif', 
                   dir, dir, gifName))
    if(cleanUp) system(sprintf('rm -r -f %s/temp', dir))
}
