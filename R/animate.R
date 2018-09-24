animate <- function(dir, gifName, cleanUp = FALSE) {
    # make gif using ImageMagick and clean up temp files
    system(sprintf('convert %s/temp/*.png -delay 3 -loop 0 %s/%s.gif', 
                   dir, dir, gifName))
    if(cleanUp) system(sprintf('rm -r -f %s/temp', dir))
}
