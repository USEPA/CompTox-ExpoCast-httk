.onAttach <- function(libname, pkgname) {
    # Runs when attached to search() path such as by library() or require()
    if (interactive()) 
    {
        packageStartupMessage("httk v",
                              as.character(packageVersion("httk")),
                              " successfully loaded. For help type: help(\"httk\")"
                              )
    }
}