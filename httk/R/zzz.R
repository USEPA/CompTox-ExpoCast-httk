# R CMD CHECK throws notes about "no visible binding for global variable", for
# each time a data.table column name is used without quotes. This quiets
# concerns of R CMD check re: the .'s that appear in pipelines

if(getRversion() >= "2.15.1") 
{
  utils::globalVariables(c(".","chem.physical_and_invitro.data","well_param"))
}

# Display the version when the package is loaded:
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

# Remove the memory object (DLL in windows) storing the models when the package 
# is unloaded:
.onUnload <- function(libpath) {
  library.dynam.unload("httk", libpath)
}