# Display the version when the package is loaded:
.onAttach <- function(libname, pkgname) {
    # Runs when attached to search() path such as by library() or require()
    if (interactive()) 
    {
       httkver <- suppressMessages(rvcheck::check_cran("httk"))
       packageStartupMessage(paste0("httkexamples v",
                             as.character(utils::packageVersion("httkexamples")),
                             " successfully loaded.\n",
                             ifelse(httkver$up_to_date %in% c(FALSE,NA),
                               paste0("You have \"httk\" version ",
                               httkver$installed_version,
                               " installed -- the most recent version on CRAN is ",
                               httkver$latest_version,
                               ".\n"),
                               "Your version of \"httk\" is up to date.\n"
                               )
                             ))
                              
    }
}

