# Sourced by shiny before ui.R/server.R. Installs the app's missing
# dependencies. Deliberately lives in the app directory rather than R/
# (decision 2026-08-17): runtime app behavior, kept out of the package
# code that CRAN checks scan. Non-interactive sessions auto-install,
# announced via message(); interactive sessions are prompted.
check_packages <- function(pkgs){
  installed_pkgs <- rownames(utils::installed.packages())
  miss <- pkgs[!(pkgs %in% installed_pkgs)]
  if(length(miss) == 0)
    return(invisible(FALSE))
  if(interactive()){
    cat("The following required packages are not installed: ",
        paste(miss, collapse = ", "), "\n")
    install <- readline("Install now (y/n)?")
    if(tolower(install) != "y")
      stop("Missing required packages: ", paste(miss, collapse = ", "))
  }else{
    message("Installing required packages: ", paste(miss, collapse = ", "))
  }
  utils::install.packages(miss)
  still_missing <- miss[!(miss %in% rownames(utils::installed.packages()))]
  if(length(still_missing) > 0)
    stop("Failed to install: ", paste(still_missing, collapse = ", "))
  invisible(TRUE)
}

check_packages(
  c("shinyWidgets", "shinyhelper", "promises", "future", "ipc", "ggplot2",
    "DT")
)
