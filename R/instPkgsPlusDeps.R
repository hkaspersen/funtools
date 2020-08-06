#' Install package and its dependencies
#'
#' This function is taken from the Stack Overflow issue https://stackoverflow.com/questions/21010705/update-a-specific-r-package-and-its-dependencies.
#'
#' @param pkg The package name
#' @param install If FALSE (default), the function returns a list of the dependencies. If TRUE, the function installs these dependencies
#' @param which The type of dependency, see \code{\link[tools]{package_dependencies}}.
#' @param inc.pkg If TRUE, installs the package in question in addition to the dependencies. If FALSE, only the dependencies are installed.
#'
#' @export
instPkgPlusDeps <- function(pkg, install = FALSE,
                            which = c("Depends", "Imports", "LinkingTo"),
                            inc.pkg = TRUE) {
  stopifnot(require("tools")) ## load tools
  ap <- available.packages() ## takes a minute on first use
  ## get dependencies for pkg recursively through all dependencies
  deps <- package_dependencies(pkg, db = ap, which = which, recursive = TRUE)
  ## the next line can generate warnings; I think these are harmless
  ## returns the Priority field. `NA` indicates not Base or Recommended
  pri <- sapply(deps[[1]], packageDescription, fields = "Priority")
  ## filter out Base & Recommended pkgs - we want the `NA` entries
  deps <- deps[[1]][is.na(pri)]
  ## install pkg too?
  if (inc.pkg) {
    deps = c(pkg, deps)
  }
  ## are we installing?
  if (install) {
    install.packages(deps)
  }
  deps ## return dependencies
}
