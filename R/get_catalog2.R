#' Download the Catalog of a Profile's Project
#'
#' \code{get_catalog2} is a wrapper around the \code{\link[lodown]{get_catalog}}
#' function. Before downloading the catalog corresponding to the project of the
#' specified profile, the function checks in the global environment whether there
#' is already a catalog corresponding to the same project. If so, the function
#' stops returning the names of the catalogs of the global environment that
#' correspond to this project. If not, the function proceed to download the
#' catalog and returns it with an "project" attribute that contains the name of
#' the project of the inputed profile. This avoid downloading the same catalog
#' twice.
#'
#' @param profile named character vector with fields \code{data_name},
#' \code{your_email}, \code{your_password} and \code{your_project}.
#'
#' @importFrom lodown get_catalog
#'
#' @export
#'
get_catalog2 <- function(profile) {
  globenv <- as.list(.GlobalEnv)
  if (length(globenv) > 0) {
    with_project <- globenv[sapply(globenv, function(x) "project" %in% names(attributes(x)))]
    if (length(with_project) > 0) {
      projects <- lapply(with_project, attr, "project")
      target_project <- projects[projects == profile["project"]]
      nb <- length(target_project)
      if (nb > 1)  stop(paste("In the global environment there are already more than 1 catalog corresponding to that profile:",
                              paste(names(target_project), collapse = ", ")))
      if (nb == 1) stop(paste("In the global environment there is already the following catalog corresponding to that profile:",
                              names(target_project)))
    }
  }
  with_profile(profile, get_catalog)() %>%
  `attr<-`("project", unname(profile["project"]))
}
