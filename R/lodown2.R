#' Locally Download Publicly-available data
#'
#' \code{lodown2} is a wrapper around the \code{\link[lodown]{lodown}} function
#' that checks the existance of files on disk before trying to download them. It
#' will thus download only the files that are not already on disk.
#'
#' See examples in the \code{\link{with_profile}} documentation.
#'
#' @param catalog \code{data.frame} detailing available microdata extracts and
#' metadata, see the documentation of \code{\link[lodown]{lodown}}.
#' @param ... additional arguments passed to \code{\link[lodown]{lodown}}.
#'
#' @importFrom lodown lodown
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter
#'
#' @export
#'
lodown2 <- function(catalog = NULL, ...) {
  stopifnot(! is.null(catalog))
  catalog %<>% filter(! file.exists(paste0(output_folder, "/", unique(sub("..$", "FL", file_name)), ".rds")))
  if (nrow(catalog) > 0) lodown(catalog = catalog, ...)
  else cat("The data already exist on disk.")
}
