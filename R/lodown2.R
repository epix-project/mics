#'
#'
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
}
