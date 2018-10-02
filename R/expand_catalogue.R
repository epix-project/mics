#'
#'
#' @export
#'
expand_catalogue <- function(x) {
  bind_cols(x, split_code(get_code(x$full_url)))
}
