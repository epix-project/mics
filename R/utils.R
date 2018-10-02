get_code <- function(x) {
  x %>%
    sub("^.*Filename=..", "", .) %>%
    sub("\\.ZIP.*$"   , "", .)
}


split_code <- function(x) {
  require(magrittr) # %>%
  sapply(seq(1, max(nchar(x)), 2), function(i) substr(x, i, i + 1)) %>%
    as.data.frame() %>%
    setNames(c("file_type", "version_number", "file_format"))
}
