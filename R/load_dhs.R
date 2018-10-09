#' Load data from DHS
#'
#' \code{load_dhs} (i) makes the catalog corresponding to the inputed profile's
#' project, (ii) optionally filters it according to conditions on countries,
#' years, and type of data, (iii) download the data file from the DHS server to
#' disk and (iv) load these data files to the global environment.
#'
#' Of note, this function is supposed to be efficient in doing what it does. In
#' particular, both the creation of the catalog and the downloading of data from
#' the DHS server can be time consuming in that they query the DHS server.
#' Furthermore, the loading of the data from disk to the global environment may
#' also take time depending on the size and number of data files to load. Thus,
#' each of these actions are preceded by checking whether they have previously
#' done. If yes, they are not done again. If no, it also makes sure that it won't
#' have to be done for a second call of the function. For example, if a
#' catalog corresponding to the inputed profile's project already exist in the
#' global environment, it won't be created again and this already existing
#' catalog will be used instead. If such a catalog does not exist, then it will
#' be created and used inside the function but also it will be saved in the
#' global environment for potential future calls of the function on the same
#' profile.
#'
#' @param profile named character vector with fields \code{data_name},
#'                \code{your_email}, \code{your_password} and \code{your_project}.
#' @param ... logical dplyr::filter predicates defined in terms of the
#'            variables in the catalog.
#' @param output name of the directory in which the files downloaded from the
#'               DHS server should be saved.
#'
#' @return This function does not return any thing and is used for its side
#' effects, which are (i) creating the catalog corresponding to the inputed
#' profile's project and saving it into the global environment, (ii) downloading
#' the data to disk from the DHS server, and (iii) loading the downloaded data
#' from disk to the global environment.
#'
#' @examples
#' ## Run the following call twice and see the differences in execution times:
#' \dontrun{
#' system.time(load_dhs(my_profile, year == 2015, country == "Tanzania", file_type == "KR"))
#' system.time(load_dhs(my_profile, year == 2015, country == "Tanzania", file_type == "KR"))
#' }
#'
#' ## Try deleting the data object and rerun:
#' \dontrun{
#' rm("TZKR7AFL")
#' system.time(load_dhs(my_profile, year == 2015, country == "Tanzania", file_type == "KR"))
#' }
#'
#' ## Try deleting the data file and rerun:
#' \dontrun{
#' file.remove("TZKR7AFL.rds")
#' system.time(load_dhs(my_profile, year == 2015, country == "Tanzania", file_type == "KR"))
#' }
#'
#' ## Try deleting the catalog and rerun:
#' \dontrun{
#' rm("my_profile_catalog")
#' system.time(load_dhs(my_profile, year == 2015, country == "Tanzania", file_type == "KR"))
#' }
#'
#' @importFrom dplyr filter mutate %>%
#' @importFrom dplyrx mutate2
#'
#' @export
#'
load_dhs <- function(profile, ..., output = getwd()) {
  catalog <- get_catalog2(profile)
#  if (is.character(catalog)) {
#    if (length(catalog) > 1) stop("There are already more than one catalog in the global environment that corresponds to this profile's project.")
#    cat(paste("There is already the following catalog in the global environment that corresponds to this profile's project:", catalog,
#                  "\nWe will use this catalog to download the data if there are not already downloaded."))
#    catalog <- get(catalog, .GlobalEnv)
#  } else {
    catalog_name <- paste0(as.list(match.call())$profile, "_catalog")
    assign(catalog_name, catalog, envir = .GlobalEnv)
    cat(paste0("The catalog has been save in the global environment under the name of '", catalog_name, "'.\n"))
#  }
  catalog %>%
    filter(...) %>%
#    filter(year == 2015, country == "Tanzania", file_type == "KR") %>%
    mutate2(output_folder = output) %>%
#    with_profile(profile, lodown2)(catalog = .)
    lodown2()

  data_files <- grep("FL.rds", dir(output), value = TRUE)
  data_objects <- sub(".rds", "", data_files)
  sel <- ! data_objects %in% ls(.GlobalEnv)
  if (any(sel))
    for(i in 1:sum(sel))
      assign(data_objects[sel][i], readRDS(paste0(output, "/", data_files[sel][i])), envir = .GlobalEnv)
  else cat(paste("Data are already available in the global environment:\n", paste(data_objects[! sel], sep = ", ")))
}


