#'
#'
#'
load_dhs <- function(profile, ..., output = getwd()) {
  catalog <- get_catalog2(profile)
  if (is.character(catalog)) {
    if (length(catalog) > 1) stop("There are already more than one catalog in the global environment that corresponds to this profile's project.")
    message(paste("There is already the following catalog in the global environment that corresponds to this profile's project:", catalog,
                  "\nWe will use this catalog to download the data if there are not already downloaded."))
    catalog <- get(catalog, .GlobalEnv)
  } else {
    catalog_name <- paste0(as.list(match.call())$profile, "_catalog")
    assign(catalog_name, catalog, envir = .GlobalEnv)
    message(paste0("The catalog has been save in the global environment under the name of '", catalog_name, "'."))
  }
  catalog %>%
    filter(...) %>%
    mutate(output_folder = output) %>%
    with_profile(profile, lodown2)(catalog = .)

  data_files <- grep("FL.rds", dir(output), value = TRUE)
  data_objects <- sub(".rds", "", data_files)
  sel <- ! data_objects %in% ls(.GlobalEnv)
  if (any(sel))
    for(i in 1:sum(sel))
      assign(data_objects[sel][i], readRDS(data_files[sel][i]), envir = .GlobalEnv)
  else message(paste("Data are already available in the global environment:\n", paste(data_objects[! sel], sep = ", ")))
}


