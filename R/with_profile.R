#' Add a Profile to a Function
#'
#' \code{with_profile} adds a profile to a function of the package \code{lodown}
#' that need a profile.
#'
#' DHS requires a registration before being able to download any data. See
#' \href{http://rstudio.com}{here} to get one. During the registration, you will
#' need to specify an email as a login and a password. You will also have to
#' explain what data (country, years, etc...) you need, as well as describe
#' the project you want to carry out and to give a name to that project. In the
#' profile that you will define, you will use "dhs" as the data (specifying that
#' you query the DHS data base), as well as the email, password and project's
#' name that you've defined.
#'
#' @param profile named character vector with fields \code{data_name},
#'                \code{your_email}, \code{your_password} and \code{your_project}.
#' @param fct the function that should profiled.
#' @param ... additional paramters to be passed to the profiled function.
#'
#' @return Returns the function \code{fct} with profile information predefined.
#'
#' @seealso \code{\link{expand_catalog}}, \code{\link[lodown]{get_catalog}},
#'  \code{\link{lodown2}}.
#'
#' @examples
#' ## Here is a typical 4-step pipeline that can be assembled into a function.
#' ## The name of the project here is "Global antibiotic consumption study" and,
#' ## for obvious reasons, the email and password are encrypted.
#' \dontrun{
#' require(lodown) # for get_catalog and
#' require(dplyr)  # for %>%,
#' }
#' ## step 1: defining the profile:
#' \dontrun{
#' my_profile <- c(data     = "dhs",
#'                 email    = "********", # here replace by identifier
#'                 password = "********", # here replace by password
#'                 project  = "Global antibiotic consumption study")
#' }
#'
#' ## step 2: using the profile to make the data catalogue. Depending on the size
#' ## of the project, this step can take time. So you may want to save the
#' ## catalogue in the workspace for future uses.
#' \dontrun{
#' catalogue_gerald <- with_profile(my_profile, get_catalog)()
#' }
#'
#' ## step 3: using the data catalogue and the profile to download specific data:
#' \dontrun{
#' catalogue_gerald %>%
#'   expand_catalogue() %>%
#'   filter(year == 2015, country == "Tanzania", file_type == "KR") %>%
#'   mutate(output_folder = "dhs_data") %>%  # defining the output folder
#'   with_profile(gerald_profile, lodown2)(catalog = .)
#' }
#'
#' ## step 4: loading all the data files to the workspace:
#' \dontrun{
#' for(file in dir("dhs_data")) assign(sub(".rds", "", file), readRDS(file))
#' }
#'
#' @export
#'
with_profile <- function(profile, fct, ...) {
  with(as.list(profile),
       function(...)
         fct(data_name     = data,
             your_email    = email,
             your_password = password,
             your_project  = project, ...)
  )
}


