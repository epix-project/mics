### Packages

#The packages currently installed on your system:
pkgs <- rownames(installed.packages())

#The packages we need from GitHub and installing those that are not already installed:
github <- c("larmarange/labelled", "ajdamico/lodown",
            paste0("choisy/", c("mclabelled", "mcsurvey")))   ### REDUCE THIS TO ONLY WHAT IS NEEDED TO RUN THIS SCRIPT? JUST LOWDOWN?
sel <- !sub(".*/", "", github) %in% pkgs
if (any(sel)) devtools::install_github(github[sel])

#Loading the packages we need to get the data:
library(lodown)     # get_catalog(), lodown()     ### IS THIS THE ONLY ONE USED?


### Getting the data  - ONLY NEED TO DO THIS ONCE
# Define your DHS’s project information. For the project, enter no more than the first 35 characters of the project name. Also, replace the second and third fields by your login email address and your password:

profile <- function(fct, ...) {
  fct(data_name     = "dhs",
      your_email    = "slewycka@gmail.com", 
      your_password = "********", 
      your_project  = "Global antibiotic consumption study", ...)
}

# Retrieving the catalogue of data sets of the project:

catalogue <- profile(get_catalog)

# Selecting the children under 5 year-old for 2016, and also changing the output directory:

### NEED TO MAKE THIS WORK FOR ALL THE DATASETS, NOT JUST AFGHANISTAN 2015.
### DO WE WANT THIS TO OVERWRITE THOSE ALREADY DOWNLOADED OR JUST TO ADD NEW ONES?

catalogue <- subset(catalogue, year == 2015 & grepl("AFKR", full_url))
catalogue$output_folder <- getwd()

# Downloading the data:

profile(lodown, catalog = catalogue)
