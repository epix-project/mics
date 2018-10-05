
oucru_profile <- c(data     = "dhs",
                   email    = "slewycka@gmail.com",
                   password = "oucru-hn",
                   project  = "Global antibiotic consumption study")

# step 1: making the profile:
gerald_profile <- c(data     = "dhs",
                    email    = "gerald.makuka@stx.ox.ac.uk",
                    password = "gerald22",
                    project  = "Antibiotics usage amongst under fiv")

# step 2: using the profile to make the data catalogue:
catalogue_gerald <- with_profile(gerald_profile, get_catalog)()

# step 3: using the data catalogue and the profile to download specific data:
catalogue_gerald %>%
  expand_catalogue() %>%
  filter(year == 2015, country == "Tanzania", file_type == "KR") %>%
  mutate(output_folder = "dhs_data") %>% # defining the output folder
  with_profile(gerald_profile, lodown2)(catalog = .)

# step 4: loading the data files:
