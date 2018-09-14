###Packages

#The packages currently installed on your system:
pkgs <- rownames(installed.packages())

#The packages we need from CRAN and installing those that are not already installed:
cran <- c("survey", "devtools", "dplyr", "sp", "raster", "countrycode", "stringr")
sel <- !cran %in% pkgs
if (any(sel)) install.packages(cran[sel])

#The packages we need from GitHub and installing those that are not already installed:
github <- c("larmarange/labelled", "ajdamico/lodown",
          paste0("choisy/", c("mclabelled", "mcsurvey")))
sel <- !sub(".*/", "", github) %in% pkgs
if (any(sel)) devtools::install_github(github[sel])

#Loading the packages we need for the analysis:
    
library(magrittr)   # %>%, %<>% 
library(labelled)   # to_factor(), remove_attributes()
library(lodown)     # get_catalog(), lodown()  ###DO WE NEED THIS ONE HERE???
library(survey)     # svydesign(), svyby(), svymean(), svyglm()
library(dplyr)      # filter(), select(), mutate(), mutate_if(), recode()
library(sp)         # plot(), spplot()
library(haven)      # read_dta()
library(tibble)     #
library(purrr)      #

### Read in file - need to make this generic to read in all of the files created in the "recode" script
### This should now read in the dataframes created in the "recode" step
AFG2015 <- read_dta("AFG2015.dta")

AFG2015 %<>% 
  mutate(#nbea      = 106642, # total number of enumeration areas (EA) in the country - IS THIS NECESSARY? WHAT IF WE DON'T KNOW IT???
         count     = 1      # counting variable
  )

### Set survey design       ### something wrong here...
svyset <- svydesign(ids    = ~ v021,        # the PSU clusters
                   strata  = ~ v022,        # the strata
                 # fpc     = ~ nbea,        # finite population correction (see below) - I don't think we need this...
                   weights = ~ v005,      # sampling weights
                   data    = AFG2015)

### Summarize data by population strata and create new data frame

fct1 <- function(x, y) {
  svyby(formula(paste("~", x)), formula(paste("~", y)), subset(svyset, c_age1yr, alive), svymean, na.rm = T)
}

fct2 <- function(df3c) {
  df3c[, 2] <- tibble::tibble(map2(df3c[[2]], df3c[[3]], c))
  df3c[, -3]
}

fct3 <- function(x) {
  x <- lapply(x, fct2)
  Reduce(function(...) left_join(by = names(x[[1]])[1], ...), x)
}

fct4 <- function(x) setNames(list(setNames(x, c("value", names(x)[-1]))), names(x)[1])

fct5 <- function(design, xs, ys) {
  xy <- expand.grid(xs, ys)
  out <- map2(xy$Var1, xy$Var2, fct1)
  lapply(as.list(as.data.frame(matrix(1:nrow(xy), length(xs)))), function(x) out[x])
}

# pipeline, version 1:
# the variables:
xs <- c("ttv2", "mcv")
# the strata:
ys <- c("res", "education")
xy <- expand.grid(xs, ys)
out <- map2(xy$Var1, xy$Var2, fct1)
out <- lapply(as.list(as.data.frame(matrix(1:nrow(xy), length(xs)))), function(x) out[x])
out <- lapply(out, fct3)
out <- lapply(out, fct4)
bind_rows(reduce(out, c), .id = "key")

# pipeline, version 2:
by_strata_afg <- svyset %>% 
  fct5(c("ttv2", "bcg", "dpt1", "dpt2", "dpt3", "polio0", "polio1", "polio2", "polio3", "mcv", "diarrhea",
         "ari", "fever", "amu_fever", "amu_diarr"), 
       c("res", "education")) %>% 
  lapply(fct3) %>% 
  lapply(fct4) %>% 
  reduce(c) %>% 
  bind_rows(.id = "key")


### it ends here



### We also need a row for the total country-level coverage/percentage for each variable
### How do we handle variables with more than 2 levels e.g. vaccination (vacclevel) coded as "no vaccination",
### "incomplete vaccination", and "complete vaccination"? (e.g. "vacclevel", "missottv", "missomcv", "missodptpol")










Calculate the distribution of a categorical variable, overall and by groups:
  
  svymean( ~ ethnicity , dhs_design , na.rm = TRUE )

svyby( ~ ethnicity , ~ urban_rural , dhs_design , svymean , na.rm = TRUE )



#ADD other variables... - have to do this after, as variables with NA don't run
#Need to add SE as NA for each of these variables as well

by_strata %<>%
  mutate(country = "Nigeria",
         year = 2016,
         pcv1 = NA,
         pcv2 = NA,
         pcv3 = NA,
         rota1 = NA,
         rota2 = NA,
         all8vacc_ontime = NA,
         dropout_abs = dpt1[1]-dpt3[1],
         #dropout_rel = ((dpt1-dpt3)/dpt1)*100,
         anc1st = NA
  )



#ADD c_number_unw, c_number_w, c_distribution

use
svytotal(~enroll, dclus1, deff=TRUE)?
  
# Unweighted Counts
# Count the unweighted number of records in the survey sample, overall and by groups:
  
  sum( weights( dhs_design , "sampling" ) != 0 )

svyby( ~ one , ~ urban_rural , dhs_design , unwtd.count )

# Weighted Counts
# Count the weighted size of the generalizable population, overall and by groups:
  
  svytotal( ~ one , dhs_design )

svyby( ~ one , ~ urban_rural , dhs_design , svytotal )





#out[[1]] <- lapply(out[[1]], fct2)
#Reduce(function(...) left_join(by = names(out[[1]][[1]])[1], ...), out[[1]])


#a <- left_join(fct2(out[[1]]), fct2(out[[2]]))
#b <- left_join(fct2(out[[3]]), fct2(out[[4]]))



a <- fct3(a)
b <- fct3(b)
bind_rows(c(a, b), .id = "key")


by_region <- NGA2016 %>%
              group_by(h_region) %>%
              summarize(
                ttv2 = mean(ttv2, na.rm = T) * 100,
                ttvok = mean(ttvok, na.rm = T) * 100,
                bcg = mean(bcg, na.rm = T) * 100,
                dpt1 = mean(dpt1, na.rm = T) * 100,
                dpt2 = mean(dpt2, na.rm = T) * 100,
                dpt3 = mean(dpt3, na.rm = T) * 100,
                polio0 = mean(polio0, na.rm = T) * 100,
                polio1 = mean(polio1, na.rm = T) * 100,
                polio2 = mean(polio2, na.rm = T) * 100,
                polio3 = mean(polio3, na.rm = T) * 100,
                #pcv1
                #pcv2
                #pcv3
                #rota1
                #rota2
                yfv = mean(yfv, na.rm = T) * 100,
                mcv = mean(mcv, na.rm = T) * 100,
                all8vacc = mean(all8vacc, na.rm = T) * 100,
                complete = ((grep("2", vacclevel) %>% length)/length(na.omit(vacclevel)))*100,
                incomplete = ((grep("1", vacclevel) %>% length)/length(na.omit(vacclevel)))*100,
                unimmunised = ((grep("0", vacclevel) %>% length)/length(na.omit(vacclevel)))*100,
                vcard_seen = mean(vcard_seen, na.rm = T) * 100,
                vcard_own = mean(vcard_own, na.rm = T) * 100,
                #all8vacc_ontime
                #ADD c_number_unw, c_number_w, c_distribution
                #ADD other variables...
              )

by_region %<>%
  mutate(pcv1 = NA,
         pcv2 = NA,
         pcv3 = NA,
         rota1 = NA,
         rota2 = NA,
         all8vacc_ontime = NA,
         dropout_abs = dpt1-dpt3,
         dropout_rel = ((dpt1-dpt3)/dpt1)*100
  )






# Random bits of code

# Distribution of PSUs per stratum
plot(table(colSums(with(NGA2016, table(hh1, strata)) > 0)),
     xlab = "number of PSUs per stratum", ylab = "number of strata")

# Distribution of households (SSU) per PSU
plot(table(with(NGA2016, table(hh1, strata)))[-1],
     xlab = "number of households (SSUs) per PSU", ylab = "number of PSUs")

mcsurvey::check_replicates(NGA2016, strata, hh1, hh2)

options(survey.lonely.psu = "certainty")

### Summarize vaccination by region
svytotal(~ count, svyset, na.rm = T)

a<-svyby(~ttv2, ~h_region, svyset, svymean, keep.var=TRUE)

svymean(~ mcv, ~ h_region, na.rm = TRUE) %>%
  select(contains("TRUE")) %>%
  `names<-`(c("percentage", "SE"))


###Apply svy and subset children aged 12-23m

a <- svymean(~ttv2, NGA2016)



2. cocov by vacclevel

global cocov1 anc1 sba h_watert h_toilet diarr pneumo missottv missodptpol
global cocov2 /*anc1st*/ anc4 hfdel pneumo2
global cocov4 c_vita c_sleptitn coco8_all coco8alt_all coco_lt3 coco_ge6 missomcv
global cocov5 pneumo3


3. stratifiers by cocov