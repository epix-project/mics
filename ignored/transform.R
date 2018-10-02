### Packages:

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
library(lodown)     # get_catalog(), lodown()
library(survey)     # svydesign(), svyby(), svymean(), svyglm()
library(dplyr)      # filter(), select(), mutate(), mutate_if(), recode()
library(sp)         # plot(), spplot()
library(haven)      # read_dta()
library(tibble)     #
library(purrr)      #

# Loading the data:

### NEED TO MAKE THIS GO THROUGH EACH FILE IN THE FOLDER, NOT JUST ONE COUNTRY - This one is for Afghanistan

files <- c("AFKR70") ### This should be the list of file names of interest, using the DHS dataset labelling system

loadfiles <- function(x) {
  x %>%
    paste0(".*rds") %>%
    grep(dir(), value = TRUE) %>%
    readRDS() %>%
    to_factor(labelled_only = TRUE) %>%
    ### Need to make sure continuous variables are handled correctly when 0 has a label,
    # e.g. m1, m1a, m1d, m2n, m3n. Maybe we should do this step after the mutate part?
    remove_attributes("format.stata")
}

AFKR70 <- loadfiles(files) ### At the end of the pipeline we need to create a new set of cleaned up data files,
# ideally using  DHS labelling, or country and year, e.g. AFG2015

# formula(paste(files)) <- loadfiles(files) - is this how to use the file name from the list to make a new dataset?


# Selecting the variables we are interested in:

dhsvarlist <- names(AFKR70) ### I guess this needs to go in the pipeline so that it can be done consecutively
# for each country

# This is the list of variables in the original DHS datasets that we need to keep. Some are core and exist in
# all datasets, but some are country-specific (e.g. s630aa from Tanzania survey), or not used in all surveys
# (e.g. h51 to h59 are for penta, pcv and rota vaccines not introduced in all countries or at all time-points).
# I'm thinking that we need to keep the variables we need, and create empty variables when they dont exist so
# that the following mutate code lines work even when the original dataset didn't include that variable.

varlist <- c("v001", "v002", "v004", "v005", "v008", "v012", "v013", "v021", "v022", "v023", "v024",
             "v025", "v101", "v102", "v106", "v113", "v115", "v116", "v119", "v130", "v131", "v134",
             "v140", "v141", "v149", "v161", "v190", "v191", "v212", "v426", "v459", "v463a", "v463b",
             "v467d", "v511", "v626", "v717",
             "sdistri", "sslumc", "sslumo",
             "b3", "b4", "b5", "b8", "caseid", "midx", "bord", "hw1",
             "m1", "m1a", "m1d", "m2a", "m2b", "m2c", "m2d", "m2e", "m2f", "m2g", "m2h", "m2i", "m2j",
             "m2k", "m2l", "m2m", "m2n", "m3a", "m3b", "m3c", "m3d", "m3e", "m3f", "m3g", "m3h", "m3i",
             "m3j", "m3k", "m3l", "m3m", "m3n", "m13", "m14", "m15", "m45", "m46", "m49a", "m51", "m52",
             "m70", "m71", "m72",
             "ml0", "ml1",
             "h0", "h1", "h2", "h2y", "h2m", "h2d", "h3", "h3y", "h3m", "h3d", "h4", "h4y", "h4m", "h4d",
             "h5", "h5y", "h5m", "h5d", "h6", "h6y", "h6m", "h6d", "h7", "h7y", "h7m", "h7d", "h8", "h8y",
             "h8m", "h8d", "h9", "h9y", "h9m", "h9d",
             "h51", "h51y", "h51m", "h51d", "h52", "h52y", "h52m", "h52d", "h53", "h53y", "h53m", "h53d",
             "h54", "h54y", "h54m", "h54d", "h55", "h55y", "h55m", "h55d", "h56", "h56y", "h56m", "h56d",
             "h57", "h57y", "h57m", "h57d", "h58", "h58y", "h58m", "h58d", "h59", "h59y", "h59m", "h59d",
             "syf", "h34",
             "hw70", "hw71", "hw72", "hw73",
             "h11", "h11b", "h22", "h31", "h31b", "h31c", "h12a", "h12b", "h12c", "h12d", "h12e", "h12f", "h12g",
             "h12h", "h12j", "h12k", "h12m", "h12n", "h12o", "h12p", "h12s", "h12t", "h12u", "h12x", "h32a",
             "h32b", "h32c", "h32d", "h32e", "h32f", "h32g", "h32h", "h32j", "h32k", "h32m", "h32n", "h32o",
             "h32p", "h32s", "h32t", "h32u", "h32x", "hw1", "h15", "h15b", "h37i", "h37j", "h44a", "h46a",
             "s630aa", "s630ab", "s630ac", "s630ad", "s630ae", "s630af", "s630ag", "s630ah", "s630ai", "s630aj",
             "s630ak", "s630al", "s630am", "s630an", "s630ao", "s630ap", "s630aq", "s630ar", "s630as", "s630at",
             "s630au", "s630av", "s630ax")
keep <- varlist %in% dhsvarlist
create <- !varlist %in% dhsvarlist
AFG2015 <- if (any(keep)) select(AFKR70, varlist[keep])
### BUG 1
# AFG2015 %>% if (any(create)) mutate(AFG2015, varlist[create] = NA) #TO CREATE EMPTY VARIABLES THAT DON'T EXIST WITH == NA

###DO WE NEED TO CREATE A NEW DATAFRAME HERE? OR JUST SAVE IT ALL AT THE END?



# Dividing the weights by 1,000,000 (because the weights have been multiplied by 1,000,000 in order to save space on disk):

AFG2015$v005 <- AFG2015$v005 / 1e6

# Attaching the data set to the var_def and var_def_ functions defined below for easier use:

var_def <- function(...) mclabelled::var_def(AFG2015, ...) # the NSE version
var_def_ <- function(x) mclabelled::var_def(AFG2015[x]) # the character vector version

# To look at the definitions of the variables selected:

var_def()

# Defining new variables and recoding some others
#The first line of the mutate_if code recodes all the variables that have answer “no” or “yes” only into boolean

vaccination <- c("vaccination date on card", "reported by mother") ### What does this do with NA? Coded as "no"?
AFG2015 %<>%
   mutate_if(~ identical(levels(.), c("no", "yes")), ~ . == "yes") %>%
   mutate(count     = 1,      # counting variable

          # Population subset indicators
          c_age6m = c_age < 6,              # Aged below 6 months
          c_age1yr = b8 == 1,               # Aged 12-23 months
          c_ageu2yrs = b8 == 0 | b8 == 1,   # Aged 0-23 months (under 2 years)
          c_age = v008-b3,                  # Age in months
          c_age1yr_all = between(c_age, 12, 23),   # Children aged 12-23 months to form the denominator for immunisation coverage
          alive = b5 == 1) %>%                        # Children who are still alive
          #ylclt24m = %>%

# *ylclt24* /*Youngest child living with the mother for each mother*/
#   generate ylclt24 = 1
#   replace ylclt24=. if b9~=0 /*keep if living with the mother*/
#   replace ylclt24=. if c_age>=24 /* keep only children less than 2 years*/
#   bysort v001 v002 v003: egen minbidx=min(bidx)
#   replace ylclt24=. if bidx>minbidx /* need to drop those that are bidx==2 and minbidx==1*/

### BUG 2
# mutate code doesn't seem to run through with newly created variables. e.g. c_age. Have to start a new mutate command.

          # Strata variables
    mutate(age_class = cut(AFG2015$hw1, c(0, 6, 12, 24, 36, 48, 60), right = FALSE), ### HW1 EMPTY - not in all datasets...
          c_age_grp = cut(AFG2015$c_age, c(0, 6, 12, 24, 36, 48, 60), right = FALSE)) %>% ### Better to use this
    mutate(h_region = v024,
          #h_subregion = sdist,   ###Not all datasets have this variable and some are labelled differently
          #h_ecoregion = secoreg, ###Not all datasets have this variable and some are labelled differently
          #slum,
          h_urban     = factor(recode(v025,
                                    `rural`  = "rural",
                                    `urban`  = "urban",
                                    .default =  "NA")),
          h_urbanpoor = ifelse(v025 == "urban" & v190 %in% c("richer", "richest", "middle"), "Urban non-poor",
                               ifelse(v025 == "rural" & v190 %in% c("richer", "richest", "middle"), "Rural non-poor",
                                      ifelse(v025 == "urban" & v190 %in% c("poorest", "poorer"), "Urban poor",
                                             ifelse (v025 == "rural" & v190 %in% c("poorest", "poorer"), "Rural poor", NA)))),

          h_wealth    = v190,    # Wealth index
          h_wealthsc  = v191,    # Wealth score
          m_age       = v012,    # Respondent's current age
          m_agegrp    = v013,     # Respondent's age in 5-year groups
          m_agegrp3   = ifelse(v013 == "15-19", "Under 20 years",
                               ifelse(v013 %in% c("20-24", "25-29", "30-34"), "20 to 34 years",
                                      ifelse(v013 %in% c("35-39", "40-44", "45-49"), "35 to 49 years", NA))),
          m_edu       = factor(recode(v106,            # Respondent's highest educational level
                                      `no educatuion`  = "no education",
                                      `primary`        = "primary",
                                      `secondary`      = "secondary",
                                      `higher`         = "higher",
                                      .default         =  "NA")),
          m_edu3      = factor(recode(v106,            # Respondent's highest educational level
                                      `no educatuion`  = "no education",
                                      `primary`        = "primary",
                                      `secondary`      = "secondary or higher",
                                      `higher`         = "secondary or higher",
                                      .default         =  "NA")),
          m_edu4      = factor(recode(v149,            # Respondent's highest educational level
                                      `no education`         = "no education",
                                      `incomplete primary`   = "incomplete primary",
                                      `complete primary`     = "complete primary",
                                      `incomplete secondary` = "secondary or higher",
                                      `complete secondary`   = "secondary or higher",
                                      `higher`               = "secondary or higher",
                                      .default         =  "NA")),
          m_rel       = case_when(grepl("catholic|christian|protestant|orthodox",   v130) ~ "christian",
                                  grepl("muslim|islam", v130) ~ "muslim",
                                  grepl("buddh*",       v130) ~ "buddhist",
                                  grepl("hindu*", v130) ~ "hindu",
                                  TRUE                        ~ "no religion/traditional/other"),
          m_ethn      = v131,                          # Respondent's ethnic group - ### might need to re-group this so that any ethnic groups with <50 coded as other
          m_agefb     = ifelse(v212 < 16, "Under 16 years", # Age at first birth
                               ifelse(between(v212, 16, 49), "16-49 years", NA)),
          m_agefu     = ifelse(v511 < 16, "Under 16 years", # Age at first union
                               ifelse(between(v511, 16, 49), "16-49 years", NA)),
          m_occu      = v717,
          c_sex       = b4,
          c_bord      = bord,
          c_bord3     = ifelse(bord == 1, "first child",
                               ifelse(between(bord, 2, 4), "2nd to 4th",
                               ifelse(between(bord, 5, 20), "5th or more", NA))),
          c_bord4     = ifelse(bord == 1, "first child",
                               ifelse(between(bord, 2, 3), "2nd to 3rd",
                               ifelse(between(bord, 4, 5), "4th to 5th",
                               ifelse(between(bord, 6, 20), "6th or more", NA)))),
          disthf      = factor(recode(v467d,           # Distance to a health facility is a big problem
                                      `big problem`        = "big problem",
                                      `no problem`         = "not a big problem",
                                      `not a big problem`  = "not a big problem",
                                      .default             =  "NA")),
          cluster     = factor(v021) %>%

# AFG2015 %<>%
#   mutate (clus_disthf = group_by(cluster)) %>%
#   summarise(clus_disthf = mean(disthf)*100) %>%
#   mutate(ifelse(clus_disthf <50, "distance not a big problem",
#                           ifelse(between(clus_disthf, 50, 100)), "distance a big problem", NA)) %>%
#
#  Looks like this in Stata:
# *disthf* /*Problems seeking help for illness - distance to health facility is a big problem*/ NOT ASKED IN SHORT VERSION OF WOMEN'S QUESTIONNAIRE
# generate disthf = v467d
# label variable disthf "Distance to health facility is a big problem"
# recode disthf (2=0) (9=.)
# label define disthf 0 "Is not a big problem" 1 "Is a big problem"
# label value disthf disthf
#
# *clus_disthf*
# bysort v021: egen clus_disthf = mean(disthf)
# replace clus_disthf = clus_disthf*100
# *tabstat clus_disthf, statistics( mean ) by(v021) columns(variables)
# recode clus_disthf (0/49.9999999 = 0)(50/100 = 1), generate(clus_disthf2)
#

          ### 1. Morbidity, careseeking and antibiotic use:

          # Morbidity - Symptoms of fever, cough, congestion, ARI and diarrhoea in the last 2 weeks

    mutate(diarrh    = ifelse(h11  == "yes, last two weeks", T,      #Any diarrhoea in the last 2 weeks
                             ifelse(c_age>=60 | alive==F, NA, F)),
          ### What happens when "yes, last 24 hours" exisits --> use grepl?
          sev_diarrh = ifelse(h11  == "yes, last two weeks" & h11b == "yes", T,   #Bloody diarrheoa - DHS only
                              ifelse(c_age>=60 | alive==F, NA, F)),
          diarrh_no_fever = ifelse(diarrh == T & h22 == "no", T,
                                   ifelse(diarrh ==T & h22 == "yes", F,
                                          ifelse(c_age>=60 | alive==F, NA, NA))),
          cough     = ifelse(h31  == "yes, last two weeks", T,      #Any cough in the last 2 weeks
                             ifelse(c_age>=60 | alive==F, NA, F)),
          ### What happens when "yes, last 24 hours" exisits --> use grepl?
          cough_only = ifelse(h31 == "yes, last two weeks" & h31b != "yes" & !(h31c %in% c("chest only", "both")) & h22 == "no", T,
                            ifelse(c_age>=60 | alive==F, NA, F)),
          congest = ifelse(!(h31c %in% c("chest only", "both")) & (h31== "yes, last two weeks" | h31b== "yes"), T,
                           ifelse(c_age>=60 | alive==F, NA, F)),
          congest_no_fever = ifelse(!(h31c %in% c("chest only", "both")) & (h31== "yes, last two weeks" | h31b== "yes") & h22 == "no", T,
                           ifelse(c_age>=60 | alive==F, NA, F)),
          ari       = ifelse(h31b == "yes" & h31c %in% c("chest only", "both"), T,
                             ifelse(c_age>=60 | alive==F, NA, F)),
          ari_no_fever = ifelse(h31b == "yes" & h31c %in% c("chest only", "both") & h22 == "no", T,
                                ifelse(c_age>=60 | alive==F, NA, F)),
          fever     = ifelse(h22 == "yes", T,
                             ifelse(c_age>=60 | alive==F, NA, F))) %>%

          # Careseeking and antibiotic use for fever, cough, congestion, ARI and diarrhoea



          # Immunisation
          ttv2 = m1 %in% c("2", "3", "4", "5", "6", "7"),   # Two or more TTV this pregnancy
          ### BUG 3
          # Code doesn't work as m1, m1d, m1a are recognised as a factors
          ### ttvok = ttv2 == "yes" |                   # Codes as "yes" if received 2 or more TTV this pregnancy
          #   ((m1+m1a)>1 & m1d<3 & m1<8 & m1a<8) |   # Codes as "yes" if received 2 or more TTV within last 3 years
          #   ((m1+m1a)>2 & m1d<5 & m1<8 & m1a<8) |   # Codes as "yes" if received 3 or more TTV within last 5 years
          #   ((m1+m1a)>3 & m1d<10 & m1<8 & m1a<8) |  # Codes as "yes" if received 4 or more TTV within last 10 years
          #   ((m1+m1a)>4 & m1<8 & m1a<8),            # Codes as "yes" if received 5 or more lifetime TTV
          polio0     = h0 %in% vaccination,
          bcg        = h2 %in% vaccination,
          dpt1       = h3 %in% vaccination,
          polio1     = h4 %in% vaccination,
          dpt2       = h5 %in% vaccination,
          polio2     = h6 %in% vaccination,
          dpt3       = h7 %in% vaccination,
          polio3     = h8 %in% vaccination,
          mcv        = h9 %in% vaccination,
          penta1     = h51 %in% vaccination,
          penta2     = h52 %in% vaccination,
          penta3     = h53 %in% vaccination,
          pcv1       = h54 %in% vaccination,
          pcv2       = h55 %in% vaccination,
          pcv3       = h56 %in% vaccination,
          rota1      = h57 %in% vaccination,
          rota2      = h58 %in% vaccination,
          rota3      = h59 %in% vaccination,
          #yfv        = syf %in% vaccination,

          # Immunisation timeliness              ###IS THERE ANY WAY TO SHORTEN THIS CODE SO THAT IT LOOPS OVER h*y etc?
          bcg_mth  = (((h2y-1900)*12)+h2m)-b3,
          bcg_day  = ((bcg_mth*30.4375)+h2d),
          bcg_wk   = floor((bcg_day/7)/1),       ###IS THIS THE RIGHT USE OF FLOOR()???
          dpt1_mth = (((h3y-1900)*12)+h3m)-b3,
          dpt1_day = ((dpt1_mth*30.4375)+h3d),
          dpt1_wk  = floor((dpt1_day/7)/1),
          dpt2_mth = (((h5y-1900)*12)+h5m)-b3,
          dpt2_day = ((dpt2_mth*30.4375)+h5d),
          dpt2_wk  = floor((dpt2_day/7)/1),
          dpt3_mth = (((h7y-1900)*12)+h7m)-b3,
          dpt3_day = ((dpt3_mth*30.4375)+h7d),
          dpt3_wk  = floor((dpt3_day/7)/1),
          #polio0_mth = (((h0y-1900)*12)+h0m)-b3,
          #polio0_day = ((polio0_mth*30.4375)+h0d),
          #polio0_wk  = floor((polio0_day/7)/1),
          polio1_mth = (((h4y-1900)*12)+h4m)-b3,
          polio1_day = ((polio1_mth*30.4375)+h4d),
          polio1_wk  = floor((polio1_day/7)/1),
          polio2_mth = (((h6y-1900)*12)+h6m)-b3,
          polio2_day = ((polio2_mth*30.4375)+h6d),
          polio2_wk  = floor((polio2_day/7)/1),
          polio3_mth = (((h8y-1900)*12)+h8m)-b3,
          polio3_day = ((polio3_mth*30.4375)+h8d),
          polio3_wk  = floor((polio3_day/7)/1),
          mcv_mth = (((h9y-1900)*12)+h9m)-b3,
          mcv_day = ((mcv_mth*30.4375)+h9d),
          mcv_wk  = floor((mcv_day/7)/1),

          ### Stata code looks lie this - could the above code be shortened
# *dpt*
#  tokenize 3 5 7
#  forvalues x=1/3{
#  generate dpt`x'_mth = (((h`1'y-1900)*12)+h`1'm)-b3
#  generate dpt`x'_day = ((dpt`x'_mth*30.4375)+h`1'd)
#  generate dpt`x'_wk = dpt`x'_day/7
#  replace dpt`x'_wk = floor(dpt`x'_wk/1)
#  mac shift
#  }
          dptnum   = rowSums(AFG2015[c("dpt1", "dpt2", "dpt3")], na.rm = TRUE), ###DOES THE DATAFRAME NEED TO BE STATED HERE. NEED TO MAKE THE CODE WORK FOR ALL DATASETS?
          polionum = rowSums(AFG2015[c("polio1", "polio2", "polio3")], na.rm = TRUE),
          ###WHAT IF ALL ARE MISSING? THEN SHOULD BE NA. IN STATA CAN SPECIFY , MISSING OPTION - missing only if all of them are NA
          pentanum = rowSums(AFG2015[c("penta1", "penta2", "penta3")], na.rm = TRUE),
          pcvnum   = rowSums(AFG2015[c("pcv1", "pcv2", "pcv3")], na.rm = TRUE),
          rotanum  = rowSums(AFG2015[c("rota1", "rota2", "rota3")], na.rm = TRUE),
          all8vacc = bcg==T & dpt1==T & dpt2==T & dpt3==T & polio1==T & polio2==T & polio3==T & mcv==T,
          somevacc = bcg==T | dpt1==T | dpt2==T | dpt3==T | polio1==T | polio2==T | polio3==T | mcv==T,
          vacclevel = ifelse(somevacc==F, "Unvaccinated",
                             ifelse(somevacc==T & all8vacc==F, "Incomplete vaccination" ,
                                    ifelse(all8vacc==T, "Complete vaccination", NA))),
          all13vacc = bcg==T & dpt1==T & dpt2==T & dpt3==T & polio1==T & polio2==T & polio3==T & mcv==T
          & pcv1==T & pcv2==T & pcv3==T & rota1==T & rota2==T,
          vaccnum = rowSums(AFG2015[c("bcg", "dpt1", "dpt2", "dpt3", "polio1", "polio2", "polio3", "mcv")], na.rm = TRUE),
          ### Need to recode vaccnum as missing if more than 20% of observations are missing
          vaccnum13 = rowSums(AFG2015[c("bcg", "dpt1", "dpt2", "dpt3", "polio1", "polio2", "polio3", "mcv", "pcv1", "pcv2", "pcv3", "rota1","rota2")], na.rm = TRUE),
          c_vita = ifelse(h34 == "yes", T,                 # Received vitamin A in last 6 months
                          ifelse(h34 %in% c("no", "don't know"), F, NA) %>%

          # Adjust for likely misinterpretation of polio0 and polio1
          ### See Stata code below - need to add this

          # Breastfeeding
            *eib* /*early initiation of bf*/
            generate eib = v426
          label variable eib "Early initiation of breastfeeding"
          recode eib (0/100=1)(101/900=0)(999=.)
          label value eib yesno

          *ebf* /*exclusive breastfeeding*/
            ** Exclusive breast feeding
          gen water=0
          gen liquids=0
          gen milk=0
          gen solids=0
          gen breast=0
          gen bottle=0

          *TO DETERMINE IF CHILD IS GIVEN WATER, SUGAR WATER, JUICE, TEA OR OTHER.
          replace water=1 if (v409>=1 & v409<=7)

            *IF GIVEN OTHER LIQUIDS
          foreach xvar of varlist v409a v410 v410* v413* {
            replace liquids=1 if `xvar'>=1 & `xvar'<=7
          }
            cap replace liquids=1 if v412c>=1 & v412c<=7

            *IF GIVEN POWDER/TINNED milk, FORMULA OR FRESH milk
            foreach xvar of varlist v411 v411a v412 v414p {
            replace milk=1 if `xvar'>=1 & `xvar'<=7
            }

            *IF STILL BREASTFEEDING
            replace breast=1 if m4==95

            * IF WAS EVER BOTTLE FED
            replace bottle=1 if m38==1

            *IF GIVEN ANY SOLID FOOD
            foreach xvar of varlist v414* {
            replace solids=1 if `xvar'>=1 & `xvar'<=7
            }
            replace solids=1 if v412a==1 | v412b==1

            gen diet=7
            replace diet=0 if water==0 & liquids==0 & milk==0 & solids==0
            replace diet=1 if water==1 & liquids==0 & milk==0 & solids==0
            replace diet=2 if            liquids==1 & milk==0 & solids==0
            replace diet=3 if                         milk==1 & solids==0
            replace diet=4 if                         milk==0 & solids==1
            replace diet=5 if                         milk==1 & solids==1
            replace diet=6 if breast==0

            *diet=0: given only water (full bf)
            *diet=1: given only liquids (bf & liquids)
            *diet=2: given only milks (bf & milk)
            *diet=3: given only solids (bf & solids)
            *diet=4: given only milk and solids (bf & milk & solids)
            *diet=5: not still breastfeeding (weaned)
            *diet=6: not now being breastfed (m4~=95)

            ** Matching Table 11.3 for exclusive breast feeding in 0-5 month children **
            gen ebf=0
            replace ebf=1 if diet==0

            *Denominator should be youngest child living with mother under age 24 months


            tab ebf if c_age<6 & ylclt24==1 [iw=v005/1000000] /*To check report*/

            gen feeding=1
            replace feeding=2 if water==1
            replace feeding=3 if liquids==1
            replace feeding=4 if milk==1
            replace feeding=5 if solids==1
            replace feeding=0 if breast==0
            label define feeding 0 "Not breastfeeding" 1 "exclusive breastfeeding" 2 "+Water" 3 "+Liquids" 4 "+Other Milk" 5 "+Solids"
            label val feeding feeding

            *complimentary feeding is category #5 (+solids) and this matches the report for this age group which is 19.1%
            tab feeding if c_age<6 & ylclt24==1 [iw=v005/1000000]

            *creating the predominant breastfeeding variable.
            recode feeding (0 4 5=0) (1/3=1), gen(predom)

            *this matches the report of 77.5% for predominant breastfeeding
            tab predom if c_age<6 & ylclt24==1 [iw=v005/1000000]


            # ADD duration of bf



          toilet    = factor(recode(v116,
                                   `flush toilet`                           = "improved",
                                   `flush to piped sewer system`            = "improved",
                                   `flush to septic tank`                   = "improved",
                                   `flush to pit latrine`                   = "improved",
                                   `pit toilet latrine`                     = "improved",
                                   `ventilated improved pit latrine (vip)`  = "improved",
                                   `pit latrine with slab`                  = "improved",
                                   `composting toilet`                      = "improved",
                                   .default                                 = "unimproved")),
          facility  = case_when(grepl("government|public",   h44a) ~ "public",
                                grepl("private|specialized", h44a) ~ "private",
                                grepl("pharmacy|addo",       h44a) ~ "pharmacy",
                                TRUE                               ~ "other"),




### So far goes to here, but needs some more variables coding from Stata code (some below), and need to figure
### out how to handle numerical data now as factors...








# Adjust for likely misinterpretation of polio0 and polio1

### This is the Stata code - need to convert...

# generate polio3_1 = polio3
# replace polio3 = polio2 if polio0==1 & polionum<3 & dptnum>polionum /*This is correct now*/
# generate polio2_1 = polio2
# replace polio2 = polio1 if polio0==1 & polionum<3 & dptnum>polionum
# generate polio1_1 = polio1
# replace polio1 = polio0 if polio0==1 & polionum<3 & dptnum>polionum
# generate polio0_1 = polio0
# recode polio0 (1=0) if polio0==1 & polionum<3 & dptnum>polionum
#
# *Adjust DPT according to number of doses received
# recode dpt1 (0=1) if dptnum>0
# recode dpt2 (1=0) if dptnum<2
# recode dpt2 (0=1) if dptnum>1
# recode dpt3 (1=0) if dptnum<3
#
# *Adjust polio according to number of doses received
# recode polio1 (0=1) if polionum_all>1
# recode polio2 (0=1) if polionum_all>2
# recode polio2 (1=0) if polionum_all<2
# recode polio3 (1=0) if polionum_all<3
#
# *Adjust PCV according to number of doses received
# recode pcv1 (0=1) if pcvnum>0
# recode pcv2 (1=0) if pcvnum<2
# recode pcv2 (0=1) if pcvnum>1
# recode pcv3 (1=0) if pcvnum<3
#




*Valid vs crude- i.e. were vaccinations received on time?
*generate new summary variables using international definitions - see Luman et al*/
*BCG
generate bcg_ontime = .
recode bcg_ontime (.=1) if bcg_day~=. /*using international definition of ontime*/
recode bcg_ontime (1=2) if bcg_day<0 /*i.e. a negative number where date is before DOB*/
recode bcg_ontime (1=3) if bcg_day>30 /*Not sure if this makes sense for BCG. May be ok if delivered later.*/
label variable bcg_ontime "BCG received ontime"
label define ontime 1 "Ontime" 2 "Early/invalid date" 3 "Delayed"
label value bcg_ontime ontime
recode bcg_ontime (2/3=0), generate(bcg_valid)
label variable bcg_valid "BCG received ontime"
label value bcg_valid yesno
*Polio0
generate polio0_ontime = .
recode polio0_ontime (.=1) if polio0_day~=. /*using international definition of ontime*/
recode polio0_ontime (1=2) if polio0_day<0 /*i.e. a negative number where date is before DOB*/
recode polio0_ontime (1=3) if polio0_day>30
label variable polio0_ontime "Polio0 received ontime"
label value polio0_ontime ontime
recode polio0_ontime (2/3=0), generate(polio0_valid)
label variable polio0_valid "Polio0 received ontime"
label value polio0_valid yesno
*dpt1
generate dpt1_ontime = .
recode dpt1_ontime (.=1) if dpt1_day~=. /*using international definition of ontime*/
recode dpt1_ontime (1=2) if dpt1_day<25 /*minimum acceptable interval is 28 days, but dose within 4 days of minimum considered acceptable, so interval is 24 - Luman et al*/
recode dpt1_ontime (1=3) if dpt1_day>72
label variable dpt1_ontime "DPT1 received ontime"
label value dpt1_ontime ontime
recode dpt1_ontime (2/3=0), generate(dpt1_valid)
label variable dpt1_valid "DPT1 received ontime"
label value dpt1_valid yesno
*Polio1
generate polio1_ontime = .
recode polio1_ontime (.=1) if polio1_day~=. /*using international definition of ontime*/
recode polio1_ontime (1=2) if polio1_day<25 /*i.e. =<28 with 4 day leeway*/
recode polio1_ontime (1=3) if polio1_day>72
label variable polio1_ontime "Polio1 received ontime"
label value polio1_ontime ontime
recode polio1_ontime (2/3=0), generate(polio1_valid)
label variable polio1_valid "Polio1 received ontime"
label value polio1_valid yesno
*dpt2
generate dpt2_ontime = .
recode dpt2_ontime (.=1) if dpt2_day~=. /*using international definition of ontime*/
recode dpt2_ontime (1=2) if dpt2_day<53 /*i.e. =<56 with 4 day leeway*/
recode dpt2_ontime (1=2) if (dpt2_day - dpt1_day)<24 /*minimum acceptable interval is 28 days, but dose within 4 days of minimum considered acceptable, so interval is 24 - Luman et al*/
recode dpt2_ontime (1=3) if dpt2_day>100
label variable dpt2_ontime "DPT2 received ontime"
label value dpt2_ontime ontime
recode dpt2_ontime (2/3=0), generate(dpt2_valid)
label variable dpt2_valid "DPT2 received ontime"
label value dpt2_valid yesno
*polio2
generate polio2_ontime = .
recode polio2_ontime (.=1) if polio2_day~=. /*using international definition of ontime*/
recode polio2_ontime (1=2) if polio2_day<53 /*i.e. =<56 with 4 day leeway*/
recode polio2_ontime (1=2) if (polio2_day - polio1_day)<24 /*minimum acceptable interval is 28 days, but dose within 4 days of minimum considered acceptable, so interval is 24 - Luman et al*/
recode polio2_ontime (1=3) if polio2_day>100
label variable polio2_ontime "Polio2 received ontime"
label value polio2_ontime ontime
recode polio2_ontime (2/3=0), generate(polio2_valid)
label variable polio2_valid "Polio2 received ontime"
label value polio2_valid yesno
*dpt3
generate dpt3_ontime = .
recode dpt3_ontime (.=1) if dpt3_day~=. /*using international definition of ontime*/
recode dpt3_ontime (1=2) if dpt3_day<81 /*i.e. =<84 with 4 day leeway*/
recode dpt3_ontime (1=2) if (dpt3_day - dpt1_day)<24 /*minimum acceptable interval is 28 days, but dose within 4 days of minimum considered acceptable, so interval is 24 - Luman et al*/
recode dpt3_ontime (1=3) if dpt3_day>128
label variable dpt3_ontime "DPT3 received ontime"
label value dpt3_ontime ontime
recode dpt3_ontime (2/3=0), generate(dpt3_valid)
label variable dpt3_valid "DPT3 received ontime"
label value dpt3_valid yesno
*polio3
generate polio3_ontime = .
recode polio3_ontime (.=1) if polio3_day~=. /*using international definition of ontime*/
recode polio3_ontime (1=2) if polio3_day<81 /*i.e. =<84 with 4 day leeway*/
recode polio3_ontime (1=2) if (polio3_day - polio1_day)<24 /*minimum acceptable interval is 28 days, but dose within 4 days of minimum considered acceptable, so interval is 24 - Luman et al*/
recode polio3_ontime (1=3) if polio3_day>128
label variable polio3_ontime "Polio3 received ontime"
label value polio3_ontime ontime
recode polio3_ontime (2/3=0), generate(polio3_valid)
label variable polio3_valid "Polio3 received ontime"
label value polio3_valid yesno
*mcv
generate mcv_ontime = .
recode mcv_ontime (.=1) if mcv_day~=. /*using international definition of ontime*/
recode mcv_ontime (1=2) if mcv_day<178 /*i.e. =<181 with 4 day leeway*/
recode mcv_ontime (1=3) if mcv_day>306
label variable mcv_ontime "MCV received ontime"
label value mcv_ontime ontime
recode mcv_ontime (2/3=0), generate(mcv_valid)
label variable mcv_valid "MCV received ontime"
label value mcv_valid yesno

*all8vacc_ontime* /*received all 8 basic vaccination doses on time*/
generate all8vacc_ontime = .
recode all8vacc_ontime (.=1) if bcg_ontime==1 & dpt1_ontime==1 & polio1_ontime==1 & dpt2_ontime==1 & polio2_ontime==1 & dpt3_ontime==1 & polio3_ontime==1 & mcv_ontime==1
recode all8vacc_ontime (.=0) if bcg_ontime~=. & dpt1_ontime~=. & polio1_ontime~=. & dpt2_ontime~=. & polio2_ontime~=. & dpt3_ontime~=. & polio3_ontime~=. & mcv_ontime~=.
label variable all8vacc_ontime "Received all 8 basic vaccination doses on time"
label values all8vacc_ontime yesno

*vcard_own* /*Vaccination card owned*/
recode h1 (. 0 3 9=0 "Do not have vaccination card") (1/2=1 "Have vaccination card") if b5==1, generate(vcard_own)
label variable vcard_own "Vaccination card owned"

*vcard_seen* /*Vaccination card seen*/
recode h1 (2/3 9=0 "Not seen") (.=0 "Not seen") (1=1 "Seen") if b5==1, generate(vcard_seen)
label variable vcard_seen "Vaccination card seen"














### Bits of code

ia %<>% select(v001, v002, v004, v005, v008, v012, v013, v021, v022, v023, v024,
               v025, v101, v102, v106, v113, v115, v116, v119, v130, v131, v134,
               v140, v141, v149, v161, v190, v191, v212, v426, v459, v463a, v463b,
               v467d, v511, v626, v717,
               sdistri, sslumc, sslumo,
               b3, b4, b5, b8, caseid, midx, bord, hw1,
               m1, m1a, m1d, m2a, m2b, m2c, m2d, m2e, m2f, m2g, m2h, m2i, m2j, m2k,
               m2l, m2m, m2n, m3a, m3b, m3c, m3d, m3e, m3f, m3g, m3h, m3i, m3j, m3k,
               m3l, m3m, m3n, m13, m14, m15, m45, m46, m49a, m51, m52, m70, m71, m72,
               ml0, ml1,
               h0, h1, h2, h2y, h2m, h2d, h3, h3y, h3m, h3d, h4, h4y, h4m, h4d, h5,
               h5y, h5m, h5d, h6, h6y, h6m, h6d, h7, h7y, h7m, h7d, h8, h8y, h8m, h8d,
               h9, h9y, h9m, h9d,
               h51, h51y, h51m, h51d, h52, h52y, h52m, h52d, h53, h53y, h53m, h53d, h54,
               h54y, h54m, h54d, h55, h55y, h55m, h55d, h56, h56y, h56m, h56d, h57, h57y,
               h57m, h57d, h58, h58y, h58m, h58d, h59, h59y, h59m, h59d, syf, h34,
               hw70, hw71, hw72, hw73,
               h11, h22, h31, h31b, h31c, h12a, h12b, h12c, h12d, h12e, h12f, h12g, h12h,
               h12j, h12k, h12m, h12n, h12o, h12p, h12s, h12t, h12u, h12x, h32a, h32b, h32c,
               h32d, h32e, h32f, h32g, h32h, h32j, h32k, h32m, h32n, h32o, h32p, h32s, h32t,
               h32u, h32x, hw1, h15, h15b, h37i, h37j, h44a, h46a, s630aa, s630ab, s630ac,
               s630ad, s630ae, s630af, s630ag, s630ah, s630ai, s630aj, s630ak, s630al,
               s630am, s630an, s630ao, s630ap, s630aq, s630ar, s630as, s630at, s630au, s630av, s630ax)


dhsvarlist <- names(ia)
varlist <- c("v001", "v002", "v004", "v005", "v008", "v012", "v013", "v021", "v022", "v023", "v024")
keep <- !varlist %in% dhsvarlist
create <- varlist %in% dhsvarlist
IND2015 <- if (any(keep)) select(ia, varlist[keep])


ia %<>%
  if (any(keep)) select(varlist[keep])
%>%
  if (any(create)) mutate(varlist[create] = NA)
