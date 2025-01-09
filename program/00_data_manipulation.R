###-----------------------###
###   Data Manipulation   ###
###-----------------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))


# Packages ----
library(readxl)


# Load data ----

# https://health.data.ny.gov/Health/Hospital-Inpatient-Discharges-SPARCS-De-Identified/4ny4-j5zv for 2019
# https://health.data.ny.gov/Health/Hospital-Inpatient-Discharges-SPARCS-De-Identified/tg3i-cinn for 2021


## 2019 ----

hospdata <- read.csv(file = "../raw/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2019.csv",
                     stringsAsFactors = F, header = T, sep = ",") # 3 minutes to run

### Diabetes mellitus ----

ny_diags <- levels(factor(hospdata$CCSR.Diagnosis.Description))
hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCSR.Diagnosis.Description))
diabetes_idx <- grep("diabetes mellitus", ny_diags, ignore.case = T)
hospdata$diabetes <- as.integer(hospdata$diagnosis %in% diabetes_idx)

### Amputations ----

ny_procs <- levels(factor(hospdata$CCSR.Procedure.Description))
hospdata$procedure <- as.numeric(as.factor(hospdata$CCSR.Procedure.Description))
amputation_idx <- grep("amput", ny_procs, ignore.case = T)
hospdata$amputated <- as.integer(hospdata$procedure %in% amputation_idx[amputation_idx!=112 & amputation_idx!=113])

#### Select and rename variables ----

hospdata <- hospdata[, c("Facility.Name", "Hospital.County", "Age.Group", "Gender", "Race",
                         "APR.Severity.of.Illness.Description", "APR.Risk.of.Mortality", "diabetes", "amputated")]
colnames(hospdata) <- c("ny_hosp_id", "ny_county", "cl_age", "males", "race",
                        "severe", "risky", "diabetes", "amputated")

#### Select diabetic population ----

diab_2019 <- hospdata[hospdata$diabetes == 1, ]

#### Remove NA ----

table(diab_2019$ny_county)
diab_2019 <- diab_2019[diab_2019$ny_county != "", ]
dim(diab_2019) # 44377

#### Save data set ----

save(diab_2019, file = "../data/diab_2019.Rda")


## 2021 ----

hospdata <- read.csv(file = "../raw/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2021.csv",
                     stringsAsFactors = F, header = T, sep = ",") # 3 minutes to run

### Diabetes mellitus ----

ny_diags <- levels(factor(hospdata$CCSR.Diagnosis.Description))
hospdata$diagnosis <- as.numeric(as.factor(hospdata$CCSR.Diagnosis.Description))
diabetes_idx <- grep("diabetes mellitus", ny_diags, ignore.case = T)
hospdata$diabetes <- as.integer(hospdata$diagnosis %in% diabetes_idx)

### Amputations ----

ny_procs <- levels(factor(hospdata$CCSR.Procedure.Description))
hospdata$procedure <- as.numeric(as.factor(hospdata$CCSR.Procedure.Description))
amputation_idx <- grep("amput", ny_procs, ignore.case = T)
hospdata$amputated <- as.integer(hospdata$procedure %in% amputation_idx[amputation_idx!=112 & amputation_idx!=113])

#### Select and rename variables ----

hospdata <- hospdata[, c("Facility.Name", "Hospital.County", "Age.Group", "Gender", "Race",
                         "APR.Severity.of.Illness.Description", "APR.Risk.of.Mortality", "diabetes", "amputated")]
colnames(hospdata) <- c("ny_hosp_id", "ny_county", "cl_age", "males", "race",
                        "severe", "risky", "diabetes", "amputated")

#### Select diabetic population ----

diab_2021 <- hospdata[hospdata$diabetes == 1, ]

#### Remove NA ----

table(diab_2021$ny_county)
diab_2021 <- diab_2021[diab_2021$ny_county != "", ]

table(diab_2021$males)
diab_2021 <- diab_2021[diab_2021$males != "U", ]
dim(diab_2021) # 41114

#### Save data set ----
save(diab_2021, file = "../data/diab_2021.Rda")



# County fips and labels ----

county_label <- c("NA", "Albany", "Allegany", "Bronx", "Broome", "Cattaraugus",
                  "Cayuga", "Chautauqua", "Chemung", "Chenango", "Clinton",
                  "Columbia", "Cortland", "Delaware", "Dutchess", "Erie", "Essex",
                  "Franklin", "Fulton", "Genesee", "Herkimer", "Jefferson", "Kings",
                  "Lewis", "Livingston", "Madison", "Manhattan", "Monroe", "Montgomery",
                  "Nassau", "Niagara", "Oneida", "Onondaga", "Ontario", "Orange",
                  "Orleans", "Oswego", "Otsego", "Putnam", "Queens", "Rensselaer",
                  "Richmond", "Rockland", "Saratoga", "Schenectady", "Schoharie",
                  "Schuyler", "St Lawrence", "Steuben", "Suffolk", "Sullivan",
                  "Tompkins", "Ulster", "Warren", "Wayne", "Westchester", "Wyoming", "Yates")

county_id_label <- data.frame(County = county_label, county_id = 1:length(county_label))

fips_counties <- data.frame(read_excel("../raw/fips_counties.xlsx"))

county_id_label <- merge(county_id_label, fips_counties, by = "County")

save(county_id_label, file = "../data/county_id_label.Rda")

