###----------------------------###
###   Direct Standardization   ###
###----------------------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))

output_path <- normalizePath("../output", winslash = "/")
if (!dir.exists(output_path)) {
  dir.create(output_path)
}


# Packages ----

library(gmodels)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(grid)
library(readxl)
library(usmap)
library(viridis)
library(gridExtra)


# Load data ----

load("../data/diab_2019.Rda")
load("../data/diab_2021.Rda")
load("../data/county_id_label.Rda")


# Direct standardization ----

## 2019 ----

# Frequencies
table(diab_2019$males)
table(diab_2019$cl_age)

# Common structure for the denominator
n_terms <- length(levels(as.factor(diab_2019$cl_age))) * length(levels(as.factor(diab_2019$males)))
std_m <- matrix(c(rep(0, 1, n_terms)), length(levels(as.factor(diab_2019$cl_age))))
rownames(std_m) <- levels(as.factor(diab_2019$cl_age))
colnames(std_m) <- levels(as.factor(diab_2019$males))

# Population table for the whole population
pop_ij <- table(diab_2019[,c("cl_age")], diab_2019[,c("males")])
totpop <- sum(pop_ij)


### Hospital ----

hosp_names <- unique(diab_2019$ny_hosp_id)

# Crude and direct standardized rates for all hospitals
for (i in 1:length(hosp_names)) {

  target_group <- subset(diab_2019, diab_2019$ny_hosp_id == hosp_names[i])
  outcome_intarget <- subset(target_group, target_group$amputated == 1)
  
  label_target_group <- hosp_names[i]
  
  num_data <- table(outcome_intarget[, c("cl_age")], outcome_intarget[, c("males")])
  den_data <- table(target_group[, c("cl_age")], target_group[, c("males")])
  
  # Adapt num_data structure if needed
  if (nrow(num_data) > 0) {
    res <- merge(as.data.frame.matrix(num_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
    res <- res[2:3]
  }
  else {
    res <- as.data.frame.matrix(std_m)
  }
  res[is.na(res)] <- 0
  res <- as.matrix(res[, c(sort(names(res)))])
  rownames(res) <- rownames(std_m)
  res <- rowsum(res, row.names(res))
  
  # Adapt den_data structure if needed
  den_data <- merge(as.data.frame.matrix(den_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
  den_data <- den_data[, 2:3]
  den_data <- den_data[, c(sort(names(den_data)))]
  rownames(den_data) <- levels(as.factor(diab_2019$cl_age))
  colnames(den_data) <- levels(as.factor(diab_2019$males))
  den_data[is.na(den_data)] <- 0

  asr_ijt <- merge(as.data.frame.matrix(res/den_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
  asr_ijt <- as.matrix(asr_ijt[2:3])
  asr_ijt[is.na(asr_ijt)] <- 0
  
  # Crude rates
  cr_t <- ((sum(num_data)/sum(den_data) ))*100
  
  # Standardized rates
  sr_t <- (sum(asr_ijt*pop_ij)/totpop)*100
  
  if (sum(den_data) > 0) {
    sr_t.se   <- sqrt((sr_t/100*(1-sr_t/100))/(sum(den_data)))
    sr_t.ll95 <- sr_t - 1.96 * sr_t.se
    sr_t.ul95 <- sr_t + 1.96 * sr_t.se
  }
  else {
    sr_t.se   <- NA
    sr_t.ll95 <- NA
    sr_t.ul95 <- NA
  }
  
  output <- data.frame(label_target_group, round(cr_t,2), round(sr_t,2), round(sum(den_data),2),round(sr_t.ll95,2),round(sr_t.ul95,2))
  names(output) <- c("Hospital", "cr_2019", "dsr_2019", "N_2019", "Lower 95% CI_2019", "Upper 95% CI_2019")
  
  if (i==1) {dsr_output_hospital_2019 <- output} else {dsr_output_hospital_2019 <- rbind(dsr_output_hospital_2019, output)}
  rm(output)
  
}

tail(dsr_output_hospital_2019 %>% arrange(dsr_2019))


### County ----

county_names <- unique(diab_2019$ny_county)

# Crude and direct standardized rates for all counties
for (i in 1:length(county_names)) {

  target_group <- subset(diab_2019, diab_2019$ny_county == county_names[i])
  outcome_intarget <- subset(target_group, target_group$amputated == 1)
  
  label_target_group <- county_names[i]
  
  num_data <- table(outcome_intarget[, c("cl_age")], outcome_intarget[, c("males")])
  den_data <- table(target_group[, c("cl_age")], target_group[, c("males")])
  
  # Adapt num_data structure if needed
  if (nrow(num_data) > 0) {
    res <- merge(as.data.frame.matrix(num_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
    res <- res[2:3]
  }
  else {
    res <- as.data.frame.matrix(std_m)
  }
  res[is.na(res)] <- 0
  res <- as.matrix(res[, c(sort(names(res)))])
  rownames(res) <- rownames(std_m)
  res <- rowsum(res, row.names(res))
  
  # Adapt den_data structure if needed
  den_data <- merge(as.data.frame.matrix(den_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
  den_data <- den_data[, 2:3]
  den_data <- den_data[, c(sort(names(den_data)))]
  rownames(den_data) <- levels(as.factor(diab_2019$cl_age))
  colnames(den_data) <- levels(as.factor(diab_2019$males))
  den_data[is.na(den_data)] <- 0
  
  asr_ijt <- merge(as.data.frame.matrix(res/den_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
  asr_ijt <- as.matrix(asr_ijt[2:3])
  asr_ijt[is.na(asr_ijt)] <- 0
  
  # Crude rates
  cr_t <- ((sum(num_data)/sum(den_data) ))*100
  
  # Standardized rates
  sr_t <- (sum(asr_ijt*pop_ij)/totpop)*100
  
  if (sum(den_data) > 0) {
    sr_t.se   <- sqrt((sr_t/100*(1-sr_t/100))/(sum(den_data)))
    sr_t.ll95 <- sr_t - 1.96 * sr_t.se
    sr_t.ul95 <- sr_t + 1.96 * sr_t.se
  }
  else {
    sr_t.se   <- NA
    sr_t.ll95 <- NA
    sr_t.ul95 <- NA
  }
  
  output <- data.frame(label_target_group, round(cr_t,2), round(sr_t,2), round(sum(den_data),2),round(sr_t.ll95,2),round(sr_t.ul95,2))
  names(output) <- c("county_id", "cr_2019", "dsr_2019", "N_2019", "Lower 95% CI_2019", "Upper 95% CI_2019")
  
  if (i==1) {dsr_output_county_2019 <- output} else {dsr_output_county_2019 <- rbind(dsr_output_county_2019, output)}
  rm(output)
  
}

dsr_output_county_2019 %>% arrange(dsr_2019)


## 2021 ----

# Frequencies
table(diab_2021$males)
table(diab_2021$cl_age)

# Common structure for the denominator
n_terms <- length(levels(as.factor(diab_2021$cl_age))) * length(levels(as.factor(diab_2021$males)))
std_m <- matrix(c(rep(0, 1, n_terms)), length(levels(as.factor(diab_2021$cl_age))))
rownames(std_m) <- levels(as.factor(diab_2021$cl_age))
colnames(std_m) <- levels(as.factor(diab_2021$males))

# Population table for the whole population
pop_ij <- table(diab_2021[,c("cl_age")], diab_2021[,c("males")])
totpop <- sum(pop_ij)


### Hospital ----

hosp_names <- unique(diab_2021$ny_hosp_id)

# Crude and direct standardized rates for all hospitals
for (i in 1:length(hosp_names)) {
  
  target_group <- subset(diab_2021, diab_2021$ny_hosp_id == hosp_names[i])
  outcome_intarget <- subset(target_group, target_group$amputated == 1)
  
  label_target_group <- hosp_names[i]
  
  num_data <- table(outcome_intarget[, c("cl_age")], outcome_intarget[, c("males")])
  den_data <- table(target_group[, c("cl_age")], target_group[, c("males")])
  
  # Adapt num_data structure if needed
  if (nrow(num_data) > 0) {
    res <- merge(as.data.frame.matrix(num_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
    res <- res[2:3]
  }
  else {
    res <- as.data.frame.matrix(std_m)
  }
  res[is.na(res)] <- 0
  res <- as.matrix(res[, c(sort(names(res)))])
  rownames(res) <- rownames(std_m)
  res <- rowsum(res, row.names(res))
  
  # Adapt den_data structure if needed
  den_data <- merge(as.data.frame.matrix(den_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
  den_data <- den_data[, 2:3]
  den_data <- den_data[, c(sort(names(den_data)))]
  rownames(den_data) <- levels(as.factor(diab_2021$cl_age))
  colnames(den_data) <- levels(as.factor(diab_2021$males))
  den_data[is.na(den_data)] <- 0
  
  asr_ijt <- merge(as.data.frame.matrix(res/den_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
  asr_ijt <- as.matrix(asr_ijt[2:3])
  asr_ijt[is.na(asr_ijt)] <- 0
  
  # Crude rates
  cr_t <- ((sum(num_data)/sum(den_data) ))*100
  
  # Standardized rates
  sr_t <- (sum(asr_ijt*pop_ij)/totpop)*100
  
  if (sum(den_data) > 0) {
    sr_t.se   <- sqrt((sr_t/100*(1-sr_t/100))/(sum(den_data)))
    sr_t.ll95 <- sr_t - 1.96 * sr_t.se
    sr_t.ul95 <- sr_t + 1.96 * sr_t.se
  }
  else {
    sr_t.se   <- NA
    sr_t.ll95 <- NA
    sr_t.ul95 <- NA
  }
  
  output <- data.frame(label_target_group, round(cr_t,2), round(sr_t,2), round(sum(den_data),2),round(sr_t.ll95,2),round(sr_t.ul95,2))
  names(output) <- c("Hospital", "cr_2021", "dsr_2021", "N_2021", "Lower 95% CI_2021", "Upper 95% CI_2021")
  
  if (i==1) {dsr_output_hospital_2021 <- output} else {dsr_output_hospital_2021 <- rbind(dsr_output_hospital_2021, output)}
  rm(output)
  
}

tail(dsr_output_hospital_2021 %>% arrange(dsr_2021))


### County ----

county_names <- unique(diab_2021$ny_county)

# Crude and direct standardized rates for all counties
for (i in 1:length(county_names)) {
  
  target_group <- subset(diab_2021, diab_2021$ny_county == county_names[i])
  outcome_intarget <- subset(target_group, target_group$amputated == 1)
  
  label_target_group <- county_names[i]
  
  num_data <- table(outcome_intarget[, c("cl_age")], outcome_intarget[, c("males")])
  den_data <- table(target_group[, c("cl_age")], target_group[, c("males")])
  
  # Adapt num_data structure if needed
  if (nrow(num_data) > 0) {
    res <- merge(as.data.frame.matrix(num_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
    res <- res[2:3]
  }
  else {
    res <- as.data.frame.matrix(std_m)
  }
  res[is.na(res)] <- 0
  res <- as.matrix(res[, c(sort(names(res)))])
  rownames(res) <- rownames(std_m)
  res <- rowsum(res, row.names(res))
  
  # Adapt den_data structure if needed
  den_data <- merge(as.data.frame.matrix(den_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
  den_data <- den_data[, 2:3]
  den_data <- den_data[, c(sort(names(den_data)))]
  rownames(den_data) <- levels(as.factor(diab_2021$cl_age))
  colnames(den_data) <- levels(as.factor(diab_2021$males))
  den_data[is.na(den_data)] <- 0
  
  asr_ijt <- merge(as.data.frame.matrix(res/den_data), as.data.frame.matrix(std_m), by = "row.names", all.y = TRUE)
  asr_ijt <- as.matrix(asr_ijt[2:3])
  asr_ijt[is.na(asr_ijt)] <- 0
  
  # Crude rates
  cr_t <- ((sum(num_data)/sum(den_data) ))*100
  
  # Standardized rates
  sr_t <- (sum(asr_ijt*pop_ij)/totpop)*100
  
  if (sum(den_data) > 0) {
    sr_t.se   <- sqrt((sr_t/100*(1-sr_t/100))/(sum(den_data)))
    sr_t.ll95 <- sr_t - 1.96 * sr_t.se
    sr_t.ul95 <- sr_t + 1.96 * sr_t.se
  }
  else {
    sr_t.se   <- NA
    sr_t.ll95 <- NA
    sr_t.ul95 <- NA
  }
  
  output <- data.frame(label_target_group, round(cr_t,2), round(sr_t,2), round(sum(den_data),2),round(sr_t.ll95,2),round(sr_t.ul95,2))
  names(output) <- c("county_id", "cr_2021", "dsr_2021", "N_2021", "Lower 95% CI_2021", "Upper 95% CI_2021")
  
  if (i==1) {dsr_output_county_2021 <- output} else {dsr_output_county_2021 <- rbind(dsr_output_county_2021, output)}
  rm(output)
  
}

dsr_output_county_2021 %>% arrange(dsr_2021)


## Comparisons ----

### Hospital ----

dsr_output_hospital <- merge(dsr_output_hospital_2019, dsr_output_hospital_2021, by = "Hospital")
dsr_output_hospital$diff <- dsr_output_hospital$dsr_2021 - dsr_output_hospital$dsr_2019

# Improved
table(dsr_output_hospital$diff < 0) 
head(dsr_output_hospital %>% arrange(diff), 5)

# Didn't change
table(dsr_output_hospital$diff == 0) 
dsr_output_hospital[dsr_output_hospital$diff == 0, ]

# Got worse
table(dsr_output_hospital$diff > 0) 
tail(dsr_output_hospital %>% arrange(diff), 5)


### County ----

dsr_output_county <- merge(dsr_output_county_2019, dsr_output_county_2021, by = "county_id")
dsr_output_county$diff <- dsr_output_county$dsr_2021 - dsr_output_county$dsr_2019

# Improved
table(dsr_output_county$diff < 0) 
head(dsr_output_county %>% arrange(diff), 5)

# Didn't change
table(dsr_output_county$diff == 0) 
dsr_output_county[dsr_output_county$diff == 0, ]

# Got worse
table(dsr_output_county$diff > 0) 
tail(dsr_output_county %>% arrange(diff), 4)


#### Maps by county ----

dsr_county_fips <- merge(dsr_output_county, county_id_label, by.x = "county_id", by.y = "County")

sr19 <- plot_usmap(regions = "counties",
                   include = "NY",
                   data = dsr_county_fips,
                   values = "dsr_2019") +
  scale_fill_viridis() +
  theme(legend.position = "bottom",
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "(a) Directly Standardized Amptutation Rates - 2019",
       caption = "Source: Hospital Inpatient Discharges (SPARCS De-Identified) 2017",
       fill = "Standardized amputation rate") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))


sr21 <- plot_usmap(regions = "counties",
                   include = "NY",
                   data = dsr_county_fips,
                   values = "dsr_2021") +
  scale_fill_viridis() +
  theme(legend.position = "bottom",
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "(b) Directly Standardized Amptutation Rates - 2021",
       caption = "Source: Hospital Inpatient Discharges (SPARCS De-Identified) 2017",
       fill = "Standardized amputation rate") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))


sr_diff <- plot_usmap(regions = "counties",
                      include = "NY",
                      data = dsr_county_fips,
                      values = "diff") +
  scale_fill_viridis() +
  theme(legend.position = "bottom",
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "(c) Difference in Directly Standardized Amptutation Rates",
       caption = "Source: Hospital Inpatient Discharges (SPARCS De-Identified) 2017",
       fill = "Difference") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))


grid.arrange(sr19, sr21, sr_diff, ncol = 3)

output_file <- file.path(output_path, "direct_standardization_maps.png")
png(output_file, width = 3600, height = 1200, res = 300)
grid.arrange(sr19, sr21, sr_diff, ncol = 3)
dev.off()

