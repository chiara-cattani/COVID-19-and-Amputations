###------------------------###
###   Summary Statistics   ###
###------------------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))

output_path <- normalizePath("../output", winslash = "/")
if (!dir.exists(output_path)) {
  dir.create(output_path)
}


# Packages ----

library(dplyr)
library(pastecs)
library(readxl)
library(ggplot2)
library(usmap)
library(viridis)
library(gridExtra)
library(corrplot)
library(gplots)


# Load data ----

load("../data/diab_2019.Rda")
load("../data/diab_2021.Rda")
load("../data/county_id_label.Rda")


## Save plot function ----
save_plot <- function(plot, filename) {
  ggsave(
    filename = file.path(output_path, filename),
    plot = plot,
    width = 10,
    height = 8,
    dpi = 300
  )
}


# Summary statistics ----

## Diabetic population ----

dim(diab_2019)[1] # 44377
dim(diab_2021)[1] # 40573

## Amputation ----

t(rbind("2019 N" = table(diab_2019$amputated), "2019 %" = round(table(diab_2019$amputated)/sum(table(diab_2019$amputated)), 4)*100,
        "2021 N" = table(diab_2021$amputated), "2021 %" = round(table(diab_2021$amputated)/sum(table(diab_2021$amputated)), 4)*100))

## Age ----

t(rbind("2019 N" = table(diab_2019$cl_age), "2019 %" = round(table(diab_2019$cl_age)/sum(table(diab_2019$cl_age)), 4)*100,
        "2021 N" = table(diab_2021$cl_age), "2021 %" = round(table(diab_2021$cl_age)/sum(table(diab_2021$cl_age)), 4)*100))

## Sex ----

t(rbind("2019 N" = table(diab_2019$males), "2019 %" = round(table(diab_2019$males)/sum(table(diab_2019$males)), 4)*100,
        "2021 N" = table(diab_2021$males), "2021 %" = round(table(diab_2021$males)/sum(table(diab_2021$males)), 4)*100))

## Race ----

t(rbind("2019 N" = table(diab_2019$race), "2019 %" = round(table(diab_2019$race)/sum(table(diab_2019$race)), 4)*100,
        "2021 N" = table(diab_2021$race), "2021 %" = round(table(diab_2021$race)/sum(table(diab_2021$race)), 4)*100))

## Severity ----

t(rbind("2019 N" = table(diab_2019$severe), "2019 %" = round(table(diab_2019$severe)/sum(table(diab_2019$severe)), 4)*100,
        "2021 N" = table(diab_2021$severe), "2021%" = round(table(diab_2021$severe)/sum(table(diab_2021$severe)), 4)*100))

## Risk ----

t(rbind("2019 N" = table(diab_2019$risky), "2019 %" = round(table(diab_2019$risky)/sum(table(diab_2019$risky)), 4)*100,
        "2021 N" = table(diab_2021$risky), "2021 %" = round(table(diab_2021$risky)/sum(table(diab_2021$risky)), 4)*100))


# Amputation rate ----

## By hospital ----

# 2019
amp_hosp_2019 <- diab_2019 %>%
  group_by(ny_hosp_id) %>%
  summarize(amputation_rate = mean(amputated, na.rm = TRUE) * 100,
            amputated_patients = sum(amputated, na.rm = TRUE)) %>%
  rename(hospital_id = ny_hosp_id) %>%
  mutate(amputation_rate = round(amputation_rate, 2)) %>%
  mutate(log_amputated_patients = log(amputated_patients)) %>%
  as.data.frame()

# 2021
amp_hosp_2021 <- diab_2021 %>%
  group_by(ny_hosp_id) %>%
  summarize(amputation_rate = mean(amputated, na.rm = TRUE) * 100,
            amputated_patients = sum(amputated, na.rm = TRUE)) %>%
  rename(hospital_id = ny_hosp_id) %>%
  mutate(amputation_rate = round(amputation_rate, 2)) %>%
  mutate(log_amputated_patients = log(amputated_patients)) %>%
  as.data.frame()

# Comparison
round(cbind("2019" = stat.desc(amp_hosp_2019$amputation_rate), "2021" = stat.desc(amp_hosp_2021$amputation_rate)), 2)[c(4:6, 8:10, 12:14),]

tail(amp_hosp_2019 %>% arrange(amputation_rate))
tail(amp_hosp_2021 %>% arrange(amputation_rate))

tail(amp_hosp_2019 %>% arrange(log(amputated_patients)))
tail(amp_hosp_2021 %>% arrange(log(amputated_patients)))

hospital_boxplot <- ggplot() +
  geom_boxplot(data = amp_hosp_2019, aes(y = amputation_rate, x = factor(2019)), fill = "blue", alpha = 0.6) +
  geom_boxplot(data = amp_hosp_2021, aes(y = amputation_rate, x = factor(2021)), fill = "#FF7F0E", alpha = 0.6) +
  labs(
    title = "Distribution of Amputation Rates by Hospital (2019 vs. 2021)",
    x = "Year",
    y = "Amputation Rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  coord_cartesian(ylim = c(0, 35))

hospital_boxplot_file <- file.path(output_path, "hospital_amputation_boxplot.png")
ggsave(hospital_boxplot_file, plot = hospital_boxplot, width = 8, height = 6, dpi = 300)

hospital_boxplot


## By county ----

# 2019
amp_county_2019 <- diab_2019 %>%
  group_by(ny_county) %>%
  summarize(amputation_rate = mean(amputated) * 100,
            amputated_patients = sum(amputated)) %>%
  rename(county_id = ny_county) %>%
  mutate(amputation_rate = round(amputation_rate, 2)) %>%
  mutate(log_amputated_patients = log(amputated_patients)) %>%
  as.data.frame()

# 2021
amp_county_2021 <- diab_2021 %>%
  group_by(ny_county) %>%
  summarize(amputation_rate = mean(amputated, na.rm = TRUE) * 100,
            amputated_patients = sum(amputated, na.rm = TRUE)) %>%
  rename(county_id = ny_county) %>%
  mutate(amputation_rate = round(amputation_rate, 2)) %>%
  mutate(log_amputated_patients = log(amputated_patients)) %>%
  as.data.frame()

# Comparison
round(cbind("2019" = stat.desc(amp_county_2019$amputation_rate), "2021" = stat.desc(amp_county_2021$amputation_rate)), 2)[c(4:6, 8:10, 12:14),]

tail(amp_county_2019 %>% arrange(amputation_rate))
tail(amp_county_2021 %>% arrange(amputation_rate))

tail(amp_county_2019 %>% arrange(log(amputated_patients)))
tail(amp_county_2021 %>% arrange(log(amputated_patients)))

county_boxplot <- ggplot() +
  geom_boxplot(data = amp_county_2019, aes(y = amputation_rate, x = factor(2019)), fill = "blue", alpha = 0.6) +
  geom_boxplot(data = amp_county_2021, aes(y = amputation_rate, x = factor(2021)), fill = "#FF7F0E", alpha = 0.6) +
  labs(
    title = "Distribution of Amputation Rates by County (2019 vs. 2021)",
    x = "Year",
    y = "Amputation Rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  coord_cartesian(ylim = c(0, 35))

county_boxplot_file <- file.path(output_path, "county_amputation_boxplot.png")
ggsave(county_boxplot_file, plot = county_boxplot, width = 8, height = 6, dpi = 300)

county_boxplot


# Maps by county ----

amp_county_fips_2019 <- merge(amp_county_2019, county_id_label, by.x = "county_id", by.y = "County")
amp_county_fips_2021 <- merge(amp_county_2021, county_id_label, by.x = "county_id", by.y = "County")


## Amputation rate ----

# Distribution
hist(amp_county_fips_2019$amputation_rate, breaks = 15, 
     xlab = "Amputation Rate", ylab = "Count",
     main = "Distribution of Amputation Rate in NY Counties - 2019",
     ylim = c(0, 13), xlim = c(0, 30))

hist(amp_county_fips_2021$amputation_rate, breaks = 15, 
     xlab = "Diabetes Rate", ylab = "Count",
     main = "Distribution of Diabetes Rate in NY Counties - 2021",
     ylim = c(0, 13), xlim = c(0, 30))


# 2019
ar19 <- plot_usmap(regions = "counties",
                   include = "NY",
                   data = amp_county_fips_2019,
                   values = "amputation_rate") +
  scale_fill_viridis() +
  theme(legend.position = "bottom",
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Amputation Rate in NY Counties - 2019",
       caption = "Source: Hospital Inpatient Discharges (SPARCS De-Identified) 2017",
       fill = "Amputation Rate") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))

amp_county_fips_2019 %>% arrange(amputation_rate)

# 2021
ar21 <- plot_usmap(regions = "counties",
                   include = "NY",
                   data = amp_county_fips_2021,
                   values = "amputation_rate") +
  scale_fill_viridis() +
  theme(legend.position = "bottom",
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Amputation Rate in NY Counties - 2021",
       caption = "Source: Hospital Inpatient Discharges (SPARCS De-Identified) 2017",
       fill = "Amputation Rate") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))

amp_county_fips_2021 %>% arrange(amputation_rate)

grid.arrange(ar19, ar21, ncol = 2)

combined_maps_file <- file.path(output_path, "amputation_rate_maps_2019_2021.png")
ggsave(combined_maps_file, plot = arrangeGrob(ar19, ar21, ncol = 2),
       width = 14, height = 7, dpi = 300)


## Log of amputated patients ----

# Distribution
hist(amp_county_fips_2019$amputated_patients, breaks = 8, 
     xlab = "Amputated Patients", ylab = "Count",
     main = "Distribution of Amputated Patients in NY Counties - 2019")

hist(amp_county_fips_2021$amputated_patients, breaks = 8, 
     xlab = "Amputated Patients", ylab = "Count",
     main = "Distribution of Amputated Patients in NY Counties - 2021")
# NOTE: It is skewed, let's use the logarithm instead.

# Distribution of the logarithm
hist(amp_county_fips_2019$log_amputated_patients, breaks = 8, 
     xlab = "Log of Amputated Patients", ylab = "Count",
     main = "Distribution of Log Amputated Patients in NY Counties - 2019")

hist(amp_county_fips_2021$log_amputated_patients, breaks = 8, 
     xlab = "Log of Amputated Patients", ylab = "Count",
     main = "Distribution of Log Amputated Patients in NY Counties - 2021")


# 2019
ap19 <- plot_usmap(regions = "counties",
                   include = "NY",
                   data = amp_county_fips_2019,
                   values = "log_amputated_patients") +
  scale_fill_viridis() +
  theme(legend.position = "bottom",
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of Log Amputated Patients in NY Counties - 2019",
       caption = "Source: Hospital Inpatient Discharges (SPARCS De-Identified) 2017",
       fill = "Logarithm of the number of amputated patients") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))

# 2021
ap21 <- plot_usmap(regions = "counties",
                   include = "NY",
                   data = amp_county_fips_2021,
                   values = "log_amputated_patients") +
  scale_fill_viridis() +
  theme(legend.position = "bottom",
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of Log Amputated Patients in NY Counties - 2021",
       caption = "Source: Hospital Inpatient Discharges (SPARCS De-Identified) 2017",
       fill = "Logarithm of the number of amputated patients") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))

grid.arrange(ap19, ap21, ncol = 2)

combined_maps_file <- file.path(output_path, "log_amputated_maps_2019_2021.png")
ggsave(combined_maps_file, plot = arrangeGrob(ap19, ap21, ncol = 2),
       width = 14, height = 7, dpi = 300)

