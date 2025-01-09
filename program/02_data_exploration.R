###----------------------###
###   Data Exploration   ###
###----------------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))

output_path <- normalizePath("../output", winslash = "/")
if (!dir.exists(output_path)) {
  dir.create(output_path)
}


# Packages ----

library(gplots)
library(corrplot)


# Load data ----

load("../data/diab_2019.Rda")
load("../data/diab_2021.Rda")


# Exploratory Analysis ----

plot_chi_square_no_legend <- function(data, title) {
  corrplot(
    data,
    is.cor = FALSE,
    title = title,
    mar = c(0, 0, 2, 0),
    col = viridis::viridis(200),
    tl.cex = 1.2,         
    cl.cex = 1.2,         
    tl.col = "black",    
    cl.pos = "n",       
    yaxt = "n"          
  )
}

analyze_variable <- function(var_2019, var_2021, var_name, xlab, ylab) {
  
  # 2019 Analysis
  cat(paste("\nAnalysis for", var_name, "2019:\n"))
  proportion_2019 <- 100 * round(prop.table(table(var = var_2019, amputated = diab_2019$amputated), 1), 4)
  print(proportion_2019)
  
  table_2019 <- table(var = var_2019, amputated = diab_2019$amputated)
  
  # Balloon plot for 2019
  png(file.path(output_path, paste0("balloonplot_", var_name, "_amputation_2019.png")), width = 1000, height = 600, res = 150)
  balloonplot(t(table_2019),
              main = paste("Amputation vs", gsub("_", " ", var_name), "(2019)"),
              xlab = xlab,
              ylab = ylab,
              label = TRUE,
              show.margins = FALSE)
  dev.off()
  
  chisq_2019 <- chisq.test(table_2019)
  print(chisq_2019)
  
  # 2021 Analysis
  
  cat(paste("\nAnalysis for", var_name, "2021:\n"))
  proportion_2021 <- 100 * round(prop.table(table(var = var_2021, amputated = diab_2021$amputated), 1), 4)
  print(proportion_2021)
  
  table_2021 <- table(var = var_2021, amputated = diab_2021$amputated)
  
  # Balloon plot for 2021
  
  png(file.path(output_path, paste0("balloonplot_", var_name, "_amputation_2021.png")), width = 800, height = 600, res = 150)
  balloonplot(t(table_2021),
              main = paste("Amputation vs", gsub("_", " ", var_name), "(2021)"),
              xlab = xlab,
              ylab = ylab,
              label = TRUE,
              show.margins = FALSE)
  dev.off()
  
  chisq_2021 <- chisq.test(table_2021)
  print(chisq_2021)
  
  # Combined Chi-Square Contribution Matrices Plot
  
  png(file.path(output_path, paste0("chi_square_residuals_combined_", var_name, ".png")), width = 3000, height = 1500, res = 300)
  par(mfrow = c(1, 2), bg = "white")
  plot_chi_square_no_legend((100 * chisq_2019$residuals^2 / chisq_2019$statistic),
                            title = paste("Contribution to Chi-Squared Statistic (2019) -", gsub("_", " ", var_name)))
  plot_chi_square_no_legend((100 * chisq_2021$residuals^2 / chisq_2021$statistic),
                            title = paste("Contribution to Chi-Squared Statistic (2021) -", gsub("_", " ", var_name)))
  dev.off()
  
}


## Age ----
analyze_variable(diab_2019$cl_age, diab_2021$cl_age, "Age", xlab = "Amputated", ylab = "Age Group")

## Gender ----
analyze_variable(diab_2019$males, diab_2021$males, "Gender", xlab = "Amputated", ylab = "Gender")

## Race ----
analyze_variable(diab_2019$race, diab_2021$race, "Race", xlab = "Amputated", ylab = "Race")

## Severity of diabetes ----
analyze_variable(diab_2019$severe, diab_2021$severe, "Severity of Diabetes", xlab = "Amputated", ylab = "Severity Level")

## Risk of death ----
analyze_variable(diab_2019$risky, diab_2021$risky, "Risk ofDeath", xlab = "Amputated", ylab = "Risk Level")



