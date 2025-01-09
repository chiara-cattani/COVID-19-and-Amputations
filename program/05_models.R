####-----------###
###   Models   ###
###------------###


# Prepare environment ----

rm(list = ls())
graphics.off()
setwd(file_path <- dirname(rstudioapi::getSourceEditorContext()$path))

output_path <- normalizePath("../output", winslash = "/")
if (!dir.exists(output_path)) {
  dir.create(output_path)
}


# Packages ----

library(geepack)
library(ggplot2)
library(gridExtra)
library(pROC)
library(cutpointr)
library(ROCR)


# Load data ----

load("../data/diab_2019.Rda")
load("../data/diab_2021.Rda")


# Prepare data ----

# Combine 2019 and 2021 data
diab_2019$year <- "2019"
diab_2021$year <- "2021"
diab_all <- rbind(diab_2019, diab_2021)

# Recode age groups
diab_all$recl_age <- diab_all$cl_age
diab_all$recl_age[diab_all$recl_age %in% c("0 to 17", "18 to 29")] <- "0 to 29"

# Convert relevant variables to factors
diab_all$race <- as.factor(diab_all$race)
diab_all$year <- as.factor(diab_all$year)
diab_all$ny_hosp_id <- as.factor(diab_all$ny_hosp_id)
diab_all$recl_age <- as.factor(diab_all$recl_age)
diab_all$risky <- as.factor(diab_all$risky)

# Save combined data
save(diab_all, file = "../data/diab_all.Rda")


# Logistic Regression Models ----

## Full Model ----

glm_full <- glm(amputated ~ recl_age + males + race + severe + risky + year, 
                data = diab_all, 
                family = binomial("logit"))
summary(glm_full)

## Reduced Model ----

glm_reduced <- glm(amputated ~ recl_age + males + race + year, 
                   data = diab_all, 
                   family = binomial("logit"))
summary(glm_reduced)


### Compare Models ----

comparison <- cbind(
  Full_Model_BIC = BIC(glm_full),
  Reduced_Model_BIC = BIC(glm_reduced),
  Full_Model_AIC = AIC(glm_full),
  Reduced_Model_AIC = AIC(glm_reduced)
)
comparison


# Generalized Estimating Equations (GEE) ----

## Exchangeable Correlation ----

gee_full <- geeglm(amputated ~ recl_age + males + race + severe + risky + year, 
                   data = na.omit(diab_all), 
                   id = ny_hosp_id, 
                   family = binomial("logit"), 
                   corstr = "exchangeable")
summary(gee_full)

gee_reduced <- geeglm(amputated ~ recl_age + males + race + year, 
                      data = na.omit(diab_all), 
                      id = ny_hosp_id, 
                      family = binomial("logit"), 
                      corstr = "exchangeable")
summary(gee_reduced)

## Independence Correlation ----

gee_full_ind <- geeglm(amputated ~ recl_age + males + race + severe + risky + year, 
                       data = na.omit(diab_all), 
                       id = ny_hosp_id, 
                       family = binomial("logit"), 
                       corstr = "independence")
summary(gee_full_ind)

gee_reduced_ind <- geeglm(amputated ~ recl_age + males + race + year, 
                          data = na.omit(diab_all), 
                          id = ny_hosp_id, 
                          family = binomial("logit"), 
                          corstr = "independence")
summary(gee_reduced_ind)


# ROC Curve and AUC ----

## Predictions
diab_all$p_full <- predict(glm_full, newdata = diab_all, type = "response")
diab_all$p_reduced <- predict(glm_reduced, newdata = diab_all, type = "response")

## Compute Performance
pr_full <- prediction(diab_all$p_full, diab_all$amputated)
pr_reduced <- prediction(diab_all$p_reduced, diab_all$amputated)

prf_full <- performance(pr_full, measure = "tpr", x.measure = "fpr")
prf_reduced <- performance(pr_reduced, measure = "tpr", x.measure = "fpr")

# Save ROC Curves
png(file.path(output_path, "ROC_Curve_Comparison.png"), width = 800, height = 600)
plot(prf_full, col = "red", lwd = 2, main = "ROC Curve Comparison")
plot(prf_reduced, col = "blue", add = TRUE, lwd = 2)
abline(a = 0, b = 1, lty = 2)
legend("bottomright", legend = c("Full Model", "Reduced Model"), col = c("red", "blue"), lty = 1, lwd = 2)
dev.off()

## Compute AUC
auc_full <- performance(pr_full, measure = "auc")@y.values[[1]]
auc_reduced <- performance(pr_reduced, measure = "auc")@y.values[[1]]
cat("AUC Full Model:", auc_full, "\n")
cat("AUC Reduced Model:", auc_reduced, "\n")

