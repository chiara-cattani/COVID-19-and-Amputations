###------------------------------###
###   Indirect Standardization   ###
###------------------------------###


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


# Indirect standardization ----

funnel_plot <- function(
    title = "",
    rate,
    rate_se = NULL,
    names = NULL,
    names_outliers = 0,
    in_colour = 0,
    plot_names = 0,
    colour_var = NULL,
    colour_values = c("indianred2", "grey"),
    unit = 100,
    population,
    binary = 1,
    p_mean = "weighted",
    p_se = "weighted",
    alpha_1 = 0.05,
    alpha_2 = 0.002,
    filename = "",
    graph = "",
    pdf_height = 4.5,
    pdf_width = 7,
    dot_size = 0.9,
    low = NULL,
    high = NULL,
    ylab = "%",
    xlab = "N",
    selectlab = "Selected",
    selectlev = c("Yes", "No"),
    dfout = ""
) {
  number <- population # X-axis
  
  if (binary == 1) {
    p <- rate / unit # Y-axis
    p.se <- sqrt((p * (1 - p)) / number)
  } else {
    p <- rate
    if (p_se == "unweighted") {
      p.se <- mean(rate_se) # Unweighted
    } else if (p_se == "weighted") {
      p.se <- weighted.mean(rate_se, number, na.rm = TRUE) # Weighted by N
    } else {
      p.se <- p_se # External Value
    }
  }
  
  if (!is.null(colour_var) == TRUE) {
    cyl <- colour_var
  }
  
  if (!is.null(names) == TRUE) {
    if (!is.null(colour_var) == TRUE) {
      df <- data.frame(p, number, p.se, names, cyl)
      names(df) <- c("p", "number", "p.se", "names", "cyl")
      df <- df[order(-df$cyl), ] # Put selection in foreground
    } else {
      df <- data.frame(p, number, p.se, names)
      names(df) <- c("p", "number", "p.se", "names")
    }
  } else {
    if (!is.null(colour_var) == TRUE) {
      df <- data.frame(p, number, p.se, cyl)
      df <- df[order(-df$cyl), ] # Put selection in foreground
    } else {
      df <- data.frame(p, number, p.se)
    }
  }
  
  # Common effect (fixed effect model)
  if (p_mean == "unweighted") {
    p.fem <- mean(p) # Unweighted
  } else if (p_mean == "weighted") {
    p.fem <- weighted.mean(p, number, na.rm = TRUE) # Weighted by N
  } else if (p_mean == "meta") {
    p.fem <- weighted.mean(p, 1 / p.se^2, na.rm = TRUE) # Meta-Analysis
  } else {
    p.fem <- as.numeric(p_mean)
    if (binary == 1) {
      p.fem <- p.fem / unit
    }
  }
  
  # Standard error (fixed effect model)
  if (binary == 1) {
    se.fem <- sqrt(p.fem * (1 - p.fem))
  } else {
    se.fem <- as.numeric(p_se)
  }
  
  p.fem <- ifelse(!is.na(p.fem), p.fem, 0)
  se.fem <- ifelse(!is.na(se.fem), se.fem, 0)
  
  # Lower and upper limits for 95% and 99.8% CI, based on FEM estimator
  z_value1 <- qnorm(1 - alpha_1 / 2)
  z_value2 <- qnorm(1 - alpha_2 / 2)
  
  # Plot CIs
  max_step <- round((max(number) - min(number)) * 0.005)
  if (max_step == 0) {
    max_step <- 1
  }
  
  number.seq <- seq(min(number), max(number), max_step)
  number.ll95 <- p.fem - z_value1 * (se.fem / sqrt(number.seq))
  number.ul95 <- p.fem + z_value1 * (se.fem / sqrt(number.seq))
  number.ll99 <- p.fem - z_value2 * (se.fem / sqrt(number.seq))
  number.ul99 <- p.fem + z_value2 * (se.fem / sqrt(number.seq))
  
  dfCI <- data.frame(number.ll95, number.ul95, number.ll99, number.ul99, number.seq, p.fem)
  
  # Determine outliers
  df$outlier <- 0
  if (names_outliers > 0) {
    df$outlier <- ifelse(df$p < (p.fem - z_value2 * (se.fem / sqrt(df$number))), -1, df$outlier)
    df$outlier <- ifelse(df$p > (p.fem + z_value2 * (se.fem / sqrt(df$number))), 1, df$outlier)
    value_outlier <- 1
  } else {
    value_outlier <- 0
  }
  
  # Rescale
  if (binary == 1) {
    df$p <- df$p * unit
    
    dfCI$number.ll95 <- dfCI$number.ll95 * unit
    dfCI$number.ul95 <- dfCI$number.ul95 * unit
    dfCI$number.ll99 <- dfCI$number.ll99 * unit
    dfCI$number.ul99 <- dfCI$number.ul99 * unit
    dfCI$p.fem <- dfCI$p.fem * unit
    
    p.fem <- p.fem * unit
  }
  
  if (is.null(low) == TRUE) {
    low <- min(dfCI$number.ll99, df$p)
  }
  if (is.null(high) == TRUE) {
    high <- max(dfCI$number.ul99, df$p)
  }
  
  # Plot
  fp <- ggplot(df, aes(number, p), na.rm = TRUE) +
    geom_line(aes(x = number.seq, y = number.ll95, linetype = as.factor(c(2))), data = dfCI, na.rm = TRUE) +
    geom_line(aes(x = number.seq, y = number.ul95, linetype = as.factor(c(2))), data = dfCI, na.rm = TRUE) +
    geom_line(aes(x = number.seq, y = number.ll99, linetype = as.factor(c(1))), data = dfCI, na.rm = TRUE) +
    geom_line(aes(x = number.seq, y = number.ul99, linetype = as.factor(c(1))), data = dfCI, na.rm = TRUE) +
    geom_hline(aes(yintercept = p.fem), data = dfCI, na.rm = TRUE) +
    scale_linetype_manual("Probability", values = c(1, 2), labels = c(eval(1 - alpha_2), eval(1 - alpha_1))) +
    scale_y_continuous(limits = c(low, high)) +
    xlab(xlab) + ylab(ylab) +
    theme_bw()
  
  # Add dots, names, and colours
  if (plot_names != 0 & !is.null(names)) { # Names outside and coloured dots inside
    if (nrow(df[abs(df$outlier) != value_outlier, ]) > 0) {
      if (in_colour != 0) {
        fp <- fp + geom_point(data = df[abs(df$outlier) != value_outlier, ], aes(colour = factor(cyl)), shape = 20, size = dot_size, na.rm = TRUE)
      } else {
        fp <- fp + geom_point(data = df[abs(df$outlier) != value_outlier, ], shape = 20, size = dot_size, na.rm = TRUE)
      }
      fp <- fp + scale_colour_manual(selectlab, values = colour_values, labels = selectlev)
    }
    
    if (nrow(df[abs(df$outlier) == value_outlier, ]) > 0) {
      if (!is.null(colour_var) == TRUE) {
        fp <- fp + geom_text(data = df[abs(df$outlier) == value_outlier, ], aes(number, p, label = names, colour = factor(cyl)), size = dot_size * 1.5, inherit.aes = FALSE, na.rm = TRUE)
      } else {
        fp <- fp + geom_text(data = df[abs(df$outlier) == value_outlier, ], aes(number, p, label = names), size = dot_size * 1.5, inherit.aes = FALSE, na.rm = TRUE)
      }
    }
  } else {
    if (!is.null(colour_var) == TRUE) {
      fp <- fp + geom_point(data = df, aes(colour = factor(cyl)), shape = 20, size = dot_size, na.rm = TRUE)
      fp <- fp + scale_colour_manual(selectlab, values = colour_values, labels = selectlev)
    } else {
      fp <- fp + geom_point(data = df, shape = 20, size = dot_size, na.rm = TRUE)
    }
  }
  
  if (filename != "") {
    pdf(paste(filename, ".pdf", sep = ""), width = pdf_width, height = pdf_height)
    print(fp)
    dev.off()
    
    png(paste(filename, ".png", sep = ""), width = pdf_width * 60, height = pdf_height * 60)
    print(fp)
    dev.off()
  }
  
  fp <- fp + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  
  if (graph != "") {
    assign(graph, fp, envir = .GlobalEnv)
  }
  
  rm(p, number, p.se, names, number.seq, dfCI, fp)
  
  if (dfout != "") {
    assign(dfout, df, envir = .GlobalEnv)
  } else {
    rm(df)
  }
  
  if (!is.null(colour_var) == TRUE) {
    rm(cyl)
  }
}



## 2019 ----

DLogit <- glm(amputated ~ cl_age + males, family = binomial("logit"), data = diab_2019)
round(cbind(Beta = coef(DLogit), confint(DLogit), P = coef(summary(DLogit))[,4], exp(cbind(OR = coef(DLogit), confint(DLogit)))), 4)

# Predicted probabilities
diab_2019$p <- predict(DLogit, newdata = diab_2019, type = "response")         

# Overall amputation rate 
Ybar <- sum(diab_2019$amputated)/dim(diab_2019)[1]                 
round(Ybar*100, 2)


### Hospital ----

hosp_names <- unique(diab_2019$ny_hosp_id)

# Crude and indirect standardized rates for all hospitals
for (i in 1:length(hosp_names)) {
  
  label_target_group <- hosp_names[i]
  
  # Observed amputation rate
  O1  <- mean(diab_2019[diab_2019$ny_hosp_id == hosp_names[i], c("amputated")])
  # Expected amputation rate
  E1  <- mean(diab_2019[diab_2019$ny_hosp_id == hosp_names[i], c("p")])  
  # Number of patients
  N1  <- dim(diab_2019[diab_2019$ny_hosp_id == hosp_names[i],])[1]    
  # Predicted probability
  P1j <- diab_2019[diab_2019$ny_hosp_id == hosp_names[i], c("p")]   
  
  # Risk adjusted rate
  rar1      <- Ybar*(O1/E1)  
  rar1.se   <- sqrt((Ybar/E1)^2 *(1/N1)^2 * sum(P1j*(1-P1j))) 
  rar1.ll95 <- rar1 - 1.96 * rar1.se                                   
  rar1.ul95 <- rar1 + 1.96 * rar1.se                                    
  
  output <- data.frame(label_target_group, round(O1*100, 2), round(rar1*100, 2), N1, round(rar1.ll95*100, 2), round(rar1.ul95*100, 2))
  names(output) <- c("Hospital", "cr_2019", "isr_2019", "N_2019", "Lower 95% CI_2019", "Upper 95% CI_2019")  
  
  if (i==1) {isr_output_hospital_2019 <- output} else {isr_output_hospital_2019 <- rbind(isr_output_hospital_2019, output)}
  rm(output)
  
}

tail(isr_output_hospital_2019 %>% arrange(isr_2019))

# Compare to average
isr_output_hospital_2019$diff_average <- isr_output_hospital_2019$isr_2019 - Ybar*100
tail(isr_output_hospital_2019 %>% arrange(diff_average))

#### Funnel plot ----

funnel_plot(title = "(a) Indirectly Standardized Amputation Rates at Hospital Level - 2019", 
            names = isr_output_hospital_2019$Hospital,  
            names_outliers = 1,                          
            plot_names = 0,                              
            in_colour = 1,                               
            colour_var = ,                               
            colour_values = ,                           
            rate = isr_output_hospital_2019$isr_2019,   
            unit = 100,                                   
            population = isr_output_hospital_2019$N_2019, 
            binary = 1,                              
            p_mean = "weighted",                    
            filename = "",                           
            graph = "isr_hosp_2019",                
            pdf_height = 3.5,                        
            pdf_width = 6,                          
            ylab = "Amputation Rate",               
            xlab = "Diabetic patients",              
            selectlab = "County",                   
            selectlev = c("1","2"),                 
            dot_size = 1.5,                          
            dfout = "dfout")                         

isr_hosp_2019
png(file.path(output_path, "funnel_plot_hospital_2019.png"), width = 1800, height = 1200, res = 300)
print(isr_hosp_2019)
dev.off()


### County ----

county_names <- unique(diab_2019$ny_county)

# Crude and indirect standardized rates for all counties
for (i in 1:length(county_names)) {
  
  label_target_group <- county_names[i]
  
  # Observed amputation rate
  O1  <- mean(diab_2019[diab_2019$ny_county == county_names[i], c("amputated")], na.rm = TRUE)
  # Expected amputation rate
  E1  <- mean(diab_2019[diab_2019$ny_county == county_names[i], c("p")], na.rm = TRUE)  
  # Number of patients
  N1  <- dim(diab_2019[diab_2019$ny_county == county_names[i],])[1]    
  # Predicted probability
  P1j <- diab_2019[diab_2019$ny_county == county_names[i], c("p")]   
  
  # Risk adjusted rate
  rar1      <- Ybar*(O1/E1)  
  rar1.se   <- sqrt((Ybar/E1)^2 *(1/N1)^2 * sum(P1j*(1-P1j), na.rm = TRUE)) 
  rar1.ll95 <- rar1 - 1.96 * rar1.se                                   
  rar1.ul95 <- rar1 + 1.96 * rar1.se                                    
  
  output <- data.frame(label_target_group, round(O1*100, 2), round(rar1*100, 2), N1, round(rar1.ll95*100, 2), round(rar1.ul95*100, 2))
  names(output) <- c("county_id", "cr_2019", "isr_2019", "N_2019", "Lower 95% CI_2019", "Upper 95% Cr_2019")  
  
  if (i==1) {isr_output_county_2019 <- output} else {isr_output_county_2019 <- rbind(isr_output_county_2019, output)}
  rm(output)
  
}

tail(isr_output_county_2019 %>% arrange(isr_2019))


#### Funnel plot ----

funnel_plot(title = "Indirectly Standardized Amputation Rates at County Level - 2019", 
            names = isr_output_county_2019$county_id,    
            names_outliers = 1,                          
            plot_names = 0,                              
            in_colour = 1,                                
            colour_var = ,                            
            colour_values = ,                        
            rate = isr_output_county_2019$isr_2019,    
            unit = 100,                               
            population = isr_output_county_2019$N_2019, 
            binary = 1,                            
            p_mean = "weighted",                   
            filename = "",                         
            graph = "isr_county_2019",             
            pdf_height = 3.5,                       
            pdf_width = 6,                          
            ylab = "Amputation Rate",              
            xlab = "Diabetic patients",            
            selectlab = "County",                  
            selectlev = c("1","2"),                 
            dot_size = 1.5,                          
            dfout = "dfout")                         

isr_county_2019
png(file.path(output_path, "funnel_plot_county_2019.png"), width = 1800, height = 1200, res = 300)
print(isr_county_2019)
dev.off()


## 2021 ----

DLogit <- glm(amputated ~ cl_age + males, family = binomial("logit"), data = diab_2021)
round(cbind(Beta = coef(DLogit), confint(DLogit), P = coef(summary(DLogit))[,4], exp(cbind(OR = coef(DLogit), confint(DLogit)))), 4)

# Predicted probabilities
diab_2021$p <- predict(DLogit, newdata = diab_2021, type = "response")         

# Overall amputation rate 
Ybar <- sum(diab_2021$amputated, na.rm = TRUE)/dim(diab_2021)[1]                 
round(Ybar*100, 2)


### Hospital ----

hosp_names <- unique(diab_2021$ny_hosp_id)

# Crude and indirect standardized rates for all hospitals
for (i in 1:length(hosp_names)) {
  
  label_target_group <- hosp_names[i]
  
  # Observed amputation rate
  O1  <- mean(diab_2021[diab_2021$ny_hosp_id == hosp_names[i], c("amputated")], na.rm = TRUE)
  # Expected amputation rate
  E1  <- mean(diab_2021[diab_2021$ny_hosp_id == hosp_names[i], c("p")], na.rm = TRUE)  
  # Number of patients
  N1  <- dim(diab_2021[diab_2021$ny_hosp_id == hosp_names[i],])[1]    
  # Predicted probability
  P1j <- diab_2021[diab_2021$ny_hosp_id == hosp_names[i], c("p")]   
  
  # Risk adjusted rate
  rar1      <- Ybar*(O1/E1)  
  rar1.se   <- sqrt((Ybar/E1)^2 *(1/N1)^2 * sum(P1j*(1-P1j), na.rm = TRUE)) 
  rar1.ll95 <- rar1 - 1.96 * rar1.se                                   
  rar1.ul95 <- rar1 + 1.96 * rar1.se                                    
  
  output <- data.frame(label_target_group, round(O1*100, 2), round(rar1*100, 2), N1, round(rar1.ll95*100, 2), round(rar1.ul95*100, 2))
  names(output) <- c("Hospital", "cr_2021", "isr_2021", "N_2021", "Lower 95% CI_2021", "Upper 95% CI_2021")  
  
  if (i==1) {isr_output_hospital_2021 <- output} else {isr_output_hospital_2021 <- rbind(isr_output_hospital_2021, output)}
  rm(output)
  
}

tail(isr_output_hospital_2021 %>% arrange(isr_2021))

# Compare to average
isr_output_hospital_2021$diff_average <- isr_output_hospital_2021$isr_2021 - Ybar*100
tail(isr_output_hospital_2021 %>% arrange(diff_average))


#### Funnel plot ----

funnel_plot(title = "(b) Indirectly Standardized Amputation Rates at Hospital Level - 2021", 
            names = isr_output_hospital_2021$Hospital,    
            names_outliers = 1,                         
            plot_names = 0,                              
            in_colour = 1,                               
            colour_var = ,                                
            colour_values = ,                            
            rate = isr_output_hospital_2021$isr_2021,     
            unit = 100,                                  
            population = isr_output_hospital_2021$N_2021, 
            binary = 1,                              
            p_mean = "weighted",                  
            filename = "",                        
            graph = "isr_hosp_2021",            
            pdf_height = 3.5,                     
            pdf_width = 6,                          
            ylab = "Amputation Rate",                
            xlab = "Diabetic patients",            
            selectlab = "County",                   
            selectlev = c("1","2"),                 
            dot_size = 1.5,                         
            dfout = "dfout")                        

isr_hosp_2021
png(file.path(output_path, "funnel_plot_hospital_2021.png"), width = 1800, height = 1200, res = 300)
print(isr_hosp_2021)
dev.off()


### County ----

county_names <- unique(diab_2021$ny_county)

# Crude and indirect standardized rates for all counties
for (i in 1:length(county_names)) {
  
  label_target_group <- county_names[i]
  
  # Observed amputation rate
  O1  <- mean(diab_2021[diab_2021$ny_county == county_names[i], c("amputated")], na.rm = TRUE)
  # Expected amputation rate
  E1  <- mean(diab_2021[diab_2021$ny_county == county_names[i], c("p")], na.rm = TRUE)  
  # Number of patients
  N1  <- dim(diab_2021[diab_2021$ny_county == county_names[i],])[1]    
  # Predicted probability
  P1j <- diab_2021[diab_2021$ny_county == county_names[i], c("p")]   
  
  # Risk adjusted rate
  rar1      <- Ybar*(O1/E1)  
  rar1.se   <- sqrt((Ybar/E1)^2 *(1/N1)^2 * sum(P1j*(1-P1j), na.rm = TRUE)) 
  rar1.ll95 <- rar1 - 1.96 * rar1.se                                   
  rar1.ul95 <- rar1 + 1.96 * rar1.se                                    
  
  output <- data.frame(label_target_group, round(O1*100, 2), round(rar1*100, 2), N1, round(rar1.ll95*100, 2), round(rar1.ul95*100, 2))
  names(output) <- c("county_id", "cr_2021", "isr_2021", "N_2021", "Lower 95% CI_2021", "Upper 95% Cr_2021")  
  
  if (i==1) {isr_output_county_2021 <- output} else {isr_output_county_2021 <- rbind(isr_output_county_2021, output)}
  rm(output)
  
}

tail(isr_output_county_2021 %>% arrange(isr_2021))


#### Funnel plot ----

funnel_plot(title = "Indirectly Standardized Amputation Rates at County Level - 2021", 
            names = isr_output_county_2021$county_id,    
            names_outliers = 1,                          
            plot_names = 0,                              
            in_colour = 1,                              
            colour_var = ,                               
            colour_values = ,                            
            rate = isr_output_county_2021$isr_2021,     
            unit = 100,                                   
            population = isr_output_county_2021$N_2021,
            binary = 1,                          
            p_mean = "weighted",                     
            filename = "",                         
            graph = "isr_county_2021",              
            pdf_height = 3.5,                      
            pdf_width = 6,                          
            ylab = "Amputation Rate",                
            xlab = "Diabetic patients",             
            selectlab = "County",                    
            selectlev = c("1","2"),                
            dot_size = 1.5,                          
            dfout = "dfout")                       

isr_county_2021
png(file.path(output_path, "funnel_plot_county_2021.png"), width = 1800, height = 1200, res = 300)
print(isr_county_2021)
dev.off()


## Comparisons ----

### Hospital ----

isr_output_hospital <- merge(isr_output_hospital_2019, isr_output_hospital_2021, by = "Hospital")
isr_output_hospital$diff <- isr_output_hospital$isr_2021 - isr_output_hospital$isr_2019

table(isr_output_hospital$diff < 0) 
head(isr_output_hospital %>% arrange(diff), 4)

table(isr_output_hospital$diff == 0) 
isr_output_hospital[isr_output_hospital$diff == 0, ]

table(isr_output_hospital$diff > 0) 
tail(isr_output_hospital %>% arrange(diff), 6)


### County ----

isr_output_county <- merge(isr_output_county_2019, isr_output_county_2021, by = "county_id")
isr_output_county$diff <- isr_output_county$isr_2021 - isr_output_county$isr_2019

isr_county_fips <- merge(isr_output_county, county_id_label, by.x = "county_id", by.y = "County")

table(isr_county_fips$diff < 0) 
head(isr_county_fips %>% arrange(diff), 4)

table(isr_county_fips$diff == 0) 
isr_county_fips[isr_county_fips$diff == 0, ]

table(isr_county_fips$diff > 0) 
tail(isr_county_fips %>% arrange(diff), 4)

