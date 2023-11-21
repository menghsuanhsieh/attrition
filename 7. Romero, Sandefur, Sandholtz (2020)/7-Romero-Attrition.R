rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(ramify, fastDummies, doParallel, foreach, data.table, nbpMatching, sandwich, lmtest, haven, tidyverse, xtable, rstudioapi, readstata13)

# # working directory  # if running this file alone, uncomment this + next line
# setwd(dirname(getSourceEditorContext()$path))

# result table
result_table_romero <- data.table(outcome = c("English","Math","Abstract","Composite","New Modules","Conceptual"), no_sfe = NA_real_, with_sfe = NA_real_, attrition = NA_real_)

# data ------------------
final_data <- as.data.table(read_dta("estimation data.dta"))

# english ---------------
english1 <- lm(IRT_English ~ treatment, data = final_data[originalrct==1], weights = W)
summary(english1)

result_table_romero[1,2] <- english1$coefficients[2]

english_with_sfe <- lm(IRT_English ~ treatment + factor(groupid_nopii), data = final_data[originalrct==1], weights = W)
summary(english_with_sfe)

result_table_romero[1,3] <- english_with_sfe$coefficients[2]

result_table_romero[1,4] <- table(is.na(final_data$IRT_English))[2] / (table(is.na(final_data$IRT_English))[1] + table(is.na(final_data$IRT_English))[2]) * 100

# math ---------------
math1 <- lm(IRT_math ~ treatment, data = final_data[originalrct==1], weights = W)
summary(math1)

result_table_romero[2,2] <- math1$coefficients[2]

math_with_sfe <- lm(IRT_math ~ treatment + factor(groupid_nopii), data = final_data[originalrct==1], weights = W)
summary(math_with_sfe)

result_table_romero[2,3] <- math_with_sfe$coefficients[2]

result_table_romero[2,4] <- table(is.na(final_data$IRT_math))[2] / (table(is.na(final_data$IRT_math))[1] + table(is.na(final_data$IRT_math))[2]) * 100

# abstract ---------------
abstract1 <- lm(IRT_Abstract ~ treatment, data = final_data[originalrct==1], weights = W)
summary(abstract1)

result_table_romero[3,2] <- abstract1$coefficients[2]

abstract_with_sfe <- lm(IRT_Abstract ~ treatment + factor(groupid_nopii), data = final_data[originalrct==1], weights = W)
summary(abstract_with_sfe)

result_table_romero[3,3] <- abstract_with_sfe$coefficients[2]

result_table_romero[3,4] <- table(is.na(final_data$IRT_Abstract))[2] / (table(is.na(final_data$IRT_Abstract))[1] + table(is.na(final_data$IRT_Abstract))[2]) * 100

# composite ---------------
composite1 <- lm(IRT_Composite ~ treatment, data = final_data[originalrct==1], weights = W)
summary(composite1)

result_table_romero[4,2] <- composite1$coefficients[2]

composite_with_sfe <- lm(IRT_Composite ~ treatment + factor(groupid_nopii), data = final_data[originalrct==1], weights = W)
summary(composite_with_sfe)

result_table_romero[4,3] <- composite_with_sfe$coefficients[2]

result_table_romero[4,4] <- table(is.na(final_data$IRT_Composite))[2] / (table(is.na(final_data$IRT_Composite))[1] + table(is.na(final_data$IRT_Composite))[2]) * 100

# new modules ---------------
nm1 <- lm(IRT_New ~ treatment, data = final_data[originalrct==1], weights = W)
summary(nm1)

result_table_romero[5,2] <- nm1$coefficients[2]

nm_with_sfe <- lm(IRT_New ~ treatment + factor(groupid_nopii), data = final_data[originalrct==1], weights = W)
summary(nm_with_sfe)

result_table_romero[5,3] <- nm_with_sfe$coefficients[2]

result_table_romero[5,4] <- table(is.na(final_data$IRT_New))[2] / (table(is.na(final_data$IRT_New))[1] + table(is.na(final_data$IRT_New))[2]) * 100

# conceptual ---------------
conceptual1 <- lm(IRT_Conceptual ~ treatment, data = final_data[originalrct==1], weights = W)
summary(conceptual1)

result_table_romero[6,2] <- conceptual1$coefficients[2]

conceptual_with_sfe <- lm(IRT_Conceptual ~ treatment + factor(groupid_nopii), data = final_data[originalrct==1], weights = W)
summary(conceptual_with_sfe)

result_table_romero[6,3] <- conceptual_with_sfe$coefficients[2]

result_table_romero[6,4] <- table(is.na(final_data$IRT_Conceptual))[2] / (table(is.na(final_data$IRT_Conceptual))[1] + table(is.na(final_data$IRT_Conceptual))[2]) * 100

# finally, attrition at midline:
result_table_romero$attrition <- (1 - 3493/3654) * 100 # from attrition.dta endline

# save point estimates results ----------
fwrite(result_table_romero, "romero-main-results.csv")

