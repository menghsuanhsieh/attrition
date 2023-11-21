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
result_table_attanasio <- data.table(outcome = c("Hours Worked"), no_sfe = NA_real_, with_sfe = NA_real_, attrition = NA_real_)

# data ------------------
data <- as.data.table(read_dta("data/AER-2015-0183_data_appendix/data/measures_extended.dta"))
data[central == 1, stratum := 1]
data[oriental == 1 & is.na(stratum)==TRUE, stratum := 2]
data[is.na(stratum)==TRUE, stratum := 3]

# regression outcome: hours worked + attrition rate calculation -------------
hoursworked1 <- lm(totwklyhrs_mo1_zeros ~ treat + age_mo1 + I(age_mo1^2) + edu_yrs_mo1 + nokids2 + nokids3 + nokids4 + nokids5 + nokids6plus + factor(ent_fu), data = data)

result_table_attanasio[1,2] <- hoursworked1$coefficients[2]

hoursworked_with_sfe <- lm(totwklyhrs_mo1_zeros ~ treat + age_mo1 + I(age_mo1^2) + edu_yrs_mo1 + nokids2 + nokids3 + nokids4 + nokids5 + nokids6plus + factor(ent_fu) + factor(stratum), data = data)

result_table_attanasio[1,3] <- hoursworked_with_sfe$coefficients[2]

result_table_attanasio[1,4] <- table(is.na(data$totwklyhrs_mo1_zeros))[2] / (table(is.na(data$totwklyhrs_mo1_zeros))[1] + table(is.na(data$totwklyhrs_mo1_zeros))[2]) * 100

# save point estimates results ----------
fwrite(result_table_attanasio, "attanasio-main-results.csv")
