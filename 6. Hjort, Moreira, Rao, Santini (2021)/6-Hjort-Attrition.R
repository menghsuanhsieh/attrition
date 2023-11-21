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
result_table_hjort <- data.table(outcome = c("Adopted"), no_sfe = NA_real_, with_sfe = NA_real_, attrition = NA_real_)

# data -----------------
final_data <- fread("raw data/policy-adoption experiment/policyadoption_raw.csv")

# ITT ------------------
implemented1 <- lm(implemented1 ~ treatment_assignment, data = final_data)
summary(implemented1)

result_table_hjort[1,2] <- implemented1$coefficients[2]

implemented_with_Sfe <- lm(implemented1 ~ treatment_assignment + factor(munic_id), data = final_data)
summary(implemented_with_Sfe)

result_table_hjort[1,3] <- implemented_with_Sfe$coefficients[2]

result_table_hjort[1,4] <- table(is.na(final_data$implemented1))[2] / (table(is.na(final_data$implemented1))[1] + table(is.na(final_data$implemented1))[2]) * 100

# save point estimates results ----------
fwrite(result_table_hjort, "hjort-main-results.csv")

