rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(ramify, fastDummies, doParallel, foreach, data.table, nbpMatching, sandwich, lmtest, haven, tidyverse, xtable, rstudioapi)

# # working directory  # if running this file alone, uncomment this + next line
# setwd(dirname(getSourceEditorContext()$path))

result_table_abebe <- data.table(outcome = rep(c("Application rate","Cognitive","Noncognitive", "Experience"), each = 2), treatment = rep(c("incentive","wage"), times = 4), no_sfe = NA_real_, with_sfe = NA_real_, attrition = NA_real_)

# data -----------------
final_data <- as.data.table(read_dta("data/generated/MainExperiment_ForAnalysis.dta"))

# application rate + calculate attrition rate -------------
application1 <- lm(attended ~ incentive + wage, data = final_data)
summary(application1)

result_table_abebe[1,3] <- application1$coefficients[2]
result_table_abebe[2,3] <- application1$coefficients[3]

application_with_sfe <- lm(attended ~ incentive + wage + factor(blockid), data = final_data)
summary(application_with_sfe)

result_table_abebe[1,4] <- application_with_sfe$coefficients[2]
result_table_abebe[2,4] <- application_with_sfe$coefficients[3]

result_table_abebe[1,5] <- table(is.na(final_data$attended))[2]/(table(is.na(final_data$attended))[1] + table(is.na(final_data$attended))[2]) * 100
result_table_abebe[2,5] <- table(is.na(final_data$attended))[2]/(table(is.na(final_data$attended))[1] + table(is.na(final_data$attended))[2]) * 100


# cognitive + calculate attrition rate ---------------
cognitive1 <- lm(cognitive ~ incentive + wage, data = final_data)
summary(cognitive1)

result_table_abebe[3,3] <- cognitive1$coefficients[2]
result_table_abebe[4,3] <- cognitive1$coefficients[3]

cognitive_with_sfe <- lm(cognitive ~ incentive + wage + factor(blockid), data = final_data)
summary(cognitive_with_sfe)

result_table_abebe[3,4] <- cognitive_with_sfe$coefficients[2]
result_table_abebe[4,4] <- cognitive_with_sfe$coefficients[3]

result_table_abebe[3,5] <- table(is.na(final_data$cognitive))[2]/(table(is.na(final_data$cognitive))[1] + table(is.na(final_data$cognitive))[2]) * 100
result_table_abebe[4,5] <- table(is.na(final_data$cognitive))[2]/(table(is.na(final_data$cognitive))[1] + table(is.na(final_data$cognitive))[2]) * 100

# noncognitive + calculate attrition rate ---------------
noncognitive1 <- lm(noncognitive ~ incentive + wage, data = final_data)
summary(noncognitive1)

result_table_abebe[5,3] <- noncognitive1$coefficients[2]
result_table_abebe[6,3] <- noncognitive1$coefficients[3]

noncognitive_with_sfe <- lm(noncognitive ~ incentive + wage + factor(blockid), data = final_data)
summary(noncognitive_with_sfe)

result_table_abebe[5,4] <- noncognitive_with_sfe$coefficients[2]
result_table_abebe[6,4] <- noncognitive_with_sfe$coefficients[3]

result_table_abebe[5,5] <- table(is.na(final_data$noncognitive))[2]/(table(is.na(final_data$noncognitive))[1] + table(is.na(final_data$noncognitive))[2]) * 100
result_table_abebe[6,5] <- table(is.na(final_data$noncognitive))[2]/(table(is.na(final_data$noncognitive))[1] + table(is.na(final_data$noncognitive))[2]) * 100

# experience + calculate attrition rate ---------------
experience1 <- lm(experience ~ incentive + wage, data = final_data)
summary(experience1)

result_table_abebe[7,3] <- experience1$coefficients[2]
result_table_abebe[8,3] <- experience1$coefficients[3]

experience_with_sfe <- lm(experience ~ incentive + wage + factor(blockid), data = final_data)
summary(experience_with_sfe)

result_table_abebe[7,4] <- experience_with_sfe$coefficients[2]
result_table_abebe[8,4] <- experience_with_sfe$coefficients[3]

result_table_abebe[7,5] <- table(is.na(final_data$experience))[2]/(table(is.na(final_data$experience))[1] + table(is.na(final_data$experience))[2]) * 100
result_table_abebe[8,5] <- table(is.na(final_data$experience))[2]/(table(is.na(final_data$experience))[1] + table(is.na(final_data$experience))[2]) * 100

result_table_abebe[is.na(attrition), attrition := 0]

# save point estimates results ----------
fwrite(result_table_abebe, "abebe-main-results.csv")

