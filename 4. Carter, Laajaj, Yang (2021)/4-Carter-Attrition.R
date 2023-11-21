rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(ramify, fastDummies, doParallel, foreach, data.table, nbpMatching, sandwich, lmtest, haven, tidyverse, xtable, rstudioapi)

# # working directory  # if running this file alone, uncomment this + next line
# setwd(dirname(getSourceEditorContext()$path))

# data -----------------
final_data <- as.data.table(read_dta("data/original/Moz1234panel.dta"))

# result table 
result_table_carter <- data.table(outcome = rep(c("Fertilizer","Improved seed","Maize yield", "Consumption", "Expected yield"), each = 2), before_after = rep(c("before","after"), times = 5), no_sfe = NA_real_, with_sfe = NA_real_, attrition = NA_real_)

# outcome 1 ---------------
lfertmizr1 <- lm(lfertmaizr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round) + factor(vlgid_round), data = final_data)

summary(lfertmizr1)

result_table_carter[1,4] <- lfertmizr1$coefficients[2]
result_table_carter[2,4] <- lfertmizr1$coefficients[3]

lfertmizr1_without_sfe <- lm(lfertmaizr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round), data = final_data)

summary(lfertmizr1_without_sfe)

result_table_carter[1,3] <- lfertmizr1_without_sfe$coefficients[2]
result_table_carter[2,3] <- lfertmizr1_without_sfe$coefficients[3]

result_table_carter[1,5] <- table(is.na(final_data$lfertmaizr))[2] / (table(is.na(final_data$lfertmaizr))[1] + table(is.na(final_data$lfertmaizr))[2]) * 100
result_table_carter[2,5] <- table(is.na(final_data$lfertmaizr))[2] / (table(is.na(final_data$lfertmaizr))[1] + table(is.na(final_data$lfertmaizr))[2]) * 100

# outcome 2 ---------------
limprovedseedsr1 <- lm(limprovedseedsr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round) + factor(vlgid_round), data = final_data)

summary(limprovedseedsr1)

result_table_carter[3,4] <- limprovedseedsr1$coefficients[2]
result_table_carter[4,4] <- limprovedseedsr1$coefficients[3]

limprovedseedsr_without_sfe <- lm(limprovedseedsr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round), data = final_data)

summary(lfertmizr1_without_sfe)

result_table_carter[3,3] <- limprovedseedsr_without_sfe$coefficients[2]
result_table_carter[4,3] <- limprovedseedsr_without_sfe$coefficients[3]

result_table_carter[3,5] <- table(is.na(final_data$limprovedseedsr))[2] / (table(is.na(final_data$limprovedseedsr))[1] + table(is.na(final_data$limprovedseedsr))[2]) * 100
result_table_carter[4,5] <- table(is.na(final_data$limprovedseedsr))[2] / (table(is.na(final_data$limprovedseedsr))[1] + table(is.na(final_data$limprovedseedsr))[2]) * 100


# outcome 3 ---------------
lyieldr1 <- lm(lyieldr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round) + factor(vlgid_round), data = final_data)

summary(lyieldr1)

result_table_carter[5,4] <- lyieldr1$coefficients[2]
result_table_carter[6,4] <- lyieldr1$coefficients[3]

lyieldr_without_sfe <- lm(lyieldr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round), data = final_data)

summary(lyieldr_without_sfe)

result_table_carter[5,3] <- lyieldr_without_sfe$coefficients[2]
result_table_carter[6,3] <- lyieldr_without_sfe$coefficients[3]

result_table_carter[5,5] <- table(is.na(final_data$lyieldr))[2] / (table(is.na(final_data$lyieldr))[1] + table(is.na(final_data$lyieldr))[2]) * 100
result_table_carter[6,5] <- table(is.na(final_data$lyieldr))[2] / (table(is.na(final_data$lyieldr))[1] + table(is.na(final_data$lyieldr))[2]) * 100


# outcome 4 ---------------
ldailyconsr1 <- lm(ldailyconsr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round) + factor(vlgid_round), data = final_data)

summary(ldailyconsr1)

result_table_carter[7,4] <- ldailyconsr1$coefficients[2]
result_table_carter[8,4] <- ldailyconsr1$coefficients[3]

ldailyconsr_without_sfe <- lm(ldailyconsr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round), data = final_data)

summary(ldailyconsr_without_sfe)

result_table_carter[7,3] <- ldailyconsr_without_sfe$coefficients[2]
result_table_carter[8,3] <- ldailyconsr_without_sfe$coefficients[3]


result_table_carter[7,5] <- table(is.na(final_data$ldailyconsr))[2] / (table(is.na(final_data$ldailyconsr))[1] + table(is.na(final_data$ldailyconsr))[2]) * 100
result_table_carter[8,5] <- table(is.na(final_data$ldailyconsr))[2] / (table(is.na(final_data$ldailyconsr))[1] + table(is.na(final_data$ldailyconsr))[2]) * 100

# outcome 5 ---------------
lexp_yield_fertr1 <- lm(lexp_yield_fertr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round) + factor(vlgid_round), data = final_data)

summary(lexp_yield_fertr1)

result_table_carter[9,4] <- lexp_yield_fertr1$coefficients[2]
result_table_carter[10,4] <- lexp_yield_fertr1$coefficients[3]

lexp_yield_fertr_without_sfe <- lm(lexp_yield_fertr ~ vouch_dur + vouch_aft + sn2up_sub_dur + sn2up_sub_aft + factor(nw_talkedagmoder)*factor(round), data = final_data)

summary(lexp_yield_fertr_without_sfe)

result_table_carter[9,3] <- lexp_yield_fertr_without_sfe$coefficients[2]
result_table_carter[10,3] <- lexp_yield_fertr_without_sfe$coefficients[3]

result_table_carter[9,5] <- table(is.na(final_data$lexp_yield_fertr))[2] / (table(is.na(final_data$lexp_yield_fertr))[1] + table(is.na(final_data$lexp_yield_fertr))[2]) * 100
result_table_carter[10,5] <- table(is.na(final_data$lexp_yield_fertr))[2] / (table(is.na(final_data$lexp_yield_fertr))[1] + table(is.na(final_data$lexp_yield_fertr))[2]) * 100


# save point estimates results ----------
fwrite(result_table_carter, "carter-main-results.csv")
