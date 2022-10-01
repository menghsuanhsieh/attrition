###############################
# date: 09/20/2022
# Casaburi & Reed application
###############################

rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(data.table, sandwich, lmtest, haven, tidyverse, xtable)

# working directory
setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

# data
bags <- as.data.table(read_dta("Data/data_bags_cleaned_210630.dta"))

# table
cr_attr_est <- data.table(reg = c("Bags (1)",
                                  "Bags (2)",
                                  "Bags (3)",
                                  "Bags (4)",
                                  "Bags (5)"),
                          Original = NA_real_, Theta = NA_real_, Attrition = NA_real_)

# bags (1) ----------
t5_reg <- lm(pf ~ treat + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg$coefficients[2]

# save estimates
cr_attr_est[1, 2:3] <- list(as.numeric(t5_reg$coefficients[2]),
                            as.numeric(lm(pf ~ treat, data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))

# calculate attrition rate
cr_attr_est[1, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

# bags (2) FE ----------
t5_reg <- lm(pf ~ treat + factor(week) + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg$coefficients[2]

# save estimates
cr_attr_est[2, 2:3] <- list(as.numeric(t5_reg$coefficients[2]),
                            as.numeric(lm(pf ~ treat + factor(week), data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))

# calculate attrition rate
cr_attr_est[2, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

# bags (3) FE TC -----------
t5_reg_5 <- lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B   + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B  + factor(week) + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg_5$coefficients[2]

# save estimates
cr_attr_est[3, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B + factor(week), data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))

# calculate attrition rate
cr_attr_est[3, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

# bags (4) FE VC -----------
t5_reg_5 <- lm(pf ~ treat + lshare + N_traders_oth + N_treat_oth + milesnearest + suppliers + chief_1 + chief_2 + chief_3 + chief_4 + chief_5 + factor(week) + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg_5$coefficients[2] 

# save estimates
cr_attr_est[4, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(pf ~ treat + lshare + N_traders_oth + N_treat_oth + milesnearest + suppliers + chief_1 + chief_2 + chief_3 + chief_4 + chief_5 + factor(week), data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))

# calculate attrition rate
cr_attr_est[4, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

#  bags (%) everything ----------
t5_reg_5 <- lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B   + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B + lshare + N_traders_oth + N_treat_oth + milesnearest + suppliers + chief_1 + chief_2 + chief_3 + chief_4 + chief_5 + factor(week) + factor(pair), data = bags[after==1][a16_1_gradeA==1])
t5_reg_5$coefficients[2] 

# save estimates
cr_attr_est[5, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B   + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B + lshare + N_traders_oth + N_treat_oth + milesnearest + suppliers + chief_1 + chief_2 + chief_3 + chief_4 + chief_5 + factor(week), data = bags[after==1][a16_1_gradeA==1])$coefficients[2]))

# calculate attrition rate
cr_attr_est[5, 4] <- list(100 - 100 * length(unique(bags[after==1][a16_1_gradeA==1][is.na(pf)==FALSE]$sid)) / 80)

# final table -------------
final_table <- dcast(melt.data.table(copy(cr_attr_est)), variable ~ reg)

# percentage deviation ---------------
cr_attr_est[, ss := ifelse(reg=="Bags", 0, 1)
            ][, pct := abs((Original - Theta)/abs(Theta)) * 100
              ][, avg_pct_chg := mean(pct)
                ][, pct_1 := abs((Theta - Original)/abs(Original)) * 100]

# export ----------
fwrite(final_table, "Output/CR-attrition-est-results-final.csv")
print(xtable(final_table, include.colnames=TRUE, booktabs=TRUE, type = "latex", digits = 5, caption = "Average Treatment Effect Estimates in Casabury \\& Reed (2021)"),
      include.rownames=FALSE,
      # add.to.row = comment, 
      hline.after = c(-1,0),
      file = "Output/CR-attrition-est-results-final.tex")

