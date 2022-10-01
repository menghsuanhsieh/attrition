###############################
# date: 09/03/2022
# Groh & McKenzie application
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
gm_cleaned <- as.data.table(read_dta("Data/MacroInsurance_Cleaned.dta")) ## keep the unit with 16 individuals [(pair != "99999"),]

# table
gm_attr_est <- data.table(reg = c("Monthly Consumption",
                                  "Hours Worked",
                                  "Any Worker",
                                  "Paid Employees",
                                  "High Revenue",
                                  "Revenue",
                                  "High Profits",
                                  "Monthly Profits"),
                          Original = NA_real_, Theta = NA_real_, Attrition = NA_real_)

# Monthly Consumption ---------------
t5_reg <- lm(m_consumption_avg_cap ~ treat + factor(pair), data = gm_cleaned)
t5_reg$coefficients[2] # -0.0036, i.e. -0.004 (as reported in paper)

# save estimates
gm_attr_est[1, 2:3] <- list(as.numeric(t5_reg$coefficients[2]),
                            as.numeric(lm(m_consumption_avg_cap ~ treat, data = gm_cleaned)$coefficients[2]))

# calculate attrition rate
gm_attr_est[1, 4] <- list(100 * length(gm_cleaned[is.na(m_consumption_avg_cap)]$pair) / length(unique(gm_cleaned$pair)))

# Hours Worked ----------
t5_reg_5 <- lm(hours ~ treat + factor(pair), data = gm_cleaned)
t5_reg_5$coefficients[2] # 0.00136998

# save estimates
gm_attr_est[2, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(hours ~ treat, data = gm_cleaned)$coefficients[2]))

# calculate attrition rate
gm_attr_est[2, 4] <- list(100 * length(gm_cleaned[is.na(hours)==T]$pair) / length(gm_cleaned$pair))

# Any Worker ----------
t5_reg_61 <- lm(anyworker ~ treat + factor(pair), data = gm_cleaned)
t5_reg_61$coefficients[2] # 0.01164483

# save estimates
gm_attr_est[3, 2:3] <- list(as.numeric(t5_reg_61$coefficients[2]),
                            as.numeric(lm(anyworker ~ treat, data = gm_cleaned)$coefficients[2]))

# calculate attrition rate
gm_attr_est[3, 4] <- list(100 * nrow(gm_cleaned[is.na(anyworker)]) / length(gm_cleaned$pair))

# Paid Employees ----------
t5_reg_62 <- lm(m_paidemployees ~ treat + factor(pair), data = gm_cleaned)
t5_reg_62$coefficients[2] # -54.0697

# save estimates
gm_attr_est[4, 2:3] <- list(as.numeric(t5_reg_62$coefficients[2]),
                            as.numeric(lm(m_paidemployees ~ treat, data = gm_cleaned)$coefficients[2]))

# calculate attrition rate
gm_attr_est[4, 4] <- list(100 * nrow(gm_cleaned[is.na(m_paidemployees)]) / length(gm_cleaned$pair))

# High Revenue ----------
t5_reg_63 <- lm(highrevenue ~ treat + factor(pair), data = gm_cleaned)
t5_reg_63$coefficients[2] # -0.023554603854388911

# save estimates
gm_attr_est[5, 2:3] <- list(as.numeric(t5_reg_63$coefficients[2]),
                            as.numeric(lm(highrevenue ~ treat, data = gm_cleaned)$coefficients[2]))

# calculate attrition rate
gm_attr_est[5, 4] <- list(100 * nrow(gm_cleaned[is.na(highrevenue)]) / length(gm_cleaned$pair))

# Revenue ----------
t5_reg_64 <- lm(m_rev_cap ~ treat + factor(pair), data = gm_cleaned)
t5_reg_64$coefficients[2] # -4.853285e-16 

# save estimates
gm_attr_est[6, 2:3] <- list(as.numeric(t5_reg_64$coefficients[2]),
                            as.numeric(lm(m_rev_cap ~ treat, data = gm_cleaned)$coefficients[2]))

# calculate attrition rate
gm_attr_est[6, 4] <- list(100 * nrow(gm_cleaned[is.na(m_rev_cap)]) / length(gm_cleaned$pair))

# High Profits ----------
t5_reg_66 <- lm(highprofits ~ treat + factor(pair), data = gm_cleaned)
t5_reg_66$coefficients[2] # 0.008048634

# save estimates
gm_attr_est[7, 2:3] <- list(as.numeric(t5_reg_66$coefficients[2]),
                            as.numeric(lm(highprofits ~ treat, data = gm_cleaned)$coefficients[2]))

# calculate attrition rate
gm_attr_est[7, 4] <- list(100 * nrow(gm_cleaned[is.na(highprofits)]) / length(gm_cleaned$pair))

# Monthly Profits ----------
t5_reg_67 <- lm(m_prof_cap ~ treat + factor(pair), data = gm_cleaned)
t5_reg_67$coefficients[2] # 0.008048634

# save estimates
gm_attr_est[8, 2:3] <- list(as.numeric(t5_reg_67$coefficients[2]),
                            as.numeric(lm(m_prof_cap ~ treat, data = gm_cleaned)$coefficients[2]))

# calculate attrition rate
gm_attr_est[8, 4] <- list(100 * nrow(gm_cleaned[is.na(m_prof_cap)]) / length(gm_cleaned$pair))

## create final table ------------
final_table <- dcast(melt(gm_attr_est), variable ~ reg)

setcolorder(final_table, c("variable", "Monthly Profits", "High Profits", "Revenue", "High Revenue", "Paid Employees","Any Worker", "Hours Worked", "Monthly Consumption"))
setnames(final_table, c("variable","Monthly Profits","High Profits","Paid Employees","Hours Worked"), c("Estimates","Profits","High Profit","Number Employees","Owner's Hours"))

# percentage deviation ---------------
gm_attr_est[, ss := ifelse(reg=="High Revenue"|reg=="Revenue", 1, 0)
            ][, pct := abs(Original - Theta)/abs(Theta) * 100
              ][, avg_pct_chg := mean(pct), by = "ss"]

mean(gm_attr_est$pct, na.rm=TRUE)

# export ----------
fwrite(final_table, "Output/GM-attrition-est-results-final-wo-cov.csv")
print(xtable(final_table[,-c("Estimates")], include.colnames=TRUE, booktabs=TRUE, type = "latex", digits = 3, caption = "Average Treatment Effect Estimates in Groh \\& McKenzie (2016), No Covariates"),
      include.rownames=FALSE,
      hline.after = c(-1,0),
      file = "Output/GM-attrition-est-results-final-wo-cov.tex")
