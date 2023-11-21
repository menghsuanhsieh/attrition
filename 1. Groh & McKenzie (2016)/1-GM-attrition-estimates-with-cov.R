rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(ramify, fastDummies, doParallel, foreach, data.table, nbpMatching, sandwich, lmtest, haven, tidyverse, xtable, rstudioapi)

# # working directory # if running this file alone, uncomment this + next line
# setwd(dirname(getSourceEditorContext()$path))

# data
gm_cleaned <- as.data.table(read_dta("MacroInsurance_Cleaned.dta"))#[(pair != "99999"),]

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

# Table 5, paper, regression 1 ----------
t5_reg <- lm(m_consumption_avg_cap ~ treat + totalconsumption_monthly + factor(pair), data = gm_cleaned)
t5_reg$coefficients[2] 

gm_attr_est[1, 2:3] <- list(as.numeric(t5_reg$coefficients[2]),
                            as.numeric(lm(m_consumption_avg_cap ~ treat + totalconsumption_monthly, data = gm_cleaned)$coefficients[2]))
gm_attr_est[1, 4] <- list(100 * length(gm_cleaned[is.na(m_consumption_avg_cap)==T]$pair) / length(gm_cleaned$pair))

# Table 5, paper, regression 5 ----------
t5_reg_5 <- lm(hours ~ treat + b_hours + b_hours_miss + factor(pair), data = gm_cleaned)
t5_reg_5$coefficients[2] 

gm_attr_est[2, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(hours ~ treat + b_hours + b_hours_miss, data = gm_cleaned)$coefficients[2]))
gm_attr_est[2, 4] <- list(100 * length(gm_cleaned[is.na(hours)==T]$pair) / length(gm_cleaned$pair))

# Table 6, paper, regression 1 ----------
t5_reg_61 <- lm(anyworker ~ treat + baseanyworker + factor(pair), data = gm_cleaned)
t5_reg_61$coefficients[2] 

gm_attr_est[3, 2:3] <- list(as.numeric(t5_reg_61$coefficients[2]),
                            as.numeric(lm(anyworker ~ treat + baseanyworker, data = gm_cleaned)$coefficients[2]))
gm_attr_est[3, 4] <- list(100 * nrow(gm_cleaned[is.na(anyworker)==T]) / length(gm_cleaned$pair))

# Table 6, paper, regression 2 ----------
t5_reg_62 <- lm(m_paidemployees ~ treat + base_paidemp + factor(pair), data = gm_cleaned)
t5_reg_62$coefficients[2]

gm_attr_est[4, 2:3] <- list(as.numeric(t5_reg_62$coefficients[2]),
                            as.numeric(lm(m_paidemployees ~ treat + base_paidemp, data = gm_cleaned)$coefficients[2]))
gm_attr_est[4, 4] <- list(100 * nrow(gm_cleaned[is.na(m_paidemployees)==T]) / length(gm_cleaned$pair))

# Table 6, paper, regression 3 ----------
t5_reg_63 <- lm(highrevenue ~ treat + b_highrevenue + miss_baserevenue + factor(pair), data = gm_cleaned)
t5_reg_63$coefficients[2]

gm_attr_est[5, 2:3] <- list(as.numeric(t5_reg_63$coefficients[2]),
                            as.numeric(lm(highrevenue ~ treat + b_highrevenue + miss_baserevenue, data = gm_cleaned)$coefficients[2]))
gm_attr_est[5, 4] <- list(100 * nrow(gm_cleaned[is.na(highrevenue)==T]) / length(gm_cleaned$pair))

# Table 6, paper, regression 4 ----------
t5_reg_64 <- lm(m_rev_cap ~ treat + b_revenue + miss_baserevenue + factor(pair), data = gm_cleaned)
t5_reg_64$coefficients[2]

gm_attr_est[6, 2:3] <- list(as.numeric(t5_reg_64$coefficients[2]),
                            as.numeric(lm(m_rev_cap ~ treat + b_revenue + miss_baserevenue, data = gm_cleaned)$coefficients[2]))
gm_attr_est[6, 4] <- list(100 * nrow(gm_cleaned[is.na(m_rev_cap)==T]) / length(gm_cleaned$pair))

# Table 6, paper, regression 6 ----------
t5_reg_66 <- lm(highprofits ~ treat + b_highprofits + miss_baseprofits + factor(pair), data = gm_cleaned)
t5_reg_66$coefficients[2] 

gm_attr_est[7, 2:3] <- list(as.numeric(t5_reg_66$coefficients[2]),
                            as.numeric(lm(highprofits ~ treat + b_highprofits + miss_baseprofits, data = gm_cleaned)$coefficients[2]))
gm_attr_est[7, 4] <- list(100 * nrow(gm_cleaned[is.na(highprofits)==T]) / length(gm_cleaned$pair))

# Table 6, paper, regression 6 ----------
t5_reg_67 <- lm(m_prof_cap ~ treat + b_profits + miss_baseprofits + factor(pair), data = gm_cleaned)
t5_reg_67$coefficients[2] 

gm_attr_est[8, 2:3] <- list(as.numeric(t5_reg_67$coefficients[2]),
                            as.numeric(lm(m_prof_cap ~ treat + b_profits + miss_baseprofits, data = gm_cleaned)$coefficients[2]))
gm_attr_est[8, 4] <- list(100 * nrow(gm_cleaned[is.na(m_prof_cap)==T]) / length(gm_cleaned$pair))

## dcast
final_table <- dcast(melt(gm_attr_est), variable ~ reg)

setcolorder(final_table, c("variable", "Monthly Profits", "High Profits", "Revenue", "High Revenue", "Paid Employees","Any Worker", "Hours Worked", "Monthly Consumption"))
setnames(final_table, c("variable","Monthly Profits","High Profits","Paid Employees","Hours Worked"), c("Estimates","Profits","High Profit","Number Employees","Owner's Hours"))

# percentage deviatio ---------------
gm_attr_est[, ss := ifelse(reg=="High Revenue"|reg=="Revenue", 1, 0)
][, pct := abs(Original - Theta)/abs(Original) * 100
][, avg_pct_chg := mean(pct), by = "ss"]

mean(gm_attr_est$pct)

# export ----------
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(gm_attr_est))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Note: For each outcome regression specification, we report (1) the point estimates reported in the original paper, (2) the standard difference-in-means estimators (\\eqref{}), (3) the estimator on the coefficient of treatment status with matched-pair/strata fixed effects, and (4) the attrition rate in \\% by outcome, defined as [number of matched-pairs/individuals with missing outcome in row/total number of matched-pairs/individuals]. Rows are labeled according to the outcome used in paper. Columns are labeled according to the types of estimates and attrition rate. The definitions of the outcomes can be found in the main text.}\n", sep = ""))

fwrite(final_table, "attrition-est-results-final.csv")
print(xtable(final_table[,-c("Estimates")], include.colnames=TRUE, booktabs=TRUE, type = "latex", digits = 3, caption = "Average Treatment Effect Estimates in Groh \\& McKenzie (2016)"),
      include.rownames=FALSE,
      # add.to.row = comment, 
      hline.after = c(-1,0),
      file = "attrition-est-results-final.tex")
