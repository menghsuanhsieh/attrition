###############################
# date: 9/1/2022
# Groh & McKenzie application
###############################

rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(ramify, fastDummies, doParallel, foreach, data.table, nbpMatching, sandwich, lmtest, haven, tidyverse, xtable)

registerDoParallel(cores = 36)
getDoParWorkers()
getDoParName()

# working directory
setwd("/Users/rexhsieh/Dropbox (University of Michigan)/Attrition RCT - Applications/1. Groh & McKenzie (2016)")

# data
gm_cleaned <- as.data.table(read_dta("MacroInsurance_Cleaned.dta"))#[(pair != "99999"),]

# df = gm_cleaned; Y = "admin_loanrenewal"; D = "treat"; A = "attrition"; G = "pair"   ##debug

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
t5_reg <- lm(m_consumption_avg_cap ~ treat + factor(pair), data = gm_cleaned)
t5_reg$coefficients[2] # -0.0036, i.e. -0.004 (as reported in paper)

## missing observations, 17 pairs with missing outcomes
gm_attr_est[1, 2:3] <- list(as.numeric(t5_reg$coefficients[2]),
                            as.numeric(lm(m_consumption_avg_cap ~ treat, data = gm_cleaned)$coefficients[2]))
gm_attr_est[1, 4] <- list(100 * length(gm_cleaned[is.na(m_consumption_avg_cap)]$pair) / length(unique(gm_cleaned$pair)))

# Table 5, paper, regression 5 ----------
t5_reg_5 <- lm(hours ~ treat + factor(pair), data = gm_cleaned)
t5_reg_5$coefficients[2] # 0.00136998

gm_attr_est[2, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
                            as.numeric(lm(hours ~ treat, data = gm_cleaned)$coefficients[2]))
gm_attr_est[2, 4] <- list(100 * length(gm_cleaned[is.na(hours)==T]$pair) / length(gm_cleaned$pair))

# Table 6, paper, regression 1 ----------
t5_reg_61 <- lm(anyworker ~ treat + factor(pair), data = gm_cleaned)
t5_reg_61$coefficients[2] # 0.01164483

gm_attr_est[3, 2:3] <- list(as.numeric(t5_reg_61$coefficients[2]),
                            as.numeric(lm(anyworker ~ treat, data = gm_cleaned)$coefficients[2]))
gm_attr_est[3, 4] <- list(100 * nrow(gm_cleaned[is.na(anyworker)]) / length(gm_cleaned$pair))

# Table 6, paper, regression 2 ----------
t5_reg_62 <- lm(m_paidemployees ~ treat + factor(pair), data = gm_cleaned)
t5_reg_62$coefficients[2] # -54.0697

gm_attr_est[4, 2:3] <- list(as.numeric(t5_reg_62$coefficients[2]),
                            as.numeric(lm(m_paidemployees ~ treat, data = gm_cleaned)$coefficients[2]))
gm_attr_est[4, 4] <- list(100 * nrow(gm_cleaned[is.na(m_paidemployees)]) / length(gm_cleaned$pair))

# Table 6, paper, regression 3 ----------
t5_reg_63 <- lm(highrevenue ~ treat + factor(pair), data = gm_cleaned)
t5_reg_63$coefficients[2] # -0.023554603854388911

gm_attr_est[5, 2:3] <- list(as.numeric(t5_reg_63$coefficients[2]),
                            as.numeric(lm(highrevenue ~ treat, data = gm_cleaned)$coefficients[2]))
gm_attr_est[5, 4] <- list(100 * nrow(gm_cleaned[is.na(highrevenue)]) / length(gm_cleaned$pair))

# Table 6, paper, regression 4 ----------
t5_reg_64 <- lm(m_rev_cap ~ treat + factor(pair), data = gm_cleaned)
t5_reg_64$coefficients[2] # -4.853285e-16 

gm_attr_est[6, 2:3] <- list(as.numeric(t5_reg_64$coefficients[2]),
                            as.numeric(lm(m_rev_cap ~ treat, data = gm_cleaned)$coefficients[2]))
gm_attr_est[6, 4] <- list(100 * nrow(gm_cleaned[is.na(m_rev_cap)]) / length(gm_cleaned$pair))

# Table 6, paper, regression 6 ----------
t5_reg_66 <- lm(highprofits ~ treat + factor(pair), data = gm_cleaned)
t5_reg_66$coefficients[2] # 0.008048634

gm_attr_est[7, 2:3] <- list(as.numeric(t5_reg_66$coefficients[2]),
                            as.numeric(lm(highprofits ~ treat, data = gm_cleaned)$coefficients[2]))
gm_attr_est[7, 4] <- list(100 * nrow(gm_cleaned[is.na(highprofits)]) / length(gm_cleaned$pair))

# Table 6, paper, regression 6 ----------
t5_reg_67 <- lm(m_prof_cap ~ treat + factor(pair), data = gm_cleaned)
t5_reg_67$coefficients[2] # 0.008048634

gm_attr_est[8, 2:3] <- list(as.numeric(t5_reg_67$coefficients[2]),
                            as.numeric(lm(m_prof_cap ~ treat, data = gm_cleaned)$coefficients[2]))
gm_attr_est[8, 4] <- list(100 * nrow(gm_cleaned[is.na(m_prof_cap)]) / length(gm_cleaned$pair))

## dcast
final_table <- dcast(melt(gm_attr_est), variable ~ reg)

setcolorder(final_table, c("variable", "Monthly Profits", "High Profits", "Revenue", "High Revenue", "Paid Employees","Any Worker", "Hours Worked", "Monthly Consumption"))
setnames(final_table, c("variable","Monthly Profits","High Profits","Paid Employees","Hours Worked"), c("Estimates","Profits","High Profit","Number Employees","Owner's Hours"))

# # Table 7, last specification -----------
# t5_reg_7 <- lm(Table7index ~ treat + factor(pair), data = gm_cleaned)
# t5_reg_7$coefficients[2] # 0.008048634
# 
# list(as.numeric(t5_reg_7$coefficients[2]),
#      theta_n(df = gm_cleaned, Y = "Table7index", D = "treat", A = "attrition"),
#      theta_drop(df = gm_cleaned, Y = "Table7index", D = "treat", A = "attrition", G = "pair"))


# percentage deviatio ---------------
gm_attr_est[, ss := ifelse(reg=="High Revenue"|reg=="Revenue", 1, 0)
][, pct := abs(Original - Theta)/abs(Theta) * 100
][, avg_pct_chg := mean(pct), by = "ss"]

mean(gm_attr_est$pct, na.rm=TRUE)

# export ----------
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(gm_attr_est))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Note: For each outcome regression specification, we report (1) the point estimates reported in the original paper, (2) the standard difference-in-means estimators (\\eqref{}), (3) the estimator on the coefficient of treatment status with matched-pair/strata fixed effects, and (4) the attrition rate in \\% by outcome, defined as [number of matched-pairs/individuals with missing outcome in row/total number of matched-pairs/individuals]. Rows are labeled according to the outcome used in paper. Columns are labeled according to the types of estimates and attrition rate. The definitions of the outcomes can be found in the main text.}\n", sep = ""))

fwrite(final_table, "attrition-est-results-final-wo-cov.csv")
print(xtable(final_table[,-c("Estimates")], include.colnames=TRUE, booktabs=TRUE, type = "latex", digits = 3, caption = "Average Treatment Effect Estimates in Groh \\& McKenzie (2016)"),
      include.rownames=FALSE,
      # add.to.row = comment, 
      hline.after = c(-1,0),
      file = "attrition-est-results-final-wo-cov.tex")


# OLD -------------------------
# # Table 5, paper, regression 1 ----------
# t5_reg <- lm(admin_loanrenewal ~ treat + factor(pair), data = gm_cleaned)
# t5_reg$coefficients[2] # -0.0036, i.e. -0.004 (as reported in paper)
# 
# ## missing observations, 17 pairs with missing outcomes
# gm_attr_est[1, 2:3] <- list(as.numeric(t5_reg$coefficients[2]),
#                             theta_n(df = gm_cleaned, Y = "admin_loanrenewal", D = "treat", A = "attrition"),
#                             theta_drop(df = gm_cleaned, Y = "admin_loanrenewal", D = "treat", A = "attrition", G = "pair"))
# gm_attr_est[1, 4] <- list(100 * nrow(gm_cleaned[is.na(admin_loanrenewal)]) / length(unique(gm_cleaned$pair)))
# 
# # Table 5, paper, regression 5 ----------
# t5_reg_5 <- lm(hours ~ treat + factor(pair), data = gm_cleaned)
# t5_reg_5$coefficients[2] # 0.00136998
# 
# gm_attr_est[2, 2:3] <- list(as.numeric(t5_reg_5$coefficients[2]),
#                             theta_n(df = gm_cleaned, Y = "hours", D = "treat", A = "attrition"),
#                             theta_drop(df = gm_cleaned, Y = "hours", D = "treat", A = "attrition", G = "pair"))
# gm_attr_est[2, 4] <- list(100 * nrow(gm_cleaned[is.na(hours)]) / length(unique(gm_cleaned$pair)))
# 
# # Table 6, paper, regression 1 ----------
# t5_reg_61 <- lm(anyworker ~ treat + factor(pair), data = gm_cleaned)
# t5_reg_61$coefficients[2] # 0.01164483
# 
# gm_attr_est[3, 2:3] <- list(as.numeric(t5_reg_61$coefficients[2]),
#                             theta_n(df = gm_cleaned, Y = "anyworker", D = "treat", A = "attrition"),
#                             theta_drop(df = gm_cleaned, Y = "anyworker", D = "treat", A = "attrition", G = "pair"))
# gm_attr_est[3, 4] <- list(100 * nrow(gm_cleaned[is.na(anyworker)]) / length(unique(gm_cleaned$pair)))
# 
# # Table 6, paper, regression 2 ----------
# t5_reg_62 <- lm(m_paidemployees ~ treat + factor(pair), data = gm_cleaned)
# t5_reg_62$coefficients[2] # -54.0697
# 
# gm_attr_est[4, 2:3] <- list(as.numeric(t5_reg_62$coefficients[2]),
#                             theta_n(df = gm_cleaned, Y = "m_paidemployees", D = "treat", A = "attrition"),
#                             theta_drop(df = gm_cleaned, Y = "m_paidemployees", D = "treat", A = "attrition", G = "pair"))
# gm_attr_est[4, 4] <- list(100 * nrow(gm_cleaned[is.na(m_paidemployees)]) / length(unique(gm_cleaned$pair)))
# 
# # Table 6, paper, regression 3 ----------
# t5_reg_63 <- lm(highrevenue ~ treat + factor(pair), data = gm_cleaned)
# t5_reg_63$coefficients[2] # -0.023554603854388911
# 
# gm_attr_est[5, 2:3] <- list(as.numeric(t5_reg_63$coefficients[2]),
#                             theta_n(df = gm_cleaned, Y = "highrevenue", D = "treat", A = "attrition"),
#                             theta_drop(df = gm_cleaned, Y = "highrevenue", D = "treat", A = "attrition", G = "pair"))
# gm_attr_est[5, 4] <- list(100 * nrow(gm_cleaned[is.na(highrevenue)]) / length(unique(gm_cleaned$pair)))
# 
# # Table 6, paper, regression 4 ----------
# t5_reg_64 <- lm(m_rev_cap ~ treat + factor(pair), data = gm_cleaned)
# t5_reg_64$coefficients[2] # -4.853285e-16 
# 
# gm_attr_est[6, 2:3] <- list(as.numeric(t5_reg_64$coefficients[2]),
#                             theta_n(df = gm_cleaned, Y = "m_rev_cap", D = "treat", A = "attrition"),
#                             theta_drop(df = gm_cleaned, Y = "m_rev_cap", D = "treat", A = "attrition", G = "pair"))
# gm_attr_est[6, 4] <- list(100 * nrow(gm_cleaned[is.na(m_rev_cap)]) / length(unique(gm_cleaned$pair)))
# 
# # Table 6, paper, regression 6 ----------
# t5_reg_66 <- lm(highprofits ~ treat + factor(pair), data = gm_cleaned)
# t5_reg_66$coefficients[2] # 0.008048634
# 
# gm_attr_est[7, 2:3] <- list(as.numeric(t5_reg_66$coefficients[2]),
#                             theta_n(df = gm_cleaned, Y = "highprofits", D = "treat", A = "attrition"),
#                             theta_drop(df = gm_cleaned, Y = "highprofits", D = "treat", A = "attrition", G = "pair"))
# gm_attr_est[7, 4] <- list(100 * nrow(gm_cleaned[is.na(highprofits)]) / length(unique(gm_cleaned$pair)))
# 
# # Table 6, paper, regression 6 ----------
# t5_reg_67 <- lm(m_prof_cap ~ treat + factor(pair), data = gm_cleaned)
# t5_reg_67$coefficients[2] # 0.008048634
# 
# gm_attr_est[8, 2:3] <- list(as.numeric(t5_reg_67$coefficients[2]),
#                             theta_n(df = gm_cleaned, Y = "m_prof_cap", D = "treat", A = "attrition"),
#                             theta_drop(df = gm_cleaned, Y = "m_prof_cap", D = "treat", A = "attrition", G = "pair"))
# gm_attr_est[8, 4] <- list(100 * nrow(gm_cleaned[is.na(m_prof_cap)]) / length(unique(gm_cleaned$pair)))
# 
# 
# # # Table 7, last specification -----------
# # t5_reg_7 <- lm(Table7index ~ treat + factor(pair), data = gm_cleaned)
# # t5_reg_7$coefficients[2] # 0.008048634
# # 
# # list(as.numeric(t5_reg_7$coefficients[2]),
# #      theta_n(df = gm_cleaned, Y = "Table7index", D = "treat", A = "attrition"),
# #      theta_drop(df = gm_cleaned, Y = "Table7index", D = "treat", A = "attrition", G = "pair"))
# 
# # export ----------
# comment <- list(pos = list(0), command = NULL)
# comment$pos[[1]] <- c(nrow(gm_attr_est))
# comment$command <- c(paste("\\hline\n",
#                            "{\\footnotesize Note: For each outcome regression specification, we report (1) the point estimates reported in the original paper, (2) the standard difference-in-means estimators (\\eqref{}), (3) the estimator on the coefficient of treatment status with matched-pair/strata fixed effects, and (4) the attrition rate in \\% by outcome, defined as [number of matched-pairs/individuals with missing outcome in row/total number of matched-pairs/individuals]. Rows are labeled according to the outcome used in paper. Columns are labeled according to the types of estimates and attrition rate. The definitions of the outcomes can be found in the main text.}\n", sep = ""))
# 
# fwrite(gm_attr_est, "attrition-est-results-v3.csv")
# print(xtable(gm_attr_est[, .(reg, theta_n, theta_drop, attrition)], include.colnames=TRUE, booktabs=TRUE, type = "latex", digits = 5, caption = "Average Treatment Effect Estimates in Groh \\& McKenzie (2016)"),
#       include.rownames=FALSE,
#       add.to.row = comment, 
#       hline.after = c(-1,0),
#       file = "attrition-est-results-v3.tex")
# 
# 
# # # more regressions..... ------------
# # theta_drop(df = gm_cleaned, Y = "epp_loanamount", D = "treat", A = "attrition", G = "pair")
# # 
# # 
# # Table 5, paper, regression 2 ----------
# # t5_reg_2 <- lm(epp_loanamount ~ treat + factor(pair), data = gm_cleaned)
# # t5_reg_2$coefficients[2] # -0.0036, i.e. -0.004 (as reported in paper)
# 
# # theta_n(df = gm_cleaned, Y = "epp_loanamount", D = "treat", A = "attrition") # - 0.001376462, i.e. -0.005
# # theta_drop(df = gm_cleaned, Y = "epp_loanamount", D = "treat", A = "attrition", G = "pair") # -0.00136998 (as reported in paper)
# 
# # list(100 * nrow(gm_cleaned[is.na(epp_loanamount)]) / length(unique(gm_cleaned$pair)))
# 
# # theta_n(df = gm_cleaned, Y = "tookotherloan", D = "treat", A = "attrition") # - 0.001376462, i.e. -0.005
# # theta_drop(df = gm_cleaned, Y = "tookotherloan", D = "treat", A = "attrition", G = "pair") # -3.3235e-9, i.e. 0
# 
# # # superseded code --------------------
# # theta_n(df = gm_cleaned, Y = "admin_loanrenewal", D = "treat", A = "attrition") # - 0.004817, i.e. -0.005
# # theta_drop(df = gm_cleaned, Y = "admin_loanrenewal", D = "treat", A = "attrition", G = "pair") # -0.003441156, i.e. 0.00344
# 
# # # sanity check
# # drop_reg_1 <- lm(admin_loanrenewal ~ treat + factor(pair), data = gm_cleaned[attrition==1,])
# # drop_reg_1$coefficients[2] # -0.003481894  - yeah sanity check OK
# 
# # # lm problem
# # est <- (t(gm_cleaned$treat)%*%gm_cleaned$treat)^(-1)*(t(gm_cleaned$treat)%*%gm_cleaned$admin_loanrenewal)
# # 
# # gm_cleaned[, `:=` (y_mean = mean(admin_loanrenewal, na.rm=TRUE),
# #                    d_mean = mean(treat, na.rm=TRUE)), by = pair]
# # 
# # Xd <- (gm_cleaned$treat - gm_cleaned$d_mean)
# # Xd[is.na(Xd)] <- 0
# # Yd <- (gm_cleaned$admin_loanrenewal - gm_cleaned$y_mean)
# # Yd[is.na(Yd)] <- 0
# # est <- (t(Xd)%*%Xd)^(-1)*(t(Xd)%*%Yd)
# 
# # Table 6, paper, regression 5 ---------- THIS HAS COVARIATES
# # t5_reg_65 <- lm(m_inventories ~ treat + b_inventories + factor(pair), data = gm_cleaned)
# # t5_reg_65$coefficients[2] # -821.9803
# # 
# # theta_n(df = gm_cleaned, Y = "m_inventories", D = "treat", A = "attrition") # -629.2292
# # theta_drop(df = gm_cleaned, Y = "m_inventories", D = "treat", A = "attrition", G = "pair") # -526.5795
# # 
# # list(100 * nrow(gm_cleaned[is.na(m_inventories)]) / length(unique(gm_cleaned$pair)))
# diff-in-means 

# theta_n <- function (df, A, D, Y) {
#   
#   X <- sum(df[,..Y] * df[,..A] * df[,..D], na.rm=TRUE)
#   Z <- sum(df[,..A] * df[,..D], na.rm=TRUE)
#   
#   W <- sum(df[,..Y] * df[,..A] * (1-df[,..D]), na.rm=TRUE)
#   WW <- sum(df[,..A] * (1-df[,..D]), na.rm=TRUE)
#   
#   return((X/Z) - (W/WW))
#   
# }
# 
# # fixed effects
# theta_drop <- function (df, A, D, Y, G) {
#   
#   first <- sum(copy(df)[, c(..A,..G)][, .(mult = prod(get(A), na.rm = TRUE)), by = G]$mult, na.rm=TRUE)^(-1)
#   
#   temp <- copy(df)[, c(..A, ..D, ..Y, ..G)
#   ][, mult := prod(get(A), na.rm = TRUE), by = G
#   ][, `:=` (y_diff = (get(..Y) - shift(get(..Y), n = 1L)),
#             d_diff = (get(..D) - shift(get(..D), n = 1L))), by = G]
#   
#   length(which(is.na(temp$mult * temp$y_diff * temp$d_diff)==FALSE))
#   
#   second_s <- sum(temp$mult * temp$y_diff * temp$d_diff, na.rm=TRUE)
#   
#   return(first * second_s)
#   
# }
