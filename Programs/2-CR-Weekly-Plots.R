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
setwd("/Users/rexhsieh/Dropbox (University of Michigan)/Attrition RCT - Applications/2. Casaburi & Reed (2021)")

# data
bags <- as.data.table(read_dta("R2/dta/data_for_analysis/data_bags_cleaned_210630.dta"))
credit <- as.data.table(read_dta("R2/dta/data_for_analysis/data_credit_cleaned_210630.dta"))
quantity <- as.data.table(read_dta("R2/dta/data_for_analysis/data_quantity_weekly_cleaned_210630.dta"))

# df = gm_cleaned; Y = "admin_loanrenewal"; D = "treat"; A = "attrition"; G = "pair"   ##debug
# reg pf treat i.pair 
# table
cr_attr_est <- data.table(reg = c("Bags",
                                  "Credit",
                                  "Quantity"),
                          orig_est = NA_real_, theta_n = NA_real_, attrition = NA_real_)

weeks <- unique(bags[after==1][a16_1_gradeA==1]$week)

# (1) ----------
results <- list()

for (w in weeks) {
  
  results[[w]] <- list(as.numeric(lm(pf ~ treat + factor(pair), data = bags[week==w][after==1][a16_1_gradeA==1])$coefficients[2]),
                       as.numeric(lm(pf ~ treat, data = bags[week==w][after==1][a16_1_gradeA==1])$coefficients[2]))
  
}

results_dt <- rbindlist(results)
results_dt[, week := paste0(1960+floor(weeks/52), "w", weeks%%52)]
setorder(results_dt, "week")
setnames(results_dt, c("V1","V2"), c("With Pair Fixed Effects", "Without Pair Fixed Effects"))
results_dt_w <- melt(results_dt)

# calculation----
results_dt[, `(wo pfe - pfe) / pfe` := abs((`Without Pair Fixed Effects` - `With Pair Fixed Effects`) / abs(`With Pair Fixed Effects`)) * 100
           ][, `(pfe - wo pfe) / wo pfe` := abs((`With Pair Fixed Effects` - `Without Pair Fixed Effects`) / abs(`Without Pair Fixed Effects`)) * 100]

mean(results_dt$`(pfe - wo pfe) / wo pfe`, na.rm=TRUE)

# graph ------
p <- ggplot(results_dt_w) +
  geom_point(aes(x=week, y=value, shape=variable, color=variable), size = 3) +
  xlab("Week Number") +
  ylab("Point Estimates") +
  ylim(c(-200,100)) +
  theme_minimal() +
  guides(shape=guide_legend(title="Coefficient"), color=guide_legend(title="Coefficient")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=12),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.key.size = unit(1.8, 'cm'),
        axis.ticks.y = element_line()) +
  guides(fill = guide_legend(byrow = TRUE))
  
p + scale_colour_discrete(labels = function(x) str_wrap(x, width = 6)) + scale_shape_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_y_continuous(breaks = round(seq(-200, 100, by = 50),1))

ggsave("CR Point Estimates by Week (1).jpg")


# # (2) ----------
# results <- list()
# 
# for (w in weeks) {
#   
#   t5_reg_5 <- lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B   + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B  + factor(pair), data = bags[after==1][a16_1_gradeA==1][week==w])
#   t5_reg_5$coefficients[2] 
#   
#   results[[w]] <- list(as.numeric(t5_reg_5$coefficients[2]),
#                        as.numeric(lm(pf ~ treat + a6_weightA_B + villages_B + b_sup_sid_B + lshare_sid_B + b4_age_B + b7_2_years_mid_B + nice_floor_B + mobile_B + storage_B, data = bags[after==1][a16_1_gradeA==1][week==w])$coefficients[2]))
#   
# }
# 
# results_dt <- rbindlist(results)
# results_dt[, week := paste0(1960+floor(weeks/52), "w", weeks%%52)]
# setorder(results_dt, "week")
# 
# ggplot(results_dt) +
#   geom_point(aes(x=week, y=V2), color = "red") +
#   geom_point(aes(x=week, y=V1)) +
#   xlab("Week Number") +
#   ylab("Point Estimates") +
#   theme_minimal()
# 
# ggsave("CR Point Estimates by Week (2).jpg")
# 
