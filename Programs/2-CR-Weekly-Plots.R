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

pacman::p_load(data.table, sandwich, lmtest, haven, tidyverse, xtable)

# working directory
setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

# data
bags <- as.data.table(read_dta("Data/data_bags_cleaned_210630.dta"))

# unique weeks in data
weeks <- unique(bags[after==1][a16_1_gradeA==1]$week)

# results list initialization
results <- list()

# regression for every week
for (w in weeks) {
  
  results[[w]] <- list(as.numeric(lm(pf ~ treat + factor(pair), data = bags[week==w][after==1][a16_1_gradeA==1])$coefficients[2]),
                       as.numeric(lm(pf ~ treat, data = bags[week==w][after==1][a16_1_gradeA==1])$coefficients[2]))
  
}

# results data frame
results_dt <- rbindlist(results)
results_dt[, week := paste0(1960+floor(weeks/52), "w", weeks%%52)]

# relabel and reformat things
setorder(results_dt, "week")
setnames(results_dt, c("V1","V2"), c("With Pair Fixed Effects", "Without Pair Fixed Effects"))
results_dt_w <- melt(results_dt)

# deviation from original estimates calculations
results_dt[, `(wo pfe - pfe) / pfe` := abs((`Without Pair Fixed Effects` - `With Pair Fixed Effects`) / abs(`With Pair Fixed Effects`)) * 100
           ][, `(pfe - wo pfe) / wo pfe` := abs((`With Pair Fixed Effects` - `Without Pair Fixed Effects`) / abs(`Without Pair Fixed Effects`)) * 100]

mean(results_dt$`(pfe - wo pfe) / wo pfe`, na.rm=TRUE)

# point estimates plot by week ------
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

# export weekly plot
ggsave("Output/CR Point Estimates by Week (1).jpg")

