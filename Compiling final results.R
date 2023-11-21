rm(list = ls())
cat("\014")
options(scipen=999)
# install.packages("pacman") # uncomment if needed
# install.packages("rstudioapi") # uncomment if needed

# set seed for pilot
set.seed(3333)

pacman::p_load(data.table, haven, tidyverse, xtable, rstudioapi, readstata13, readxl, ggplot2, ggbreak, patchwork, rstudioapi)

# run all programs for each empirical application ------------
setwd(paste0(dirname(getSourceEditorContext()$path), "/1. Groh & McKenzie (2016)"))
source("1-GM-attrition-estimates-with-cov.R")
source("1-GM-attrition-estimates.R")

setwd(paste0(dirname(getSourceEditorContext()$path), "/2. Casaburi & Reed (2021)"))
source("2-CR-attrition-estimates-final.R")

setwd(paste0(dirname(getSourceEditorContext()$path), "/3. Dhar, Jain, Jayachandran (2022)"))
source("3-Dhar-Attrition.R")

setwd(paste0(dirname(getSourceEditorContext()$path), "/4. Carter, Laajaj, Yang (2021)"))
source("4-Carter-Attrition.R")

setwd(paste0(dirname(getSourceEditorContext()$path), "/5. Abebe, Caria, Ortiz-Ospina (2021)"))
source("5-Abebe-Attrition.R")

setwd(paste0(dirname(getSourceEditorContext()$path), "/6. Hjort, Moreira, Rao, Santini (2021)"))
source("6-Hjort-Attrition.R")

setwd(paste0(dirname(getSourceEditorContext()$path), "/7. Romero, Sandefur, Sandholtz (2020)"))
source("7-Romero-Attrition.R")

setwd(paste0(dirname(getSourceEditorContext()$path), "/8. Attanasio et al (2020)"))
source("8-Attanasio-Attrition.R")


# getting all results into view ---------------

# working directory
setwd(dirname(getSourceEditorContext()$path))

casaburi <- fread("2. Casaburi & Reed (2021)/casaburi-main-results.csv")[, paper := "Casaburi & Reed (2022)"][, main_method := "sfe"][, outcome := "bags"][1,] # just the first specification is used for main results
dhar <- fread("3. Dhar, Jain, Jayachandran (2022)/dhar-main-results.csv")[, paper := "Dhar et al (2022)"][, main_method := "no sfe"]
carter <- fread("4. Carter, Laajaj, Yang (2021)/carter-main-results.csv")[, paper := "Carter et al (2021)"][, main_method := "sfe"]
abebe <- fread("5. Abebe, Caria, Ortiz-Ospina (2021)/abebe-main-results.csv")[, paper := "Abebe et al (2021)"][, main_method := "sfe"]#[outcome %in% c("Application rate","Cognitive")]
hjort <- fread("6. Hjort, Moreira, Rao, Santini (2021)/hjort-main-results.csv")[, paper := "Hjort et al (2021)"][, main_method := "no sfe"]
romero <- fread("7. Romero, Sandefur, Sandholtz (2020)/romero-main-results.csv")[, paper := "Romero et al (2020)"][, main_method := "sfe"]
annatasio <- fread("8. Attanasio et al (2020)/attanasio-main-results.csv")[, paper := "Annatasio et al (2020)"][, main_method := "no sfe"]

setnames(casaburi, c("Original","Theta"), c("with_sfe","no_sfe"))
casaburi <- casaburi[, .(outcome, no_sfe, with_sfe, paper, main_method, attrition = mean(Attrition, na.rm=TRUE))]

# stack all results
summary_table <- rbind(casaburi, dhar, carter, abebe, hjort, romero, annatasio, fill=TRUE)

# begin to plot results -----------------
summary_table[main_method=="sfe", pct_diff := abs(with_sfe - no_sfe)/abs(with_sfe) * 100
              ][is.na(pct_diff)==TRUE, pct_diff := abs(no_sfe - with_sfe)/abs(no_sfe) * 100]

avg_summary_table <- summary_table[, .(mean(pct_diff), mean(attrition)), .(paper)]

# label paper name + attrition rates
avg_summary_table$paper_label <- paste(avg_summary_table$paper, paste0("(", as.character(round(avg_summary_table$V2,1)), "%)"))

avg_summary_table$attrition_label <- paste0(as.character(round(avg_summary_table$V1,1)), "%")

# draw these results
ggplot(avg_summary_table) +
  scale_y_break(c(100,2300), scales = 1, space = 1.0) +
  geom_point(aes(x=paper_label, y=V1), color = "blue", size = 2, shape = 1) +
  geom_text(aes(x=paper_label, y=V1, label = attrition_label), hjust=-0.15, vjust=0.35) +
  theme_minimal() +
  xlab("Paper \n (Average Attrition Rate in %)") +
  ylab("Average Percentage Change (%)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=12),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.key.size = unit(1.8, 'cm'),
        axis.ticks.y = element_line(),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.line.x.top = element_blank()) +
  guides(fill = guide_legend(byrow = TRUE)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 

ggsave("comparison-studies.jpg")

# save out the final results table
fwrite(summary_table, "summary table of results.csv")
