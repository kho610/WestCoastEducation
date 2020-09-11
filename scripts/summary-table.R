#Summary Table: Education finances for West Coast states (CA, WA, OR)
#Average revenue and expenditures from 2000-2016, however averages are found
#for every three years. 
 
#Load packages
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

#Read Csv
education_stats <- read.csv("data/education_stats.csv", stringsAsFactors = F)

#Function that takes in a dataframe and two year range specfication. Calculates
#averages values for the range of year by doing group_by and summarise(mean).

col_order <- c("Year", "STATE", "instruction_exp", "service_exp",
               "capital_exp", "other_exp", "total_avg_exp",
               "total_avg_rev", "Outstanding")

edu_spending_table <- function(df, year1, year2){
  sum_table <- df %>%
    filter(YEAR %in% c(year1:year2)) %>%
    group_by(STATE) %>%
    filter(STATE %in% c("CALIFORNIA", "OREGON", "WASHINGTON")) %>%
    summarise(instruction_exp = round(mean(INSTRUCTION_EXPENDITURE)),
              service_exp = round(mean(SUPPORT_SERVICES_EXPENDITURE)),
              capital_exp = round(mean(CAPITAL_OUTLAY_EXPENDITURE)),
              other_exp = round(mean(OTHER_EXPENDITURE)),
              total_avg_exp = round(mean(TOTAL_EXPENDITURE)),
              total_avg_rev = round(mean(TOTAL_REVENUE)),
              Outstanding = round(total_avg_rev - total_avg_exp)) %>%
    mutate(Year = paste0(year1, "-", year2))

  sum_table <- sum_table[, col_order]
  sum_table
}

#Using the function to create multiple tables
finances_1999_01 <- edu_spending_table(education_stats, 1999, 2001)
finances_2002_04 <- edu_spending_table(education_stats, 2002, 2004)
finances_2005_07 <- edu_spending_table(education_stats, 2005, 2007)
finances_2008_10 <- edu_spending_table(education_stats, 2008, 2010)
finances_2011_13 <- edu_spending_table(education_stats, 2011, 2013)
finances_2013_16 <- edu_spending_table(education_stats, 2014, 2016)

#Combining all tables into one summary table and arranging it to be meaningful
edu_fin_summary <- rbind(finances_2002_04, finances_2005_07,
                         finances_2008_10, finances_2011_13, finances_2013_16)

edu_fin_summary <- edu_fin_summary %>%
  arrange(STATE, Year)

#col names for R markdown kable
table_names <- c("Year", "State", "Instruction EXP", "Service EXP",
                 "Capital Layout EXP", "Other EXP",
                 "Total Average Expenditure", "Total Average Revenue",
                 "Outstanding Amount")
