# Chart 2
library(ggplot2)
library(dplyr)
library(plotly)

edu_stats_data <- read.csv("data/education_stats.csv", stringsAsFactors = F)

# function to grab state information
enroll_state <- function(df, state1){
  df %>%
    group_by(YEAR) %>%
    filter(STATE == state1 & (YEAR != "1992" & YEAR != "2017")) %>%
    summarise(enroll_num_state = sum(ENROLL, na.rm = T)) %>%
    select(enroll_num_state, YEAR) %>%
    mutate(index = row_number() - 1)
}

washington <- enroll_state(edu_stats_data, "WASHINGTON")
california <- enroll_state(edu_stats_data, "CALIFORNIA")
oregon <- enroll_state(edu_stats_data, "OREGON")

# join information
combined_wa_or <- left_join(washington, oregon, by = "index")
combined_all <- left_join(combined_wa_or, california, by = "index")

# line chart
ggplot_line_chart <- function(df) {
  ggplot(df, aes(x = YEAR)) +
    geom_line(aes(y = enroll_num_state, color = "CA")) +
    geom_line(aes(y = enroll_num_state.x, color = "WA")) +
    geom_line(aes(y = enroll_num_state.y, color = "OR")) +
    scale_x_continuous(breaks = seq(1993, 2016, 3)) +
    scale_y_continuous(breaks = seq(0, 7000000, 1000000)) +
    labs(title = "Year vs. Student Enrollment (by state)",
         y = "Student Enrollment (millions)", x = "Year") +
    scale_color_discrete(name = "States", labels = list("red" = "CA",
                                                        "blue" = "WA",
                                                        "green" = "OR")) +
    theme_bw()
}

combined_line_chart <- ggplot_line_chart(combined_all)
# convert to plotly
combined_line_chart_plotly <- ggplotly(combined_line_chart)
