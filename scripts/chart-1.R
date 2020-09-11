library(dplyr)
library(plotly)
library(ggplot2)
library(lintr)
library(styler)

west_education_data <- read.csv(
  "data/west-education-data.csv",
  stringsAsFactors = FALSE
)

education_stats <- read.csv(
  "data/education_stats.csv",
  stringsAsFactors = FALSE
)

chart_one <- west_education_data %>%
  group_by(State) %>%
  summarise(
    sum_teachers = mean(teachers, na.rm = TRUE),
    sum_students = mean(students, na.rm = TRUE)
  )

student_teacher_graph <- plot_ly(
  chart_one,
  x = ~State, y = ~sum_teachers, type = "bar", name = "Teachers"
) %>%
  add_trace(y = ~sum_students, name = "Students") %>%
  layout(
    title = "Student/Teacher Average By State",
    yaxis = list(title = "Student/Teacher Average "), barmode = "group"
  )
