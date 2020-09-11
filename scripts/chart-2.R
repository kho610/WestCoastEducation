# Chart 2
library(ggplot2)
library(dplyr)
library(plotly)

edu_demo_data <- read.csv("data/west-education-data.csv", stringsAsFactors = F)

students <- edu_demo_data$students
teachers <- edu_demo_data$teachers
colnames(edu_demo_data)[colnames(edu_demo_data) ==
                          "Student.Teacher.Ratio"] <- "student_teacher_ratio"

chart_2_data <- edu_demo_data %>%
  group_by(Locale) %>%
  summarise(teachers = mean(teachers, na.rm = T),
            students = mean(students, na.rm = T),
            ratio = mean(student_teacher_ratio, na.rm = T)) %>%
  filter(Locale != "N") %>%
  select(teachers, students, ratio, Locale)

# scatter plot

ggplot_scatter <- function(df){
  ggplot(data = df) +
    geom_point(mapping = aes(x = students, y = ratio, color = Locale)) +
    scale_x_continuous() +
    scale_y_continuous() +
    scale_color_discrete() +
    labs(title = "Number of Students vs. Student/Teacher Ratio",
         y = "Student/Teacher Ratio (average)",
         x = "Number of Students (average)", fill = "ENROLL") +
    theme_bw()
}

# convert to plotly
students_ratio_scatter <- ggplot_scatter(chart_2_data)
students_ratio_scatter_plotly <- ggplotly(students_ratio_scatter)
