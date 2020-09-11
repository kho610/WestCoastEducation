                                             ### Set Up ###

west_coast_data <- read.csv("data/west-education-data.csv",
                            stringsAsFactors = FALSE)
data_2 <- read.csv("data/education_stats.csv", stringsAsFactors = FALSE)

library(dplyr)

colnames(west_coast_data)[colnames(west_coast_data)
                  == "Student.Teacher.Ratio"] <- "student_teacher_ratio"

                                  ### Total Students in the West Coast ###

total_stud <- west_coast_data %>%
  na.omit(students, teachers) %>%
  summarise(total_people = sum(students + teachers)) %>%
  filter(total_people == max(total_people)) %>%
  pull(total_people)

                                   ### Most students in a single school ###

most_stud_school <- west_coast_data %>%
  select(students, State, School.Name) %>%
  na.omit(students) %>%
  filter(students == max(students)) %>%
  pull(students)

                                ### Percent of schools that are charters ###

prct_charter <- west_coast_data %>%
  select(Charter) %>%
  summarise(Charter = round(sum(Charter == "Yes") / length(Charter)
                            * 100, 2)) %>%
  pull(Charter)

                                    ### Average student to teacher ratio ###

mean_student_teacher_r <- west_coast_data %>%
  select(State, School.Name, student_teacher_ratio) %>%
  na.omit(student_teacher_ratio) %>%
  summarise(ratio = round(mean(student_teacher_ratio), 2)) %>%
  pull(ratio)

                    ### Percent of students who get free or reduced lunch ###

prct_free_reduced <- west_coast_data %>%
  na.omit(Free.Lunch, Reduced.Lunch) %>%
  summarise(lunch_prct = round(sum(Free.Lunch + Reduced.Lunch) /
                                 sum(students) * 100, 2)) %>%
  pull(lunch_prct)

            ### Function that passes the data set and individual variable ###

summary_info <- function(data_set, info) {
  print(info)
}

                                          ### Variables for Rmd file ###

tot_stud <- as.integer(summary_info(west_coast_data, total_stud))
most_school <- as.integer(summary_info(west_coast_data, most_stud_school))
prct_char <- as.integer(summary_info(west_coast_data, prct_charter))
mean_stud_teach <- as.integer(summary_info(west_coast_data,
                                           mean_student_teacher_r))
prct_lunch <- as.integer(summary_info(west_coast_data, prct_free_reduced))

### NOTE ###
# I wasn't sure if we needed to include the variables inside the function as 
# I thought that was a little unnecessary. Instead I decided to create
# variables that would pull the specific values and then pass those variables
# into the function.