# Packages
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

# Csv Files
edu_df <- read.csv("data/education_stats.csv", stringsAsFactors = F)

#------------------------------------------------------------------------------
# Page 1 content: Data Manipulation/Widget Set Up (Kenton)

# Function That Creates Financials DF for chosen state
edu_table <- function(choose_state){
  edu_df %>%
    filter(STATE %in% c(choose_state)) %>%
    select(STATE, YEAR, TOTAL_REVENUE, FEDERAL_REVENUE, STATE_REVENUE,
           LOCAL_REVENUE, TOTAL_EXPENDITURE, INSTRUCTION_EXPENDITURE,
           SUPPORT_SERVICES_EXPENDITURE, OTHER_EXPENDITURE,
           CAPITAL_OUTLAY_EXPENDITURE) %>%
    filter(YEAR %in% c(2000:2016))
}

# Table for WA, CA, OR
wa_edu_finances <- edu_table("WASHINGTON")
ca_edu_finances <- edu_table("CALIFORNIA")
or_edu_finances <- edu_table("OREGON")

# Widgets
# State selection input
state_input <- selectInput(
  "state_var",
  label = "States",
  choices = c("Washington", "California", "Oregon"),
  selected = "Washington"
)

# Y-axis input
money_input <- selectInput(
  "money_var",
  label = "Proceeds And Expenses Selection",
  choices = c("TOTAL_REVENUE", "FEDERAL_REVENUE", "STATE_REVENUE",
              "LOCAL_REVENUE", "TOTAL_EXPENDITURE", "INSTRUCTION_EXPENDITURE",
              "SUPPORT_SERVICES_EXPENDITURE", "OTHER_EXPENDITURE",
              "CAPITAL_OUTLAY_EXPENDITURE"),
  selected = "TOTAL_REVENUE"
)
#------------------------------------------------------------------------------
# Page 2 content: Data Manipulation/Widget Set Up (Matt)

# Read CSV file
edu_demo_data <- read.csv("data/west-education-data.csv", stringsAsFactors = F)

colnames(edu_demo_data)[colnames(edu_demo_data) ==
                          "Student.Teacher.Ratio"] <- "student_teacher_ratio"
colnames(edu_demo_data)[colnames(edu_demo_data) ==
                          "Free.Lunch"] <- "free_lunch"
colnames(edu_demo_data)[colnames(edu_demo_data) ==
                          "Reduced.Lunch"] <- "reduced_lunch"

page_2_data <- edu_demo_data %>%
  select(students, teachers, student_teacher_ratio, free_lunch, reduced_lunch,
         State)

student_edu_data <- na.omit(page_2_data)
selected_values <- colnames(student_edu_data)

# Widgets
y_input <- selectInput(
  "y_var",
  label = "Y Variable",
  choices = selected_values,
  selected = "free_lunch"
)

size_input <- sliderInput(
  "size",
  label = "Size of point", min = 1, max = 10, value = 1
)

#------------------------------------------------------------------------------
# Page 3 content:  Data Manipulation/Widget Set Up (Luke)

# Data Manipulation

narrow <- edu_df %>%
  filter(STATE %in% c("WASHINGTON", "OREGON", "CALIFORNIA")) %>%
  select(STATE, YEAR, ENROLL) %>%
  filter(YEAR %in% c(1993:2016))


long <- spread(narrow, key = "YEAR", value = "ENROLL")

edu_year_df <- long %>%
  select("x1993" = `1993`, "x1994" = `1994`,
         "x1995" = `1995`, "x1996" = `1996`, "x1995" = `1995`, "x1996" = `1996`,
         "x1997" = `1997`, "x1998" = `1998`, "x1999" = `1999`, "x2000" = `2000`,
         "x2001" = `2001`, "x2002" = `2002`, "x2003" = `2003`, "x2004" = `2004`,
         "x2005" = `2005`, "x2006" = `2006`, "x2007" = `2007`, "x2008" = `2008`,
         "x2009" = `2009`, "x2010" = `2010`, "x2011" = `2011`, "x2012" = `2012`,
         "x2013" = `2013`, "x2014" = `2014`, "x2015" = `2015`, "x2016" = `2016`,
         STATE)


# Creating Widgets

states <- c("WASHINGTON", "OREGON", "CALIFORNIA")

y_axis <- selectInput("y_inp", label = "Year",
                      choices = colnames(edu_year_df[, -25]))


#-----------------------------------------------------------------------------
### UI settings

# Image
picture <- mainPanel(
  img("",
      src = paste0("https://thumbor.forbes.com/thumbor/960x0/https%3A%2F%2F",
                   "specials-images.forbesimg.com%2Fdam%2Fimageserve%",
                   "2F1040138812%2F960x0.jpg%3Ffit%3Dscale"),
      width = "800", height = "500")
)

# Introduction paragraph
sidebar_one <- sidebarPanel(
  p("This project looks to compare public
    school data between States on the
    West Coast. More specifically,
    the project aims to make conclusions
    on areas of improvement within Washington,
    Oregon, and California and display them in
    a meaningful way. This is important
    because well displayed data that can be a
    powerful tool to draw attention toward
    issues within the education system. This
    data focuses on three different
    categories; enrolment, financials,
    and academic demographics.
    Our Data is taken from the
    National Center for Education Statistics, the United
    States Census Bureau's annual survey of
    school system finances, and the Nations
    report card that provides national and
    state results in main school subjects. The
    scope of this study covers three main questions.
    The first is what is the difference between
    States in terms of free and reduced lunches?
    The second is where do the educational proceeds and spendings go towards
    for each West Coast state? The last quetion is what are the differences
    in grade school student enrollment over the past decade?")
)

# About page (Russel)
home_page <- tabPanel(
  titlePanel("About"),
  h1("West Coast Education"),
  sidebarLayout(
    sidebar_one,
    picture
  )
)

#------------------------------------------------------------------------------
# Interactive Page 1
finance_sidebar_content <- sidebarPanel(
  h4("This interactive time series chart intends to display specfic educational
     revenue and expenditure categories for the states of California,
     Washington, and Oregon."),
  state_input,
  money_input
  )

finance_main_content <- mainPanel(
  plotlyOutput("time_series")
)

page_1 <- tabPanel(
  titlePanel("Finances"),
  headerPanel("West Coast Financal Data"),
  sidebarLayout(
    finance_sidebar_content,
    finance_main_content
  )
)

#-----------------------------------------------------------------------------
# Interactive Page 2

# Page Layout
scatter_sidebar_content <- sidebarPanel(
  h4("This page aims to understand the difference in the number of free and
     reduced lunches given to students between states."),
  y_input,
  size_input
  )
scatter_main_content <- mainPanel(
  plotlyOutput("scatter")
)

page_2 <- tabPanel(
  titlePanel("Lunch Aid"),
  headerPanel(title = "West Coast Student Lunch Aid"),
  sidebarLayout(
    scatter_sidebar_content,
    scatter_main_content
  )
)


#------------------------------------------------------------------------------
# Interactive Page 3

enroll_sidebar_content <- sidebarPanel(
  h4("This page aims to tell us the total education enrollment of
     the states of Washington, Oregon, and California for any given year
     between 1993 and 2016. We additionally wanted to see how these
     numbers changed over time."),
  y_axis
  )

enroll_main_content <- mainPanel(
  plotlyOutput("bar"), tableOutput("tab")
)

page_3 <- tabPanel(
  titlePanel("Enrollment"),
  headerPanel("State Education Enrollment per Year"),
  sidebarLayout(
    enroll_sidebar_content,
    enroll_main_content
  )
)

#------------------------------------------------------------------------------
# Take-away page
source("scripts/chart-1.R")
source("lunch_aid_chart_2.R")
source("scripts/chart-3.R")
source("scripts/summary-table.R")

takeaway_sidebar <- sidebarPanel(
  h1("Main Takeaways"),
  h3("Financial Takeaway"),
  p("When comparing all three states (ref summary table), we can see that
    California's educational finances accumulated the largest
    total expenditures. We can also see that for awhile they've face a lot of
    outstanding debt. This could be interpreted as a funding flaw California
    faces within their educational system."),
  p("Oregon and Washington also faced some financials issues throughout 2002 to
    2013. It wasn't until recently, around 2014-2016, that these states started
    to end their year debt free."),
  h3("Lunch Aid Takeaway"),
  p("The main insight from this data is that California has more variation in
the number of students that receive free or reduced lunches than Washington or
Oregon do. Since California is so big, this could mean that districts or schools
vary in the ways that they are able to give free or reduced lunches to students.
California also has more schools that give almost all the students free or
reduced lunches. Washington and Oregon have a tighter spread of data points.
Washington and Oregon have less schools that give out as many free or reduced
    lunches to students."),
  h3("Enrollment Takeaway"),
  p("From the information we gathered of education enrollment on the West Coast
we learned that Washington and Oregon present very similar enrollment lines.
Both of these States had a slight increase in overall enrollment since the year
1993, however has remained fairly consistent."),
  p("Another key feature of the graph was that between the years 1993 and 1994,
enrollment in California decreased by over one million students. Subsequently,
enrollment then increased by one million students the year after, from 1994 to
1995, causing a spike in the graph. California has had over six million
students for a majority of the time presented in the data, while Washington
and Oregon hover around one million and 500,000 students respectively.") )

takeaway_main <- mainPanel(
  h2("Further Findings"),
  h3("Financial Summary"),
  dataTableOutput("table"),
  h3("Free Lunches by Locale"),
  students_ratio_scatter_plotly,
  h3("Enrollment Chart"),
  combined_line_chart_plotly

)

conclusion_page <- tabPanel(
  titlePanel("Take-Away"),
  sidebarLayout(
    takeaway_sidebar,
    takeaway_main)
)

#------------------------------------------------------------------------------
# UI: Compile all pages
ui <- fluidPage(
  includeCSS("styles.css"),
  navbarPage(
  theme = shinytheme("cosmo"),
  titlePanel(""),
  home_page,
  page_1,
  page_2,
  page_3,
  conclusion_page
))
