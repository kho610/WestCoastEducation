#Packages
library(kableExtra)

# Running the server for interactivity
server <- function(input, output) {

  # Education Finance plot
  output$time_series <- renderPlotly({
    if (input$state_var == "Washington") {
      plot1 <- ggplot(wa_edu_finances,
                      aes_string(x = "YEAR", y = input$money_var)) +
        geom_point(color = "blue") +
        geom_line(linetype = "dashed", color = "green") +
        labs(x = "Years", y = input$money_var,
             title = paste0(input$state_var, " Yearly ", input$money_var)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_y_continuous()
      ggplotly(plot1)

    } else if (input$state_var == "California") {
      plot2 <- ggplot(ca_edu_finances,
                      aes_string(x = "YEAR", y = input$money_var)) +
        geom_point(color = "brown") +
        geom_line(color = "red", linetype = "dotted") +
        labs(x = "Years", y = input$money_var,
             title = paste0(input$state_var, " Yearly ", input$money_var)) +
        theme(plot.title = element_text(hjust = 0.5))
      ggplotly(plot2)

    } else {
      plot3 <- ggplot(or_edu_finances,
                      aes_string(x = "YEAR", y = input$money_var)) +
        geom_point(color = "yellow") +
        geom_line(color = "blue") +
        labs(x = "Years", y = input$money_var,
             title = paste0(input$state_var, " Yearly ", input$money_var)) +
        theme(plot.title = element_text(hjust = 0.5))
      ggplotly(plot3)
    }
  })

  # Lunch Aid Facet Grid Scatter Chart
  output$scatter <- renderPlotly({
    chart2 <- ggplot(student_edu_data) +
      geom_point(aes_string(x = "students", y = input$y_var, color = "State"),
                 size = input$size) +
      labs(x = "Students", y = input$y_var,
           title = paste0("Students v.s. ", input$y_var)) +
      facet_grid(~ State)
    chart2
  })
  # Education Enrollment
  output$bar <- renderPlotly({
    title_chart <- paste0("State Enrollment per Year")
    b <- ggplot(edu_year_df, aes_string(x = "STATE", y = input$y_inp,
                                        fill = "STATE")) +
      geom_col() +
      labs(x = "State", y = "Enrollment", title = title_chart) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    ggplotly(b)
  })
  output$tab <- renderText({
    t <- edu_year_df %>%
      select(STATE, input$y_inp)
    kable(t) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    font_size = 17) %>%
      row_spec(1, bold = FALSE, background = "Salmon") %>%
      row_spec(2, bold = FALSE, background = "LimeGreen") %>%
      row_spec(3, bold = FALSE, background = "SkyBlue")

  })
  #Take-away page summary table
  output$table <- renderDataTable(edu_fin_summary,
                            options = list(
                              pageLength = 15,
                              autoWidth = TRUE,
                              scrollX = TRUE)
                            )
}
