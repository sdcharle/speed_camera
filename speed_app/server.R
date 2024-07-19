library(shiny)
library(DBI)
library(odbc)
library(ggplot2)
library(lubridate)
library(dplyr)
library(RSQLite)
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 24),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )

# Set the custom theme as the default
theme_set(custom_theme)

server <- function(input, output, session) {
  # Connect to the database and load data
  db_path <- "../data/speed_cam.db"
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  speed_data <- odbc::dbGetQuery(db, 'select * from speed ') %>%
    mutate(speed_date = ymd_hms(log_timestamp))
  
  # Get the range of dates from the data
  min_date <- ymd('2024-07-02') #min(speed_data$speed_date)
  max_date <- max(speed_data$speed_date)
  
  # Update the dateInput widget with the min and max dates
  updateDateInput(session, "start_date", 
                  min = min_date, 
                  max = max_date, 
                  value = min_date)
  
  speed <- reactive({
    speed_data %>%
      filter(speed_date >= ymd(input$start_date))
  })
  
  output$hourly_plot <- renderPlot({
    ggplot(speed(), aes(x = hour(speed_date), group = hour(speed_date), y = ave_speed)) + 
      geom_boxplot() +
      ggtitle("Speed Spread by Hour of the Day") +
      labs(x = "Hour", y = "Speed") +
      geom_hline(yintercept = 30, color = "green", linetype = "dashed") +
      geom_hline(yintercept = 40, color = "red", linetype = "dashed") 
  })
  
  output$daily_plot <- renderPlot({
    ggplot(speed(), aes(x = wday(speed_date, label = TRUE), y = ave_speed)) + 
      geom_boxplot() +
      ggtitle("Speed Spread by Day of the Week") +
      labs(x = "", y = "Speed") + 
      geom_hline(yintercept = 30, color = "green", linetype = "dashed") +
      geom_hline(yintercept = 40, color = "red", linetype = "dashed") 
  })
  
  output$normalized_counts_plot <- renderPlot({
    speed_data <- speed()
    
    # Calculate counts per hour by day of the week
    speed_count <- speed_data %>%
      mutate(day_of_week = wday(speed_date, label = TRUE),
             hour_of_day = hour(speed_date)) %>%
      group_by(day_of_week, hour_of_day) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Calculate the number of occurrences of each day of the week in the dataset
    day_occurrences <- speed_data %>%
      mutate(day_of_week = wday(speed_date, label = TRUE)) %>%
      group_by(day_of_week) %>%
      summarise(days_count = n_distinct(floor_date(speed_date, "day")))
    
    # Join the occurrences data with the speed_count data
    normalized_speed_count <- speed_count %>%
      left_join(day_occurrences, by = "day_of_week") %>%
      mutate(normalized_count = count / days_count)
    
    # Plot the normalized data with a distinguishable color palette
    ggplot(normalized_speed_count, aes(x = hour_of_day, y = normalized_count, group = day_of_week, color = day_of_week)) + 
      geom_line() +
      ggtitle("Vehicles per Hour by Day of the Week") +
      labs(x = "Hour of the Day", y = "Vehicles") +
      scale_color_brewer(palette = "Set1")
  })
  
  # Disconnect from the database when the app is closed
  session$onSessionEnded(function() {
    RSQLite::dbDisconnect(db)
  })
}
