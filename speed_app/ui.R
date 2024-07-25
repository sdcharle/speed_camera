library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-size: 18px;
      }
      .shiny-input-container {
        font-size: 18px;
      }
    "))
  ),
  titlePanel("Speed Camera Data (For Irasburg, VT)"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("start_date", "Select Start Date:", value = "2024-07-02"),
      dateInput("end_date", "Select Start Date:", value = "2024-07-16")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Hourly Speed Spread", plotOutput("hourly_plot")),
        tabPanel("Daily Speed Spread", plotOutput("daily_plot")),
        tabPanel("Vehicles Per Hour", plotOutput("normalized_counts_plot")),
        tabPanel("About", p("This was made by Steve Charlesworth, a resident of Irasburg. (c) 2024 Good At Data, LLC"))
        
      )
    )
  )
)
