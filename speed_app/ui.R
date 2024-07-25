library(shiny)
library(DT)
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
        tabPanel("Speed Spread", plotOutput("daily_plot"),plotOutput("hourly_plot")),
        tabPanel("Vehicles Per Hour", plotOutput("normalized_counts_plot"),
                 includeMarkdown('counts_commentary.md')),
        tabPanel("Top 10 Offenders",
                 DTOutput("offendersTable")  
        ),
        tabPanel("About",     
                 includeMarkdown("about.md")
                 )
      )
    )
  )
)
