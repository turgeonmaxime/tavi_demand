
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Demand and resources for TAVIs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("growth",
                   "Growth model:",
                   c("Low", "Medium", "High"),
                   "High"),
      sliderInput("savs",
                  "Percent SAVS:",
                  min = 1,
                  max = 5.5,
                  value = 3.4),
      sliderInput("symp",
                  "Percent symptomatic:",
                  min = 65,
                  max = 85,
                  value = 75.6),
      sliderInput("sts",
                  "Percent with STS > 10:",
                  min = 20,
                  max = 60,
                  value = 44)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)
