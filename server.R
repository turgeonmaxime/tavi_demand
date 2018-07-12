# Define server logic required to draw a histogram
library(tidyverse)
n_years <- 25
baseline_pop <- 36065

server <- function(input, output) {
  y_lim <- reactive({
    if (input$sts > 45 &&
        input$symp > 75 &&
        input$savs > 5) c(0, 1500) else if (input$savs > 5) c(0, 1000) else c(0, 800)
  })
  
  dataset <- reactive({
    rate <- switch(input$growth,
                   "Low" = 1.01,
                   "Medium" = 1.0125,
                   "High" = 1.015)
    # create data
    eligible_pop <- round(baseline_pop * (input$savs/100) *
                            (input$symp/100) * (input$sts/100))
    
    y <- seq_len(n_years) + 2016L
    x1 <- round(eligible_pop * (rate)^seq_len(n_years))
    x2 <- c(cumsum(rep(25, n_years)))
    x3 <- cumsum(c(seq(25, 50, by = 5), 
                   rep(50, n_years - 6)))
    x4 <- cumsum(c(seq(25, 150, by = 25), 
                   rep(150, n_years - 6)))
    
    # create dataframe
    dataset <- bind_rows(
      tibble::tibble(
        Year = y,
        CumCases = x2,
        Target = x1
      ) %>% mutate(Method = "Current"),
      tibble::tibble(
        Year = y,
        CumCases = x3,
        Target = x1
      ) %>% mutate(Method = "5 incr."),
      tibble::tibble(
        Year = y,
        CumCases = x4,
        Target = x1
      ) %>% mutate(Method = "25 incr.")
    ) %>% 
      mutate(CumCases = pmin(CumCases, Target))
  })
  
  output$plot <- renderPlot({
    # Plot graph
    dataset() %>% 
      ggplot(aes(x = Year)) +
      geom_line(aes(y = CumCases, colour = Method),
                size = 1) + 
      geom_line(aes(y = Target), size = 1.5) +
      theme_bw() + theme(legend.position = 'top',
                         text = element_text(size = 20)) + 
      geom_vline(xintercept = 2022L, linetype = 2) +
      ylab("Cumulative TAVIs since 2016") + 
      scale_colour_manual(values = c("#898d8e", "#c80f2e",  
                                     "#026936")) +
      coord_cartesian(ylim = y_lim())
  })
  
  output$table <- renderTable({
    # Percentage caught up with demand by 2022
    percent_cup <- dataset() %>% 
      filter(Year == 2022L) %>% 
      group_by(Method) %>% 
      transmute(Percent = CumCases/Target)
    
    # When will we be caught up?
    date_cup <- dataset() %>% 
      filter(CumCases == Target) %>% 
      group_by(Method) %>% 
      summarise(Year = min(Year, na.rm = TRUE))
    
    full_join(date_cup, percent_cup, 
               by = 'Method') %>% 
      mutate(Year = as.integer(Year),
             Percent = scales::percent(Percent))
  })
}