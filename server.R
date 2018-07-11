# Define server logic required to draw a histogram
library(tidyverse)
n_years <- 25

server <- function(input, output) {
  
  y <- seq_len(n_years) + 2016
  x1 <- round(400 * (1.015)^seq_len(n_years))
  x2 <- c(cumsum(rep(25, n_years)))
  x3 <- cumsum(c(seq(25, 50, by = 5), 
                 rep(50, n_years - 6)))
  x4 <- cumsum(c(seq(25, 150, by = 25), 
                 rep(150, n_years - 6)))
  
  output$plot <- renderPlot({
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
    
    # Plot graph
    dataset %>% 
      ggplot(aes(x = Year)) +
      geom_line(aes(y = CumCases, colour = Method),
                size = 1) + 
      geom_line(aes(y = Target), size = 1.5) +
      theme_bw() + theme(legend.position = 'top') + 
      geom_vline(xintercept = 2022, linetype = 2) +
      ylab("Cumulative TAVIs since 2016") + 
      scale_colour_manual(values = c("#898d8e", "#c80f2e",  
                                     "#026936")) 
  })
}