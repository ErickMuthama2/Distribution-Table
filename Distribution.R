library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Probability Distributions"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Select a Distribution:",
                  choices = c("Normal", "Exponential")),
      numericInput("n", "Sample Size:", value = 100),
      sliderInput("mean", "Mean:", min = 0, max = 100, value = 50),
      sliderInput("sd", "Standard Deviation:", min = 1, max = 50, value = 10),
      actionButton("generate", "Generate Data")
    ),
    
    mainPanel(
      plotOutput("histogram")
    )
  )
)

server <- function(input, output) {
  
  data <- eventReactive(input$generate, {
    dist <- switch(input$dist,
                   "Normal" = rnorm(input$n, mean = input$mean, sd = input$sd),
                   "Exponential" = rexp(input$n, rate = 1/input$mean))
    data.frame(x = dist)
  })
  
  output$histogram <- renderPlot({
    data_df <- data()
    ggplot(data_df, aes(x = x)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      labs(title = input$dist, x = "Value", y = "Frequency") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)
