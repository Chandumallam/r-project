# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Load dataset (update path and dataset name if needed)
weather_data <- read.csv("weather_data.csv")

# Data Cleaning: Adjust as per available columns
cleaned_data <- weather_data %>%
  filter(!is.na(Humidity), !is.na(Pressure), !is.na(WindSpeed), 
         !is.na(MinTemp), !is.na(MaxTemp))

# Shiny UI
ui <- fluidPage(
  # CSS for styling the buttons and background
  tags$style(HTML("
    body {
      background-color: #f0f4fa;
      color: #333;
    }
    .btn-custom {
      background-color: #2b7a78;
      color: white;
      border: none;
      font-weight: bold;
    }
    .btn-custom:hover {
      background-color: #3aafa9;
    }
    .well {
      background-color: #def2f1;
      border-radius: 10px;
    }
    h1 {
      color: #2b7a78;
      font-weight: bold;
    }
  ")),
  
  titlePanel(tags$h1("Weather Data Visualization")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Select Variable:", 
                  choices = c("Humidity", "Pressure", "WindSpeed", "MinTemp", "MaxTemp")),
      sliderInput("bins", "Number of bins for Histogram:", 
                  min = 5, max = 50, value = 10)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("histogramPlot")),
        tabPanel("Boxplot", plotOutput("boxplotPlot"))
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  
  output$histogramPlot <- renderPlot({
    ggplot(cleaned_data, aes_string(x = input$var)) +
      geom_histogram(bins = input$bins, fill = "#3aafa9", color = "#17252a") +
      labs(title = paste("Histogram of", input$var),
           x = input$var, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(color = "#17252a", face = "bold", size = 16),
            axis.title = element_text(color = "#2b7a78"))
  })
  
  output$boxplotPlot <- renderPlot({
    ggplot(cleaned_data, aes_string(y = input$var)) +
      geom_boxplot(fill = "#fe8a71", color = "#17252a") +
      labs(title = paste("Boxplot of", input$var),
           y = input$var) +
      theme_minimal() +
      theme(plot.title = element_text(color = "#17252a", face = "bold", size = 16),
            axis.title = element_text(color = "#2b7a78"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)