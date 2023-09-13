library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load datasets
gini <- read.csv("gini.csv")
lex <- read.csv("lex.csv")
sug <- read.csv("sugar_per_person.csv")

# Function to remove the first character from column names
remove_first_character <- function(df) {
  names(df) <- sub("^X", "", names(df))
  return(df)
}

gini <- remove_first_character(gini) %>% gather(year, gini, -country)
lex <- remove_first_character(lex)  %>% gather(year, lex, -country)
sug <- remove_first_character(sug) %>% gather(year, sug, -country)


# Merge datasets by country and filter years

data <- gini %>%
  inner_join(lex, by = c("country", "year")) %>%
  inner_join(sug, by = c("country", "year"))%>%
  mutate(year = as.numeric(year))

# Define the UI

ui <- fluidPage(
  titlePanel("Comparison of Inequality, Sugar Consumption, and Life Expectancy Around the World"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Countries"),
      selectInput("country1", "Country 1", choices = unique(data$country)),
      selectInput("country2", "Country 2", choices = unique(data$country)),
      selectInput("country3", "Country 3", choices = unique(data$country)),
      hr(),
      h4("Select Data"),
      selectInput("data1", "Data 1", choices = c("gini", "lex", "sug")),
      selectInput("data2", "Data 2", choices = c("gini", "lex", "sug")),
      sliderInput("years", "Select Years", min = min(data$year), max = max(data$year), value = c(1970, 2018)),
      actionButton("update_button", "Update Plot"),
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        # First tab: Evolution through time
        tabPanel("Evolution through time",
                 fluidRow(
                   plotOutput("plot1"),
                   plotOutput("plot2")
                 )
        ),
        # Second tab: Regression and Prediction
        tabPanel("Regression and Prediction",
                 fluidRow(
                   plotOutput("plot3"),
                   h3("Regression Coefficient"),
                   verbatimTextOutput("prediction_output")
                 )
        ),
        #third tab
        tabPanel("Data and Terms",
                 h3("GINI"),
                 h4("Shows income inequality in a society. A higher number means more inequality"),
                 h3("SUG"),
                 h4("Shows the quantity of sugar consumed in grams per person per day"),
                 h3("LEX"),
                 h4("Shows life expectancy at birth")
        )
      )
    )
  )
)




getwd()

# Define the server logic
server <- function(input, output, session) {
  # Create a reactive dataset filtered by selected countries and years
  filtered_data <- reactive({
    filtered <- data %>%
      filter(country %in% c(input$country1, input$country2, input$country3),
             year >= input$years[1], year <= input$years[2])
    return(filtered)
  })
  
  # Create a regression model between selected "data1" and "data2"
  model <- reactive({
    lm(formula = paste(input$data1, "~", input$data2), data = filtered_data())
  })
  
  # Plot 1: Line graph of "data1" for selected countries and years
  output$plot1 <- renderPlot({
    filtered <- filtered_data()
    ggplot(filtered, aes(x = year, y = .data[[input$data1]], color = country)) +
      geom_line() +
      labs(title = paste("Line Graph of", input$data1),
           x = "Year",
           y = input$data1)
  })
  
  # Plot 2: Line graph of "data2" for selected countries and years
  output$plot2 <- renderPlot({
    filtered <- filtered_data()
    ggplot(filtered, aes(x = year, y = .data[[input$data2]], color = country)) +
      geom_line() +
      labs(title = paste("Line Graph of", input$data2),
           x = "Year",
           y = input$data2)
  })
  
  # Plot 3: Scatter plot with regression line between "data1" and "data2"
  output$plot3 <- renderPlot({
    model_data <- filtered_data()
    ggplot(model_data, aes(x = .data[[input$data1]], y = .data[[input$data2]])) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = paste("Regression Plot between", input$data1, "and", input$data2),
           x = input$data1,
           y = input$data2)
  })
  
  # Calculate and display the regression coefficient as a prediction
  output$prediction_output <- renderText({
    coef(model())[2]  # Index 2 corresponds to the coefficient for input$data2
  })
}

# Create the Shiny app object and run it
shinyApp(ui, server)


