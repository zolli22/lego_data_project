# Load libraries
library(shiny)
library(mosaic)
library(tidyverse)
library(glue)
library(lubridate)

# User interface
ui <- fluidPage(
  titlePanel("Consumption by Country"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "country",
                     label = "Enter Countries here",
                     choices = NULL,
                     multiple = TRUE),
      p("Put single space between the names."),
      checkboxGroupInput("food_group", label = "Which Food Groups?", 
                         choices = unique(food_consumption2$food_group),
                         selected = unique(food_consumption2$food_group))
      ),
    mainPanel(plotOutput("graph")
    )
  )
)

# Server function
server <- function(input, output, session){
  
  updateSelectizeInput(session, 'country', 
                       choices = unique(food_consumption2$country), 
                       server = TRUE)
  
  
  food_consumption2reactive <- reactive({
    food_consumption2 %>%
      filter(country %in% c(unlist(str_split(input$country, " "))), 
             food_group %in% input$food_group)
  })
  
  
  output$graph <- renderPlot({
    
    food_consumption2reactive() %>%
      ggplot(mapping = aes(x = country, y = consumption, fill = food_group
                           )) + 
      geom_col() + scale_fill_viridis_d(option = "inferno")
  })
  

  
}

# Creates app
shinyApp(ui = ui, server = server)