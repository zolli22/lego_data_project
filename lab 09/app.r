# Load libraries
library(shiny)
library(mosaic)
library(tidyverse)
library(glue)
library(lubridate)
library(shinythemes)


# User interface
ui <- fluidPage(
  titlePanel("Consumption by Country"),
  fluidPage(theme = shinytheme("lumen"),
            
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "country",
                     label = "Enter Countries here",
                     choices = NULL,
                     multiple = TRUE),
      p("Put single space between the Countires"),
      checkboxGroupInput("food_group", label = "Which Food Groups?", 
                         choices = unique(food_consumption2$food_group),
                         selected = unique(food_consumption2$food_group)),
      "Made by Isabelle Caldwell and Ingrid Zoll"),
    mainPanel(h4("Food consumption: Have you ever wondered what the food diets of other countries are comprised of? 
      Well, look no further! With this web application, you can select countries and view how much meat, 
      animal products, and plant products they consume per person per year. You can also view the CO2 emissions that are produced
      from each type of food product for each country per person per year."), 
h6(" The data used is from https://raw.githubusercontent.com/
      rfordatascience/tidytuesday/master/data/
      2020/2020-02-18/food_consumption.csv"),
      tabsetPanel(type = "tabs",
                  tabPanel("Food Consumption", plotOutput("plot")),
                  tabPanel("Co2 Emissions", plotOutput("plot2"))
                  )
      )
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
  
  
  output$plot <- renderPlot({
    
    food_consumption2reactive() %>%
      ggplot(mapping = aes(x = country, y = consumption, fill = food_group
                           )) + 
      geom_col() + scale_fill_manual(values = c("#A62E13","#1693A5" ,"#67A35D")) + 
      theme_classic() + 
      labs( x = "Country", 
            y = "Consumption: kg/person/year", 
            fill = "Food Group")
    
  })
  
  output$plot2 <- renderPlot({
    
    food_consumption2reactive() %>%
      ggplot(mapping = aes(x = country, y = co2_emmission, fill = food_group
      )) + 
      geom_col() + scale_fill_manual(values = c("#A62E13","#1693A5" ,"#67A35D")) + 
      theme_classic() + 
      labs( x = "Country", 
            y = "Co2 Emissions : kg/person/year", 
            fill = "Food Group")
  
  })
}


# Creates app
shinyApp(ui = ui, server = server)