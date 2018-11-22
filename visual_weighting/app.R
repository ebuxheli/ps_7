#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# attaching libraries
library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(plotly)

# Load data from the rds
hou <- read_rds("flips_hou.rds")
sen <- read_rds("flips_sen.rds")
gov <- read_rds("flips_gov.rds")

# Define UI for application that draws a scatterplot
ui <- fluidPage(
  # Application title
  titlePanel("2018 Midterm Elections: The Flips"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # checkbox with the party
      checkboxGroupInput("data_party", 
                  h3("Party:"), 
                  choices = c("Democratic", "Republican")),
      
      # selection bar with the office
      selectInput("data_office", 
                  h3("Office:"), 
                  choices = c("House"    = "hou", 
                              "Senate"   = "sen", 
                              "Governor" = "gov"))
    ),
    # shows panels
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("About this app", htmlOutput("about")),
                  tabPanel("Scatterplot", plotlyOutput("plot1")))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlotly({
    # filtering data from input
    flip_FILTER <- filter(input$data_office, file_party %in% c(input$data_party))
    
    ggplotly(tooltip = c("text"),
             ggplot(data = flip_FILTER, 
                    aes_string(x = state, y = vot_per, text = district, color = file_party)) + 
               geom_point(position = "jitter") +
               labs(x = "State", 
                    y = "Democratic Advantage Flips", 
                    title = paste("Percent of Flips in Each Voting District")) +
               scale_color_manual(name = "Party", values = c("blue", "red"))) %>% 
      config(displayModeBar = FALSE)
  }) 
  
  output$about <- renderUI({
    
    # summary of the application and instructions (credit to Mrs. Lupion for the idea and some code)
    str1 <- paste("Summary")
    str2 <- paste("This application allows the user to see how party flips played a role in the election
                  by allowing them to see which party the users flipped to. The Party shows which party
                  the voters flipped from. For example, if the legend shows DEMOCRATIC that means the 
                  there was a flip from Democratic to Republican voting based on their polling response.")
    str3 <- paste("Source")
    str4 <- paste("The New York Times Upshot/Sienna Poll and The New York Times Election Results Coverage")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4)))})
    
    # # plotting the filtered data using plotly
    # flip_FILTER %>% 
    #   filter(metric == "Expected" | metric == "Actual") %>% 
    #   plot_ly(x = ~district, y = ~percent, color = ~metric, type = "scatter", 
    #           hoverinfo = 'text',
    #           text = ~paste('District: ', district,
    #                         '\n Percent: ', scales::percent(percent))) %>% 
    #   layout(title = "2018 Midterm Elections: Republican Advantage",
    #          xaxis = list(title = "District"),
    #          yaxis = list(title = "Republican Advantage Percentage",
    #                       tickformat = "%"))
}

# Run the application 
shinyApp(ui = ui, server = server)




