library(shiny)
library(dplyr)
library(fmsb)
library(shinyWidgets)
library(ggplot2)

char_df <- read.csv("USA_Housing.csv")
char_df$Avg..Area.House.Age <- round(char_df$Avg..Area.House.Age)

#make a variable for the analysis page
analysis <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h2("Selection"),
      selectInput(
        inputId = "char",
        label = "Select  a house age",
        choices = char_df$Avg..Area.House.Age
      )
    ),
    mainPanel(
      p("This scatter plot is made with the house price as its x value and the 
      area population of the said houses as its y value. The purpose of the 
      chart is to show their relationship between the house prices and the 
      population of the area in which the houses are. From the chart, it is 
      clear that there is a positive correlation between the house price and 
      the population of the area where the houses are. Thus, houses are tend to 
      be more expensive since the high demand due to the high population, 
      possibly for the landowners to earn more profits."),
      plotOutput(outputId = "scatter", brush = "brush"),
      tableOutput(outputId = "data")
    )
  )
)

#define the UI
ui <- navbarPage("USA Housing",
                 tabPanel(
                   "Info",
                   h1("USA Housing Dataset"),
                   p("Houses could be the most valuable asset for most people, and different areas
with different variables will affect the prices of the housing prices of those
areas. To analyze what variables have a direct impact on the housing prices in
an area, a dataset with many values related to various areas' housing markets
are incredible important to accomplish this, and that is what our dataset is
trying to answer by providing tons of data on the US housing market.."),
                  
                 ),
                 # setBackgroundImage(
                 #   src = "https://images.seattletimes.com/wp-content/uploads/2021/12/12272021_real-estate_165832.jpg?d=768x489"
                 # ),
                 tabPanel("Scatter plot", analysis),
                 tabPanel("Bar Chart", h1("TODO")),
                 #tabPanel("Pie Chart", pie)
)

print(ui)

server <- function(input, output){
  
  output$scatter <- renderPlot({
    filter_df <- filter(char_df, Avg..Area.House.Age == input$char)
    ggplot(data = filter_df, aes(x = Price, y = Area.Population)) + geom_point(aes(col=Avg..Area.Income))
  })
  
  output$data <- renderTable({
    filter_df <- filter(char_df, Avg..Area.House.Age == input$char)
    brushedPoints(filter_df, input$brush, xvar = "Price", yvar = "Area.Population")
  })
}

shinyApp(ui = ui, server = server)