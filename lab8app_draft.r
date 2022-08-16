library(shiny)
library(dplyr)
library(fmsb)

char_df <- read.csv("USA_Housing.csv")

#make a variable for the analysis page
analysis <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      h2("Control Panel"),
      selectInput(
        inputId = "char",
        label = "Select  a house age",
        choices = char_df$Character
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",  plotOutput(outputId = "radar")),
        tabPanel("Table", tableOutput(outputId = "table"))
      )
    ))
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
                 tabPanel("Scatter plot", analysis),
                 tabPanel("Bar Chart", h1("TODO")),
                 #tabPanel("Pie Chart", pie)
)

print(ui)

server <- function(input, output){
  radar_table <- function(name){
    data_pt <- filter(char_df, Character == input$char)
    data_pt <- select(data_pt, -c(Character, Class))
    
    min_pt <- summarise_all(char_df, min)
    min_pt <- select(min_pt, -c(Character, Class))
    
    max_pt <- summarise_all(char_df, max)
    max_pt <- select(max_pt, -c(Character, Class))
    
    do.call("rbind", list(max_pt, min_pt, data_pt))
  }
  output$table <- renderTable({
    #filter(char_df, Character == input$char)
    radar_table(input$char)
  })
  
  output$radar <- renderPlot({
    radarchart(radar_table(input$char))
  })
}

shinyApp(ui = ui, server = server)