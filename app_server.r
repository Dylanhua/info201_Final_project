library(shiny)
library(dplyr)
library(fmsb)
library(shinyWidgets)
library(ggplot2)
library(plotly)

char_df <- read.csv("USA_Housing.csv")
char_df$Avg..Area.House.Age <- round(char_df$Avg..Area.House.Age)
char_df$Price <- round(char_df$Price)

server <- function(input, output){
  
  output$scatter <- renderPlot({
    filter_df <- filter(char_df, Price <= input$price)
    scatterPlot <- ggplot(data = filter_df, aes(x = Price, y = Area.Population)) + 
      geom_point(aes(col=Avg..Area.Income))  
    scatterPlot + ggtitle("Graph of House Price and the population in the houses' area")
  })

  output$conclusionScatter <- renderPlot({
    ggplot(data = char_df, aes(x = Price, y = Area.Population)) + geom_point(aes(col=Avg..Area.Income))
  })
  
  output$scatterData <- renderTable({
    filter_df <- filter(char_df, Price <= input$price)
    nearPoints(filter_df, input$hover, xvar = "Price", yvar = "Area.Population")
  })
  
  output$bar_output <- renderPlotly({
    df_group_price <- char_df %>% group_by(Avg..Area.House.Age) %>%
      summarise(total_population = round(sum(Area.Population)))
    filter_df <- filter(df_group_price, Avg..Area.House.Age <= input$bar_input)
    plot_ly(data = filter_df, x = ~Avg..Area.House.Age, y = ~total_population, 
            type = "bar") %>%
      layout(title = "Population In Different House Age")
  })
  
  output$conclusionBar <- renderPlotly({
    df_group_price <- char_df %>% group_by(Avg..Area.House.Age) %>%
      summarise(total_population = round(sum(Area.Population)))
    plot_ly(data = df_group_price, x = ~Avg..Area.House.Age, y = ~total_population, 
            type = "bar")
  })
  
  output$barData <- renderTable({
    filter_df <- filter(df_group_price, Avg..Area.House.Age <= input$bar_input)
    nearPoints(filter_df, input$hover, xvar = "Avg..Area.House.Age", 
               yvar = "total_population")
  })
  
  output$pie_output <- renderPlotly({
    filter_df <- filter(char_df, Avg..Area.House.Age <= input$pie)
    fig <- plot_ly(filter_df, labels = ~Avg..Area.House.Age, values = ~Price, type = 'pie')%>%
      layout(title = "Percentage of house price sum in different house age")
    fig
  })


  output$conclusionPie <- renderPlotly({
    fig <- plot_ly(char_df, labels = ~Avg..Area.House.Age, values = ~Price, type = 'pie')
    fig
  })
}

