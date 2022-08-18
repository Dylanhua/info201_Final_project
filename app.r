library(shiny)
library(dplyr)
library(fmsb)
library(shinyWidgets)
library(ggplot2)
library(plotly)

char_df <- read.csv("USA_Housing.csv")
char_df$Avg..Area.House.Age <- round(char_df$Avg..Area.House.Age)
char_df$Price <- round(char_df$Price)

#make a variable for the analysis page
analysis <- fluidPage(
  titlePanel("House Price and the population in the houses' area"),
  sidebarLayout(
    sidebarPanel(
      h2("Selection"),
      sliderInput(
        inputId = "price",
        label = "Filter by Housing Prices",
        min = min(char_df$Price),
        max = max(char_df$Price),
        value = max(char_df$Price)
      ),
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

bar_chart <-fluidPage(
  titlePanel("Population In Different House Age"),
  sidebarLayout(
    sidebarPanel(
      h2("Control Panel"),
      sliderInput(
        inputId = "bar_input",
        label = "Select  a house age",
        min = min(char_df$Avg..Area.House.Age),
        max = max(char_df$Avg..Area.House.Age),
        value = max(char_df$Avg..Area.House.Age)
      )
      
      
    ),
    mainPanel(
      p("The purpose of this chart is that we want to explore whether older houses
    have more people living in, and what age houses are most preferred by 
    people. Before I draw the chart, I have rounded all the house age to an
    integer so that it will help me to draw and explained the graph better.
    As the graph show, we can easily see the population is increasing from 0 
    years house to 6 years house and then the population is decreasing from 6
    years house to 10 years house. Overall, the sixth years house is the most
    popular house age. We think that People don't like houses that are too new
    or too old."),
      plotlyOutput(outputId = "bar_output")
     # tableOutput(outputId = "data")
    )
  )
)

pie <-fluidPage(
  titlePanel("Percentage of house price sum in different house age"),
  sidebarLayout(
    sidebarPanel(
      h2("Control Panel"),
      selectInput(
        inputId = "pie",
        label = "Select a house age",
        choices = char_df$Avg..Area.House.Age
      )
    ),
    mainPanel(
      p("The third chat is a pie chart which describes the percentage of house price in 
different age of houses from 3 to 10 years.The purpose of this pie chart is 
going to describe what age's house have the most value in the housing market 
and it could help the future house buyers decision. Because house age make a 
huge difference such as the house features, public facility, and the community 
quality. From the chart,we can tell 6 years old houses are most valuable in 
the US housing dataset. "),
      plotOutput(outputId = "pie_output")
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
                 tabPanel("Bar Chart", bar_chart),
                 tabPanel("Pie Chart", pie)
)

            
server <- function(input, output){
  
  output$scatter <- renderPlot({
    filter_df <- filter(char_df, Price <= input$price)
    ggplot(data = filter_df, aes(x = Price, y = Area.Population)) + geom_point(aes(col=Avg..Area.Income))
  })
  
  output$data <- renderTable({
    filter_df <- filter(char_df, Price <= input$price)
    brushedPoints(filter_df, input$brush, xvar = "Price", yvar = "Area.Population")
  })

  output$bar_output <- renderPlotly({
    df_group_price <- char_df %>% group_by(Avg..Area.House.Age) %>%
      summarise(total_population = round(sum(Area.Population)))
    
    filter_df <- filter(df_group_price, Avg..Area.House.Age <= input$bar_input)
    
    plot_ly(data = filter_df, x = ~Avg..Area.House.Age, y = ~total_population, 
            type = "bar")
#    barplot(filter_df$Avg..Area.House.Age)

#    g <- ggplot(df_group_price(), aes( y = ,Area.Population , x = Avg..Area.House.Age ))
#    g + geom_bar
  })
  
  output$pie_output <- renderPlot({
    filter_df <- filter(char_df, Avg..Area.House.Age <= input$pie)
    fig <- plot_ly(filter_df, labels = ~Avg..Area.House.Age, values = ~Price, type = 'pie')
    # fig <- fig %>% layout(title = 'Percentage of house price sum in different house age',
    #                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    fig
  })
  
}

shinyApp(ui = ui, server = server)