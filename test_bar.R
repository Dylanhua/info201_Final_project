library(shiny)
library(dplyr)
library(fmsb)
library(shinyWidgets)
library(ggplot2)
library(plotly)

char_df <- read.csv("USA_Housing.csv")
char_df$Avg..Area.House.Age <- round(char_df$Avg..Area.House.Age)
char_df$Price <- round(char_df$Price)


df_group_price <- char_df %>% group_by(Avg..Area.House.Age) %>%
  summarise(total_population = round(sum(Area.Population)))

filter_df <- filter(df_group_price, Avg..Area.House.Age == input$bar_input)

plot_ly(data = df_group_price, x = ~Avg..Area.House.Age, y = ~total_population, 
        type = "bar")