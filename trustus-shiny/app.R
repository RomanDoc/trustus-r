#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(readxl)

setwd('/Users/yadonistroman/Documents/GitHub/trustus-r/data')

df <- read_excel('total.xlsx')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel('Иследования по основным показателям для банка "TrustUs"'),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("region",
                        "Регион:",
                        choices = c('Россия', unique(df[, 'Регион']))),
            selectInput('indicator',
                        'Показатель:',
                        choices =  c(`Доля сделок по ипотеке, вторичка` = 16,
                                     `Доля сделок по ипотеке, первичка` = 15,
                                     `Индекс БП` = 8,
                                     `Индекс ПА` = 9,
                                     `Количество ВТ` = 10,
                                     `Онлайн-заявки на кредит` = 12,
                                     `Офлайн-заявки на кредит` = 13,
                                     `Среднемесячная з.п.` = 6,
                                     `Уровень безработицы` = 7,
                                     `Число абонентов` = 5)),
            radioButtons('visual',
                         'Режим визуализации:',
                         choices = c(`распределения показателя` = 'box_plot',
                                     `динамика показателя` = 'line_plot')),
            textInput('color', 'Введите цвет:', 'grey')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    if (input$visual == 'boxp_lot') {
      x <- df[, as.integer(input$indicator)]
      y <- df[df$Регион == input$region, ]
      boxplot(x, y, col = input$color)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
