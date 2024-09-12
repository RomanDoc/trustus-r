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
                        choices =  c(`Доля сделок по ипотеке, вторичка` = 11,
                                     `Доля сделок по ипотеке, первичка` = 10,
                                     `Индекс безналичных платежей` = 5,
                                     `Индекс покупательской активности` = 6,
                                     `Количество внутренних туристов` = 7,
                                     `Онлайн-заявки на кредит` = 9,
                                     `Офлайн-заявки на кредит` = 8,
                                     `Среднемесячная з.п.` = 3,
                                     `Уровень безработицы` = 4,
                                     `Число абонентов` = 2)),
            radioButtons('visual',
                         'Режим визуализации:',
                         choices = c(`распределения показателя` = 'box_plot',
                                     `динамика показателя` = 'line_plot')),
            textInput('color', 'Введите цвет:', 'grey')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput('distPlot')
        )
    )
)

server <- function(input, output) {
  df_rus <- aggregate(cbind(`Число абонентов`, `Среднемесячная з.п.`, `Уровень безработицы`, 
                            Индекс.БП, Индекс.ПА, Количество.ВТ, `Онлайн-заявки`, `Офлайн-заявки`, 
                            `Доля сделок, первичка`, `Доля сделок, вторичка`, Месяц)
                      ~ Месяц, data = df, FUN = mean)
  df_region <- df %>% select(Регион, `Число абонентов`, `Среднемесячная з.п.`, `Уровень безработицы`, 
                             Индекс.БП, Индекс.ПА, Количество.ВТ, `Онлайн-заявки`, `Офлайн-заявки`, 
                             `Доля сделок, первичка`, `Доля сделок, вторичка`, Месяц)
  output$distPlot <- renderPlot({
    if (input$region == 'Россия') {
      x <- df_rus[, as.integer(input$indicator)]
      if (input$visual == 'box_plot') {
        boxplot(x, col = input$color)
      } else {
        y <- 1:12
        plot(x, y)
      }
    } else {
      df_region <- df_region[df_region$Регион == input$region, ]
      x <- df_region[, as.integer(input$indicator)]
      boxplot(x, col = input$color)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
