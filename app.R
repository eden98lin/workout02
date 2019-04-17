#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyr)

ui <- fluidPage(
  
  titlePanel("Workout02"),
  
  fluidRow(
    column(4,
           sliderInput("init_amount",
                       "Initial Amount",
                       min = 0,
                       max = 100000,
                       step = 500,
                       value = 1000,
                       pre = "$",
                       sep = ",")
    ),
    column(4,
           sliderInput("return_rate",
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 5)
    ),
    column(4,
           sliderInput("year",
                       "Years",
                       min = 0,
                       max = 50,
                       step = 1,
                       value = 20)
    ),
    column(4,
           sliderInput("contrib",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       step = 500,
                       value = 2000,
                       pre = "$")
    ),
    column(4,
           sliderInput("growth_rate",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 2)
    ),
    column(4,
           selectInput("fac",
                       "Facet?",
                       choices = c("No", "Yes")
           )
    )
  ),
  
  mainPanel(
    h4("Timelines"),
    plotOutput("distPlot"),
    h4("Balance"),
    verbatimTextOutput("balance")
    )
  
)

server <- function(input, output) {

  #' @title Future value of investment
  #' @description Calculate the future value of an investment at a specific rate for a number of years
  #' @param amount, the initial amount that is invested
  #' @param rate, the annual return rate
  #' @param years, the time this investment would be kept
  #' @return Future value of this investment
  future_value <- function(amount, rate = 0.05, years = 10){
    return (amount * (1 + rate)^years)
  }
  
  #' @title Annuity
  #' @description Calculate the future value of annuity of an amount at a specific rate for a number of years
  #' @param contrib, contribution: how much you deposit at the end of each year
  #' @param rate, annual rate of return
  #' @param years, time in years this deposit will be in the account
  #' @return Future value of annuity
  annuity <- function(contrib, rate = 0.05, years = 10){
    return (contrib * (((1 + rate) ^ years) - 1) / rate)
  }
  
  #' @title Growing annuity
  #' @description Calculate the future value of gorwing annuity
  #' @param contrib, first contribution: how much you deposit at the end of year 1
  #' @param rate, annual rate of return
  #' @param growth, growth rate
  #' @param years, time in years this deposit will be in the account
  #' @return Future value of growing annuity
  growing_annuity <- function(contrib, rate = 0.05, growth = 0.03, years = 10){
    return(contrib * (((1 + rate) ^ years - (1 + growth) ^ years)/(rate - growth)))
  }
  
  output$distPlot <- renderPlot({
    modalities <- data.frame('year' = 0:input$year, 'no_contrib' = 0:input$year, 'fixed_contrib' = 0:input$year, 'growing_contrib' = 0:input$year)
    
    for (i in 0:input$year){
      modalities$no_contrib[i + 1] <- future_value(input$init_amount, input$return_rate/100, i)
    }
    for (i in 0:input$year){
      modalities$fixed_contrib[i + 1] <- future_value(input$init_amount, input$return_rate/100, i) + annuity(input$contrib, input$return_rate/100, i)
    }
    for (i in 0:input$year){
      modalities$growing_contrib[i + 1] <- future_value(input$init_amount, input$return_rate/100, i) + growing_annuity(input$contrib, input$return_rate/100, input$growth_rate/100, i)
    }
    
    mod_gather <- modalities %>% gather("no_contrib", "fixed_contrib", "growing_contrib", key = "mode", value = "return")
    
    if (input$fac == "No") {
      ggplot(mod_gather, aes(x = year, y = return, col = mode)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        ggtitle('Three Modes of Investing') +
        theme(legend.position = 'right', plot.title = element_text(hjust = 0.5)) +
        xlab('Year') +
        ylab('Return Value')
      
    }
    else{
      # how to make a graph like in the instructions?
      ggplot(mod_gather, aes(x = year, y = return, col = mode)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_area(aes(fill = mode, alpha = 0.5)) +
        ggtitle('Three Modes of Investing') +
        theme(legend.position = 'right', plot.title = element_text(hjust = 0.5)) +
        xlab('Year') +
        ylab('Return Value') +
        facet_grid(~ mode)
    }
    
  })
  output$balance <- renderPrint({
    
    modalities <- data.frame('year' = 0:input$year, 'no_contrib' = 0:input$year, 'fixed_contrib' = 0:input$year, 'growing_contrib' = 0:input$year)
    
    for (i in 0:input$year){
      modalities$no_contrib[i + 1] <- future_value(input$init_amount, input$return_rate/100, i)
    }
    for (i in 0:input$year){
      modalities$fixed_contrib[i + 1] <- future_value(input$init_amount, input$return_rate/100, i) + annuity(input$contrib, input$return_rate/100, i)
    }
    for (i in 0:input$year){
      modalities$growing_contrib[i + 1] <- future_value(input$init_amount, input$return_rate/100, i) + growing_annuity(input$contrib, input$return_rate/100, input$growth_rate/100, i)
    }
    
    print(modalities, digits = 8)
  })
}

shinyApp(ui = ui, server = server)

