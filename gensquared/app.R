

# set up: wd, retrieve data
rm(list=ls())
getwd()


library(knitr)
library(tidyverse)
library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(markdown)
library(rsconnect)

# ..................................................................................................

df <- read_csv("gen.csv")
df <- subset(df, !is.na(mom)|!is.na(daughter))

df$year <- as.character(df$year)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            htmlOutput("year_selector"),
            htmlOutput("geo_selector"),
            width=2
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$year_selector <- renderUI({
        selectInput(
            inputId = "time",
            label = "Year:",
            choices = as.character(unique(df$year)),
            selected = "1980")
    })
    
    output$geo_selector <- renderUI({
        selectInput(
            inputId = "geo", 
            label = "Continent:",
            choices = as.character(unique(df$continent)), 
            selected = "Americas", 
            multiple = F)
    })
    
    data <- reactive({
                df %>% 
            filter(year==input$time) %>%
            filter(continent==input$geo)
    })

    output$distPlot <- renderPlotly({
        plot_ly(data=as.data.frame(data()), color = I("gray80")) %>%
            add_segments(x = ~mom, xend = ~daughter, y = ~country, yend = ~country, showlegend = FALSE) %>%
            add_markers(x = ~mom, y = ~country, name = "Mother", color = I("purple")) %>%
            add_markers(x = ~daughter, y = ~country, name = "Daughter", color = I("pink")) %>%
            add_segments(x = ~dad, xend = ~son, y = ~country, yend = ~country, showlegend = FALSE, yaxis = "y2") %>%
            add_markers(x = ~dad, y = ~country, name = "Father", color = I("navy"), yaxis = "y2") %>%
            add_markers(x = ~son, y = ~country, name = "Son", color = I("blue"), yaxis = "y2") %>%
            layout(
                autosize = F, width = 800, height = 20*nrow(as.data.frame(data())),
                xaxis = list(title = "Mean Years of Education"),
                margin = list(l = 150),
                yaxis=list(title="", tickfont=list(color="white")),
                yaxis2 = list(overlaying = "y", side = "left", title = "")
            ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
