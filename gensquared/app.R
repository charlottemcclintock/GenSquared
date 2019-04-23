

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
library(shinyWidgets)


# ..................................................................................................

df <- read_csv("gen.csv")
df <- subset(df, !is.na(mom)|!is.na(daughter))
df <- arrange(df, year)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = "bootstrap.css",
    tags$head(tags$style(
        HTML('
         #sidebar {
            background-color: #ffffff;
        }
             .modebar {
                display: none !important;
            }')
    )),
    tags$h2("Mothers, Daughters, Fathers, Sons"),
    tags$h4("Intergenerational mobility as measured through educational attainment by gender"),
    tags$br(),
    sidebarLayout(position="left",
        sidebarPanel(
            id="sidebar",
            tags$p("How do educational attainment gaps between parents influence educational attainment by sons or daughters?"),
            tags$p("Select a continent, pick some countries, then select a year, or click play to see the animation:"),
            htmlOutput("geo_selector"),
            htmlOutput("country_selector"),
            htmlOutput("year_selector"),
            width=4
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$year_selector <- renderUI({
        sliderTextInput(inputId = "time", 
                    label = "",
                    choices = unique(df$year),
                    selected = 1980,
                    animate=T,
                    grid=T
                    ) 
    })
    
    
    output$geo_selector <- renderUI({
        selectInput(
            inputId = "geo", 
            label = "Continent:",
            choices = as.character(unique(df$continent)), 
            selected = "A", 
            multiple = F)
    })
    
    
    data <- reactive({
        data <- df %>% 
            filter(continent==input$geo) %>% 
            mutate(cnum=as.numeric(as.factor(country)), 
                   arb=max(daughter)+2)
    })
    
    output$country_selector <- renderUI({
        selectInput(
            inputId = "country", 
            label = "Countries:",
            choices = as.character(unique(data()$country)), 
            selected = as.character(unique(data()$country)),
            multiple = T)
    })
    
    final <- reactive({
        filter(as.data.frame(data()), country %in% c(input$country)) %>% 
            filter(year==input$time)
    })


    output$distPlot <- renderPlotly({
        plot_ly(data=as.data.frame(final()), color = I("gray80"), width = 900, height = 40*nrow(as.data.frame(final()))) %>%
            add_segments(x = ~mom, xend = ~daughter, y = ~cnum+.2, yend = ~cnum+.2, showlegend = FALSE) %>%
            add_markers(x = ~mom, y = ~cnum+.2, name = "Mother", color = I("purple"), size=2) %>%
            add_markers(x = ~daughter, y = ~cnum+.2, name = "Daughter", color = I("pink"), size=2) %>%
            add_segments(x = ~dad, xend = ~son, y = ~cnum-.1, yend = ~cnum-.1, showlegend = FALSE) %>%
            add_markers(x = ~dad, y = ~cnum-.1, name = "Father", color = I("navy"), size=2) %>%
            add_markers(x = ~son, y = ~cnum-.1, name = "Son", color = I("blue"), size=2) %>%
            add_markers(x = ~arb, y = ~country, name = " ", color = I("white"), yaxis = "y2") %>%
            layout(
                xaxis = list(title = "Mean Years of Education", tick0=0),
                margin = list(l = 150),
                yaxis=list(title="", tickfont=list(color="white")),
                yaxis2 = list(overlaying = "y", side = "left", title = ""),
                legend = list(orientation = 'h', y=10)
            ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# add x axis to the top
# select all option for continent?
# options to show biggest gains for girls or boys? biggest gaps?


