
# Charlotte McClintock
# Intergenerational Mobolity by Gender

# ..................................................................................................

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

# cleaning
df <- read_csv("gen.csv")
df <- subset(df, !is.na(mom)|!is.na(daughter))
df <- arrange(df, year)
# create useful variables for sortin
df <- mutate(df, 
             avgchange = (daughter+son)/2 - (dad+mom)/2, 
             pargap=dad-mom, 
             kidgap=son-daughter)
# drop observations with missing values
df <- subset(df, !is.na(avgchange))


# ..................................................................................................

# ui
ui <- shinyUI(fluidPage(theme = "bootstrap.css",
    # recolor sidebar panel & turn off menu bar
    tags$head(tags$style(
        HTML('
         #sidebar {
            background-color: #ffffff; 
        }
             .modebar {
                display: none !important; 
            }')
    )),
    tags$br(),
    sidebarLayout(position="left",
        sidebarPanel(
            id="sidebar",
            tags$h2("Mothers, Daughters, Fathers, Sons"),
            tags$h4("Intergenerational mobility as measured through educational attainment by gender of parent and child"),
            tags$p("How do educational attainment gaps between parents influence educational attainment by sons or daughters?"),
            tags$p("Select a continent, pick some countries, then select a year, or click play to see the animation:"),
            htmlOutput("geo_selector"),
            htmlOutput("country_selector"),
            htmlOutput("sort_selector"),
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
            choices = c(as.character(unique(df$continent)), "All"), 
            selected = "Americas", 
            multiple = F)
    })
    
    
    data <- reactive({
        if (input$geo=="All") {
            data <- df %>% 
                mutate(cnum=as.numeric(as.factor(country)), 
                       arb=max(daughter)+2)
        }
        else {
            data <- df %>% 
                         subset(continent==input$geo) %>% 
                         mutate(cnum=as.numeric(as.factor(country)), 
                          arb=max(daughter)+2)
        }
    })
    
    output$country_selector <- renderUI({
        selectInput(
            inputId = "country", 
            label = "Countries:",
            choices = as.character(unique(data()$country)), 
            selected = as.character(unique(data()$country)),
            multiple = T)
    })
    
    output$sort_selector <- renderUI({
        selectInput(
            inputId = "sort", 
            label = "Sort:",
            choices = c("Average Child Education Difference", 
                        "Parental Gender Gap", 
                        "Child Gender Gap"),
            selected = NULL,
            multiple = T)
    })
    
    final <- reactive({
        filter(as.data.frame(data()), country %in% c(input$country)) %>% 
            filter(year==input$time)
    })


    output$distPlot <- renderPlotly({
        plot_ly(data=as.data.frame(final()), color = I("gray80"), width = 800, 
                height = 250+40*nrow(as.data.frame(final()))) %>%
            add_segments(x = ~mom, xend = ~daughter, y = ~cnum+.2, yend = ~cnum+.2, showlegend = FALSE) %>%
            add_markers(x = ~mom, y = ~cnum+.2, name = "Mother", color = I("purple"), size=2) %>%
            add_markers(x = ~daughter, y = ~cnum+.2, name = "Daughter", color = I("pink"), size=2) %>%
            add_segments(x = ~dad, xend = ~son, y = ~cnum-.1, yend = ~cnum-.1, showlegend = FALSE) %>%
            add_markers(x = ~dad, y = ~cnum-.1, name = "Father", color = I("navy"), size=2) %>%
            add_markers(x = ~son, y = ~cnum-.1, name = "Son", color = I("blue"), size=2) %>%
            add_markers(x = ~arb, y = ~country, name = " ", color = I("white"), yaxis = "y2") %>%
            layout(
                xaxis = list(title = "Mean Years of Education", range = c(.1, 17), side='top'),
                margin = list(l = 150),
                yaxis=list(title="", tickfont=list(color="white")),
                yaxis2 = list(overlaying = "y", side = "left", title = ""),
                legend = list(orientation = 'h', x = 0.2, y = 1)
            ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# add x axis to the top
# select all option for continent?
# options to show biggest gains for girls or boys? biggest gaps?
## sort parent gap, sort child gap, sort mean child
# 10 random countries for ALL continents? 10 top/10 bottom
