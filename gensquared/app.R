
# Charlotte McClintock
# Intergenerational Mobolity by Gender

# ..................................................................................................

# set up: wd, retrieve data

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
            htmlOutput("sort_selector"),
            htmlOutput("country_selector"),
            htmlOutput("year_selector"),
            width=4
            
        ),

        mainPanel(
           plotlyOutput("distPlot")
        )
    )
))

# ..................................................................................................

# server
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
    
# ..................................................................................................
    
    
    output$geo_selector <- renderUI({
        selectInput(
            inputId = "geo", 
            label = "Continent:",
            choices = c(as.character(unique(df$continent)), "All"), 
            selected = "Americas", 
            multiple = F)
    })
    
    output$sort_selector <- renderUI({
        selectInput(
            inputId = "sort", 
            label = "Sort:",
            choices = c("Average Child Education Difference", 
                        "Parental Gender Gap", 
                        "Child Gender Gap", "None"),
            selected = "None",
            multiple = F)
    })
    
    data <- reactive({
        if (input$geo=="All"&is.null(input$sort)) {
            data <- df %>%
                mutate(cnum=as.numeric(as.factor(country)),
                       arb=max(daughter)+2)
        }
        else if (input$sort=="Average Child Education Difference") {
            data <- df %>%
                subset(continent==input$geo) %>%
                mutate(country=factor(country, levels=unique(country[order(avgchange, country)])),
                       cnum=as.numeric(as.factor(country)),
                       arb=max(daughter)+2)
        } 
        else if (input$sort=="Child Gender Gap") {
            data <- df %>%
                subset(continent==input$geo) %>%
                mutate(country=factor(country, levels=unique(country[order(kidgap, country)])),
                       cnum=as.numeric(as.factor(country)),
                       arb=max(daughter)+2)
        }
        else if (input$sort=="Parental Gender Gap") {
            data <- df %>%
                subset(continent==input$geo) %>%
                mutate(country=factor(country, levels=unique(country[order(pargap, country)])),
                       cnum=as.numeric(as.factor(country)),
                       arb=max(daughter)+2)
        }
        else if (input$sort=="None"){
            data <- df %>%
                subset(continent==input$geo) %>%
                mutate(country=factor(country), 
                              cnum=as.numeric(as.factor(country)),
                               arb=max(daughter)+2)
        }
    })
    
# ..................................................................................................
    
    
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
            filter(year==input$time) %>% 
            mutate(country=droplevels(country))
    })

# ..................................................................................................
    
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


# To Do:
# 10 random countries for ALL continents? 10 top/10 bottom
# tool tip?


