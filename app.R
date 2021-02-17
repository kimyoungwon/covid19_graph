# CSSS 569 Visualizing Data and Models
# Homework 3
# Youngwon Kim

# Loading packagess
library(shiny)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)

# Importing a dataset from online
df.confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header=TRUE)
df.death <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header=TRUE)
df.recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", header=TRUE)

# Chanding datasets for data visualization
df.confirmed.longer <- df.confirmed %>%
                                pivot_longer(
                                    cols = starts_with("X"), 
                                    names_to = "dates", 
                                    values_to = "frequency",
                                    values_drop_na = TRUE
                                )

df.death.longer <- df.death %>%
                                pivot_longer(
                                    cols = starts_with("X"), 
                                    names_to = "dates", 
                                    values_to = "frequency",
                                    values_drop_na = TRUE
                                )

df.recovered.longer <- df.recovered %>%
                                pivot_longer(
                                  cols = starts_with("X"), 
                                  names_to = "dates", 
                                  values_to = "frequency",
                                  values_drop_na = TRUE
                                )

# Transforming variables into dates
df.confirmed.longer$dates <- mdy(str_remove(df.confirmed.longer$dates, "[X]"))
df.death.longer$dates <- mdy(str_remove(df.death.longer$dates, "[X]"))
df.recovered.longer$dates <- mdy(str_remove(df.recovered.longer$dates, "[X]"))

# Getting dates and frequencies of each country
df.confirmed.by.country <-  df.confirmed.longer %>% 
                                group_by(Country.Region, dates) %>% 
                                summarise(frequency_confirmed = sum(frequency)) 

df.death.by.country <-  df.death.longer %>% 
                            group_by(Country.Region, dates) %>% 
                            summarise(frequency_death = sum(frequency)) 

df.recovered.by.country <-  df.recovered.longer %>% 
                            group_by(Country.Region, dates) %>% 
                            summarise(frequency_recovered = sum(frequency)) 

df.combined <- cbind.data.frame(df.confirmed.by.country, df.death.by.country[,3], df.recovered.by.country[,3])
df.combined$mortality_rate <- (df.combined$frequency_death/df.combined$frequency_confirmed)*100
df.combined$mortality_rate <- ifelse(is.nan(df.combined$mortality_rate) == TRUE, 0, df.combined$mortality_rate)

country_names <- as.character(unique(df.combined$Country.Region))
each_date <- unique(df.combined$dates)

## Shiny app

# Define UI for application that draws llne plots and show a table
ui <- fluidPage(
    
    # Application title
    titlePanel("Coronavirus Disease 2019 (COVID-19)"),
    
    
    # Sidebar with slider inputs
    sidebarLayout(
        sidebarPanel(
            helpText("Create graphs with information 
                     from Johns Hopkins."),
    
            selectInput("var",
                        label = "Choose a country to display",
                        choices = country_names,
                        selected = country_names[country_names == "US"]),
    
            sliderInput("range",
                        label = "Dates:",
                        min = min(each_date),
                        max = max(each_date),
                        value = c(min(each_date), max(each_date)))
                    ),
        
        # Show plots and a table using tabset
        mainPanel(
            tabsetPanel(
                tabPanel("Confirmed Case", plotOutput("plot1")),
                tabPanel("Death Case", plotOutput("plot2")),
                tabPanel("Recovered Case", plotOutput("plot3")),
                tabPanel("Mortality Rate", plotOutput("plot4")),
                tabPanel("Table", tableOutput("table"))
                         )
                )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    dataInput <- reactive({
        
        data <- switch(input$var,
                       from = input$range[1],
                       to = input$range[2],
                       auto.assign = FALSE)
    })
    
    # Visualizing the confirmed cases
    output$plot1 <- renderPlot({
        
        df.combined %>%
            filter(Country.Region == input$var) %>%
            filter(dates %in% each_date[each_date >= input$range[1] & each_date <= input$range[2]]) %>%
            ggplot() +
            geom_line(aes(x = dates, y = frequency_confirmed), color = "blue") +
            geom_point(aes(x = dates, y = frequency_confirmed)) +
            labs(x = "Dates",
                 y = "Frequency of Confirmed Case") +
            theme(panel.background = element_rect(fill = "white"),
                  axis.ticks.y = element_blank(),
                  axis.ticks.length = unit(0.4, "char"), 
                  panel.border = element_blank(),
                  aspect.ratio = ((1 + sqrt(5))/2)^(-1),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "gray45", size = 0.2),
                  strip.background = element_blank(),
                  strip.text.x = element_text(size=12.5),
                  strip.text.y = element_blank(), 
                  strip.placement = "outside")
        
    })
    
    # Visualizing the death cases
    output$plot2 <- renderPlot({
        
        df.combined %>%
            filter(Country.Region == input$var) %>%
            filter(dates %in% each_date[each_date >= input$range[1] & each_date <= input$range[2]]) %>%
            ggplot() +
            geom_line(aes(x=dates, y = frequency_death), color = "red") +
            geom_point(aes(x=dates, y = frequency_death)) +
            labs(x = "Dates",
                 y = "Frequency of Death Case") +
            theme(panel.background = element_rect(fill = "white"),
                  axis.ticks.y = element_blank(),
                  axis.ticks.length = unit(0.4, "char"), 
                  panel.border = element_blank(),
                  aspect.ratio = ((1 + sqrt(5))/2)^(-1),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "gray45", size = 0.2),
                  strip.background = element_blank(),
                  strip.text.x = element_text(size=12.5),
                  strip.text.y = element_blank(), 
                  strip.placement = "outside"
            ) 
    })
    
    # Visualizing the recovered cases
    output$plot3 <- renderPlot({
      
      df.combined %>%
        filter(Country.Region == input$var) %>%
        filter(dates %in% each_date[each_date >= input$range[1] & each_date <= input$range[2]]) %>%
        ggplot() +
        geom_line(aes(x=dates, y = frequency_recovered), color = "green") +
        geom_point(aes(x=dates, y = frequency_recovered)) +
        labs(x = "Dates",
             y = "Frequency of Confirmed Case") +
        theme(panel.background = element_rect(fill = "white"),
              axis.ticks.y = element_blank(),
              axis.ticks.length = unit(0.4, "char"), 
              panel.border = element_blank(),
              aspect.ratio = ((1 + sqrt(5))/2)^(-1),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "gray45", size = 0.2),
              strip.background = element_blank(),
              strip.text.x = element_text(size=12.5),
              strip.text.y = element_blank(), 
              strip.placement = "outside"
        )
      
    })
    
    # Visualizing the mortality rates
    output$plot4 <- renderPlot({
      
      df.combined %>%
        filter(Country.Region == input$var) %>%
        filter(dates %in% each_date[each_date >= input$range[1] & each_date <= input$range[2]]) %>%
        ggplot() +
        geom_line(aes(x=dates, y = mortality_rate), color = "orange") +
        geom_point(aes(x=dates, y = mortality_rate)) +
        labs(x = "Dates",
             y = "Frequency of Confirmed Case") +
        theme(panel.background = element_rect(fill = "white"),
              axis.ticks.y = element_blank(),
              axis.ticks.length = unit(0.4, "char"), 
              panel.border = element_blank(),
              aspect.ratio = ((1 + sqrt(5))/2)^(-1),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "gray45", size = 0.2),
              strip.background = element_blank(),
              strip.text.x = element_text(size=12.5),
              strip.text.y = element_blank(), 
              strip.placement = "outside"
        )
      
    })
    
    # Showing the original table
    output$table <- renderTable({
      
        df <- df.combined %>%
          filter(Country.Region == input$var) %>%
          filter(dates %in% each_date[each_date >= input$range[1] & each_date <= input$range[2]])
        
        df$dates <- as.character(each_date[each_date >= input$range[1] & each_date <= input$range[2]])
        
        df
        
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


