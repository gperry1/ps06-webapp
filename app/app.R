library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
freedom <- read_delim("freedom.csv")

# Define UI for application 

ui <- navbarPage("Freedom",
                 tabPanel("About Data", 
                          uiOutput("page1")),
                 tabPanel("Plot",
                          uiOutput("page2")),
                 tabPanel("Table",
                          uiOutput("page3")))
  

# Define server logic required for application

server <- function(input, output) {
  
  output$mytable1 <- renderTable({
    freedom %>%
      sample_n(size = 5) %>%
      select(country_territory, iso3_code, region, year, pr_rating, cl_rating)
  })
  
  output$page1 <- renderUI({
    sidebarLayout(sidebarPanel(
      h3("About this app"),
      p("This app uses data from a collection of key metrics organized and maintained
                 by ",
        strong("Freedom House.")),
      br(),
      p("This dataset contains ",
        em(nrow(freedom)),
        "observations and ",
        em(ncol(freedom)),
        "variables."),
      br(),
      p("To the right, you'll see a small (random) sample of data:"),
      br(),
      p("(For reference, 'pr_rating' indicates a country's measure of political rights and 'cl_rating'
        indicates a country's measure of civil liberties)")),
      mainPanel(tableOutput("mytable1")))
  })
  
  freedom1 <- reactive({
    freedom %>%
      filter(region %in% input$regions)
  })
  
  output$plot1 <- renderPlot ({
    p1 <- freedom1() %>%
      filter(!is.na(year), !is.na(total)) %>%
      group_by(year, region) %>%
      summarize(avg_total = mean(total)) %>%
      ggplot(aes(x = year, y = avg_total, col = region)) +
      geom_point() + 
      labs(title = "Total Freedom Level by Region",
           x = "Years (2005 - 2021)",
           y = "Total Freedom Level") 
    
    p2 <- p1 + geom_smooth(method = "lm")
    
    if (input$cb) {
      p2
}
    else{
      p1
}

})
  
  output$text1 <- renderText({
    freedom1() %>%
      filter(!is.na(year), !is.na(total)) %>%
      group_by(year, region) %>%
      summarize(avg_total = mean(total)) %>%
      pull(avg_total) %>%
      min() %>%
      paste("The minimum freedom level for the selected region(s) is:", .)
  })
  
  output$text2 <- renderText({
    freedom1() %>%
      filter(!is.na(year), !is.na(total)) %>%
      group_by(year, region) %>%
      summarize(avg_total = mean(total)) %>%
      pull(avg_total) %>%
      max() %>%
      paste("The maximum freedom level for the selected region(s) is:", .)
  })
  
  output$page2 <- renderUI({
    sidebarLayout(sidebarPanel(
      p("You can analyze the average total freedom level for different regions. 
      Select the regions you are interested in. 
      To the right, you'll see a yearly scatterplot of the afformentioned data:",
        checkboxInput(
          inputId = "cb",
          label = "Display Trend Lines:",
          value = FALSE),
        checkboxGroupInput(
          inputId = "regions",
          label = "Regions to show:",
          choices = c(unique(freedom$region)),
          selected = "Asia"),
        )),
      mainPanel(plotOutput("plot1"),
        textOutput("text1"),
        textOutput("text2")))
  })
  
  output$mytable2 <- renderTable ({
    freedom %>%
      group_by(year, region) %>%
      summarize(avg_total = mean(total)) %>%
      arrange(desc(avg_total)) %>%
      filter(year == input$years) %>%
      rename(
        "Year" = "year",
        "Region" = "region",
        "Average_Freedom_Level" = "avg_total")
  })
  output$text3 <- renderText({
    freedom %>%
      group_by(year, region) %>%
      summarize(avg_total = mean(total)) %>%
      arrange(desc(avg_total)) %>%
      filter(year == input$years) %>%
      pull(avg_total) %>%
      max() %>%
      paste("The maximum freedom level for this year is:", .)
    
  })
  
  output$text4 <- renderText({
    freedom %>%
    group_by(year, region) %>%
      summarize(avg_total = mean(total)) %>%
      arrange(desc(avg_total)) %>%
      filter(year == input$years) %>%
      pull(avg_total) %>%
      min() %>%
      paste("The minimum freedom level for this year is:", .)
      
  })
  
  output$page3 <- renderUI({
    sidebarLayout(sidebarPanel(
     p("This panel displays the average total freedom for different regions between the years", 
       em("2005"),
       "and",
       em("2021"),
      sliderInput(
        inputId = "years",
        label = "Which Year:",
        min = 2005,
        max = 2021,
        value = 2005))),
    mainPanel(tableOutput("mytable2"),
              textOutput("text4"),
              textOutput("text3")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
