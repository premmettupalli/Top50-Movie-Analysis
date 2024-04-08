# Loading necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)

# Reading the data-set
movies <- readxl::read_xlsx("movies_data.xlsx")

# Defining UI for the dashboard
ui <- fluidPage(
  #title of the dash board
  titlePanel("Movie Statistics"),
  #designing the sidebar layout and sidebar panel for hosting filtering options 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText("Filtering options"),
      #Genre Filter
      selectInput("genre_filter", "Filter by Genre:",
                  choices = c("All", unique(movies$Genre))),
      #Director Filter
      selectInput("director_filter", "Filter by Director:",
                  choices = c("All", unique(movies$Director))),
      #Year Filter
      sliderInput("year_filter", "Filter by Year:",
                  min = min(movies$Year), max = max(movies$Year),
                  value = c(min(movies$Year), max(movies$Year)), step = 1),
      #Rating filter
      sliderInput("rating_filter", "Filter by Rating:",
                  min = min(movies$Rating), max = max(movies$Rating),
                  value = c(min(movies$Rating), max(movies$Rating)), 
                  step = 0.1),
      #Clear Button 
      actionButton("clear_filters", "Clear All Filters"),
      hr(),
      helpText("Note: Not all plots may have valid data for all categories.")
    ),
    #designing layout of the main panel 
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Plots",
                 fluidRow(
                   column(6, plotlyOutput("movies_plot")),
                   column(6, plotlyOutput("directors_plot"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("genres_plot")),
                   column(6, plotlyOutput("correlation_plot"))
                 )
        ),
        tabPanel("Top Movies by Gross Earning",
                 dataTableOutput("top_movies_table")),
        tabPanel("Summary",
                 verbatimTextOutput("summary_table"))
      )
    )
  )
)

# Defining the server logic for the dashboard 
server <- function(input, output, session) {
  
  # Adding functionality to the Genre, Director,Year, and Ratings filters 
  filtered_data <- reactive({
    filtered <- movies
    if (input$genre_filter != "All") {
      filtered <- filtered %>% filter(Genre == input$genre_filter)
    }
    if (input$director_filter != "All") {
      filtered <- filtered %>% filter(Director == input$director_filter)
    }
    filtered <- filtered %>%
      filter(Year >= input$year_filter[1], Year <= input$year_filter[2]) %>%
      filter(Rating >= input$rating_filter[1], 
             Rating <= input$rating_filter[2])
    return(filtered)
  })
  
  # adding functionality to the filter clearing button
  observeEvent(input$clear_filters, {
    updateSelectInput(session, "genre_filter", selected = "All")
    updateSelectInput(session, "director_filter", selected = "All")
    updateSliderInput(session, "year_filter", value = c(min(movies$Year), 
                                                        max(movies$Year)))
    updateSliderInput(session, "rating_filter", value = c(min(movies$Rating), 
                                                          max(movies$Rating)))
  })
  
  #Plot of Top 20 Movies by Votes
  output$movies_plot <- renderPlotly({
    filtered_movies <- filtered_data() %>%
      arrange(desc(Votes)) %>%
      slice(1:20)
    p <- ggplot(filtered_movies, aes(x = reorder(Title, Votes), y = Votes)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Movie", y = "Votes", title = "Top 20 Movies by total Votes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  #Plot of Top 20 Directors by Votes
  output$directors_plot <- renderPlotly({
    top_directors <- filtered_data() %>%
      group_by(Director) %>%
      summarise(Total_Votes = sum(Votes)) %>%
      arrange(desc(Total_Votes)) %>%
      slice(1:20)
    p <- ggplot(top_directors, aes(x = reorder(Director, Total_Votes), 
                                   y = Total_Votes)) +
      geom_bar(stat = "identity", fill = "salmon") +
      labs(x = "Director", y = "Total Votes", 
           title = "Top 20 Directors by Total Votes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  #Pie chart for Genre Proportions
  output$genres_plot <- renderPlotly({
    genre_proportions <- filtered_data() %>%
      group_by(Genre) %>%
      summarise(Total = n()) %>%
      mutate(Proportion = Total / sum(Total)) %>%
      arrange(desc(Proportion))
    
    p <- plot_ly(data = genre_proportions, labels = ~Genre, values = ~Total,
                 type = 'pie',
                 marker = list(colors = 
                                 rainbow(length(genre_proportions$Genre))),
                 textposition = 'inside',
                 textinfo = 'percent+label')
    p <- p %>% layout(title = 'Proportions of Different Genres')
    p
  })
  
  # Table of Movies based on the gross earnings in millions 
  output$top_movies_table <- renderDataTable({
    filtered_data() %>%
      arrange(desc(Gross_Earning_in_Mil)) %>%
      slice(1:20) %>%
      select(Title, Director, Gross_Earning_in_Mil, Genre)
  }, options = list(pageLength = 10)) 
  
  # Summary Table
  output$summary_table <- renderPrint({
    summary(filtered_data())
  })
  
  # Correlation plot between Budgets in million and Profit in million
  output$correlation_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Budget_in_Million,
                                     y = Profit_in_Mil)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Budget (in million)", y = "Profit (in million)", 
           title = "Correlation between Budget and Profit")
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
