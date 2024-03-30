# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Read the movie data
movies <- read.csv("movies_data.csv")

# Define UI for application
ui <- fluidPage(
  titlePanel("Movie Statistics"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText("Create interactive plots based on movie data"),
      selectInput("genre_filter", "Filter by Genre:",
                  choices = c("All", unique(movies$Genre))),
      selectInput("director_filter", "Filter by Director:",
                  choices = c("All", unique(movies$Director))),
      sliderInput("year_filter", "Filter by Year:",
                  min = min(movies$Year), max = max(movies$Year),
                  value = c(min(movies$Year), max(movies$Year)), step = 1),
      sliderInput("rating_filter", "Filter by Rating:",
                  min = min(movies$Rating), max = max(movies$Rating),
                  value = c(min(movies$Rating), max(movies$Rating)), step = 0.1),
      hr(),
      helpText("Note: Not all plots may have valid data for all categories.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Plots",
                 fluidRow(
                   column(6, plotlyOutput("movies_plot")),
                   column(6, plotlyOutput("directors_plot"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("genres_plot"))
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

# Define server logic
server <- function(input, output) {
  
  # Reactive filtering based on genre, director, year, and rating selection
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
      filter(Rating >= input$rating_filter[1], Rating <= input$rating_filter[2])
    return(filtered)
  })
  
  # Top 20 Movies by Votes
  output$movies_plot <- renderPlotly({
    filtered_movies <- filtered_data() %>%
      arrange(desc(Votes)) %>%
      slice(1:20)
    p <- ggplot(filtered_movies, aes(x = reorder(Title, Votes), y = Votes)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Movie", y = "Votes", title = "Top 20 Movies by Votes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Top 20 Directors by Votes
  output$directors_plot <- renderPlotly({
    top_directors <- filtered_data() %>%
      group_by(Director) %>%
      summarise(Total_Votes = sum(Votes)) %>%
      arrange(desc(Total_Votes)) %>%
      slice(1:20)
    p <- ggplot(top_directors, aes(x = reorder(Director, Total_Votes), y = Total_Votes)) +
      geom_bar(stat = "identity", fill = "salmon") +
      labs(x = "Director", y = "Total Votes", title = "Top 20 Directors by Total Votes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Genre Proportions
  output$genres_plot <- renderPlotly({
    genre_proportions <- filtered_data() %>%
      group_by(Genre) %>%
      summarise(Total = n()) %>%
      mutate(Proportion = Total / sum(Total)) %>%
      arrange(desc(Proportion))
    
    p <- plot_ly(data = genre_proportions, labels = ~Genre, values = ~Total, type = 'pie',
                 marker = list(colors = rainbow(length(genre_proportions$Genre))),
                 textposition = 'inside',
                 textinfo = 'percent+label')
    p <- p %>% layout(title = 'Proportions of Different Genres')
    p
  })
  
  # Table of Top Movies
  output$top_movies_table <- renderDataTable({
    filtered_data() %>%
      arrange(desc(Gross_Earning_in_Mil)) %>%
      slice(1:20) %>%
      select(Title, Director, Gross_Earning_in_Mil)
  }, options = list(pageLength = 10))  # Display 10 rows per page
  
  # Summary Table
  output$summary_table <- renderPrint({
    summary(filtered_data())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
