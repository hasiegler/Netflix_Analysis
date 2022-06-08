library(shiny)
library(tidyverse)
library(here)
library(DT)
library(vtable)

ui <- fluidPage(
  
  titlePanel("Exploring the Ratings and Popularity of Netflix Shows and Movies"),
  
  strong("Created by Henry Siegler", tags$br(),
         "California Polytechnic State University, San Luis Obispo"),
  
  tags$br(),
  
  "The interactive app uses data containing information on all Netflix shows 
  and movies as of May 2022. 
  The app allows the user to explore the characteristics 
  of Netflix shows and movies using both numeric and categorical variables",
  
  "The data is from kaggle.com",
  
  tags$hr(),
  
  selectInput("movie_type",
              "Choose Movie or TV Show for Analysis",
              choices = c("MOVIE", "SHOW"),
              selected = "MOVIE"),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Descriptive Statistics")),
           column(width = 9,
                  tableOutput(outputId = "sumstats"))),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Extreme Observations"),
                  strong("Select variable to find extreme observations for:"),
                  selectInput("extreme_var",
                              label = NULL,
                              choices = c("release_year",
                                          "runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "imdb_score")),
           column(width = 9,
                  tableOutput(outputId = "extreme"))),
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Time Trend Graph (Movie/Show Release Year)"),
                  strong("Select variable to display"),
                  selectInput("timetrendvar",
                              label = NULL,
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "imdb_score"),
                  sliderInput("timetrendyear",
                              label = "Choose a range of Release Years",
                              min = 1945,
                              max = 2022,
                              value = c(1945, 2022),
                              sep = ""
                              )
                  ),
           column(width = 9,
                  plotOutput(outputId = "timeplot")
                  )
           ),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Quantile Time Trend Graph"),
                  strong("Select variable to display"),
                  selectInput("timequantvar",
                              label = NULL,
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "imdb_score"),
                  sliderInput("timequantyear",
                              label = "Choose a range of Release Years",
                              min = 1945,
                              max = 2022,
                              value = c(1945, 2022),
                              sep = ""
                  )),
           column(width = 9,
                  plotOutput(outputId = "timequantplot")
                  )
           ),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Bar Chart"),
                  strong("Select factor"),
                  selectInput("barvar",
                              label = NULL,
                              choices = c("release_year",
                                          "age_certification",
                                          "seasons",
                                          "main_genre",
                                          "country")
                              )
                  ),
           column(width = 9,
                  plotOutput(outputId = "barplot"))
           ),
  
  tags$hr(),
  
  fluidRow(column(width = 3, 
                  tags$h3("Grouped Bar Chart"),
                  strong("Select factor for x-axis"),
                  selectInput("groupbar1",
                              label = NULL,
                              choices = c("release_year",
                                          "age_certification",
                                          "seasons",
                                          "main_genre",
                                          "country"),
                              selected = "main_genre"
                              ),
                  strong("Select factor for y-axis"),
                  selectInput("groupbar2",
                              label = NULL,
                              choices = c("release_year",
                                          "age_certification",
                                          "seasons",
                                          "main_genre",
                                          "country"),
                              selected = "age_certification"),
                  strong("Select grouping method"),
                  selectInput("groupoption",
                              label = NULL,
                              choices = c("fill",
                                          "stack"),
                              selected = "fill")
                  ),
           column(width = 9,
                  plotOutput(outputId = "groupbarplot"))),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("By Group Statistics"),
                  strong("Select variable to display"),
                  selectInput("statvar",
                              label = NULL,
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "runtime"),
                  strong("Select variable to group by"),
                  selectInput("groupbyvar",
                              label = NULL,
                              choices = c("release_year",
                                          "age_certification",
                                          "seasons",
                                          "main_genre",
                                          "country"),
                              selected = "age_certification"),
                  strong("Select statistic"),
                  selectInput("stat",
                              label = NULL,
                              choices = c("mean",
                                          "median",
                                          "min",
                                          "max"),
                              selected = "mean")
                  ),
           column(width = 9,
                  plotOutput(outputId = "statbarplot")))
  
  
  
  
  
)


server <- function(input, output) {
  
  data <- read_csv(here("titles.csv/titles.csv"))
  codes <- read_csv(here("countrycodes.csv"))
  
  #manipulation start
  data_genres <- data %>% 
    filter(genres != "[]")
  locations <- str_locate_all(data_genres$genres, "'")
  loc <- c()
  for (i in 1:nrow(data_genres)){
    loc[i] <- as.numeric(locations[[i]][2,1])
  }
  data_genres <- data_genres %>% 
    mutate(main_genre = str_sub(genres, 3, loc - 1))
  data_nogenre <- data %>% 
    filter(genres == "[]") %>% 
    mutate(main_genre = "None")
  data <- data_genres %>% 
    rbind(data_nogenre)
  data_countries <- data %>% 
    filter(production_countries != "[]")
  locations <- str_locate_all(data_countries$production_countries, "'")
  loc <- c()
  for (i in 1:nrow(data_countries)){
    loc[i] <- as.numeric(locations[[i]][2,1])
  }
  data_countries <- data_countries %>% 
    mutate(main_country = str_sub(production_countries, 3, loc - 1))
  data_nocountry <- data %>% 
    filter(production_countries == "[]") %>% 
    mutate(main_country = "None")
  data <- data_countries %>% 
    rbind(data_nocountry)
  codes <- codes %>% 
    select(name, `alpha-2`) %>% 
    rename("main_country" = `alpha-2`)
  data <- left_join(data, codes, by = "main_country")
  countries <- count(data, name) %>% 
    filter(n > 50) %>% 
    pull(name)
  data <- data %>% 
    mutate(country = case_when(name %in% countries ~ name,
                               TRUE ~ "Other")) %>% 
    mutate(country = case_when(country == "Korea, Republic of" ~ "Korea",
                               country == "Taiwan, Province of China" ~ "Taiwan",
                               country == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
                               country == "United States of America" ~ "USA",
                               TRUE ~ country))
  #manipulation end
  
  df_subset <- reactive({
    a <- data %>% 
      filter(type == input$movie_type)
    return(a)
  })
  
  output$sumstats <- renderTable({
    df_subset() %>% 
      select_if(is.numeric) %>% 
      st(out = "return")
  })
  
  output$extreme <- renderTable({
    top <- df_subset() %>% 
      select(title, !!rlang::sym(input$extreme_var)) %>% 
      arrange(desc(!!rlang::sym(input$extreme_var))) %>% 
      head(5)
    low <- df_subset() %>% 
      select(title, !!rlang::sym(input$extreme_var)) %>% 
      arrange(!!rlang::sym(input$extreme_var)) %>% 
      head(5) %>% 
      arrange(desc(!!rlang::sym(input$extreme_var)))
    rbind(top, low)
  })
  
  output$timeplot <- renderPlot({
    time <- df_subset() %>% 
      filter(release_year >= input$timetrendyear[1],
             release_year <= input$timetrendyear[2]) %>% 
      drop_na(!!rlang::sym(input$timetrendvar)) %>% 
      group_by(release_year) %>% 
      summarise(Mean = mean(!!rlang::sym(input$timetrendvar)))
    
    ggplot(time, aes(x = as.integer(release_year), y = Mean)) + 
      geom_point() + 
      geom_line(color = "red") + 
      theme_bw() + 
      labs(x = "Release Year",
           y = str_c("Mean ", as.character(input$timetrendvar)))
  })
  
  output$timequantplot <- renderPlot({
    timequant <- df_subset() %>% 
      filter(release_year >= input$timequantyear[1],
             release_year <= input$timequantyear[2]) %>% 
      drop_na(!!rlang::sym(input$timequantvar)) %>% 
      group_by(release_year) %>% 
      summarize(enframe(quantile(!!rlang::sym(input$timequantvar), c(0.25, 0.5, 0.75)),
                        "quant",
                        "var"))
    
    ggplot(timequant, aes(x = as.integer(release_year), y = var, color = quant)) + 
      geom_point() + 
      geom_line() + 
      theme_bw() + 
      labs(x = "Release Year",
           y = str_c("Quantiles of ", as.character(input$timetrendvar)),
           color = "Quantile") + 
      scale_color_discrete(breaks = c("75%", "50%", "25%"))
  })
  
  output$barplot <- renderPlot({
    df_subset() %>% 
      ggplot(aes(x = as.factor(!!rlang::sym(input$barvar)))) + 
      geom_bar(fill = "dodgerblue") + 
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(x = input$barvar)
  })
  
  output$groupbarplot <- renderPlot({
    df_subset() %>% 
      ggplot(aes(x = as.factor(!!rlang::sym(input$groupbar1)), 
                 fill = as.factor(!!rlang::sym(input$groupbar2)))) + 
      geom_bar(position = input$groupoption) +
      theme_bw() + 
      labs(y = "") +
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(x = input$groupbar1, 
           fill = input$groupbar2)
  })
  
  output$statbarplot <- renderPlot({
    df_subset() %>% 
      ggplot(aes(x = as.factor(!!rlang::sym(input$groupbyvar)),
                 y = !!rlang::sym(input$statvar))) + 
      geom_bar(stat = "summary",
               fun = input$stat, 
               fill = "dodgerblue") + 
      labs(x = input$groupbyvar,
           y = str_c(input$stat, " ", input$statvar))
  })
  
  
}

shinyApp(ui = ui, server = server)
