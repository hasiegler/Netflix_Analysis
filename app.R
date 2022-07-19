library(shiny)
library(tidyverse)
library(here)
library(DT)
library(vtable)

ui <- fluidPage(
  
  titlePanel("Data Exploration of Netflix Shows and Movies"),
  
  strong("Henry Siegler"),
  
  tags$br(),
  "The interactive app uses data containing information on all Netflix shows 
  and movies as of May 2022.",
  tags$br(),
  "The app allows the user to explore the characteristics 
  of Netflix shows and movies using both numeric and categorical variables.",
  tags$br(),
  "The original data is from ", em("kaggle.com"), ", with additional data manipulation for the following analysis.",
  tags$br(),
  tags$br(),
  "See the Github link below for data manipulations and code for Shiny App:",
  tags$br(),
  tags$a(href = "https://github.com/hasiegler/Netflix_Analysis/blob/master/app.R",
         "GitHub Code"),
  
  tags$hr(),
  
  selectInput("movie_type",
              "Choose Movie or TV Show for Analysis",
              choices = c("MOVIE", "SHOW"),
              selected = "MOVIE"),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Variable Descriptions")),
           column(width = 9,
                  tableOutput(outputId = "descriptions"))
           ),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Descriptive Statistics")),
           column(width = 9,
                  tableOutput(outputId = "sumstats"))),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Number of Missing Observations by Column"),
                  strong("Total Number of Observations in Analysis"),
                  verbatimTextOutput(outputId = "totalobs"),
                  em("Note: In the graphs below, 
                     if a movie/show is missing a value used in the graph, that observation will be omitted.")),
           column(width = 9,
                  tableOutput(outputId = "missingobs"))),
  
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
                  tags$h3("Univariate Analysis"),
                  strong("Select variable for density plot"),
                  selectInput("densityvar",
                              label = NULL,
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "imdb_score"),
                  strong("Check box to remove outliers"),
                  checkboxInput("outlierbox",
                                label = NULL),
                  tableOutput(outputId = "outliertable")
                  ),
           column(width = 9,
                  plotOutput(outputId = "densityplot"))
           ),
  
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
                              value = c(2000, 2022),
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
                              value = c(2000, 2022),
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
                                          "country"),
                              selected = "country"
                              ),
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
                  plotOutput(outputId = "statbarplot"))),
  
  tags$hr(),
  
  fluidRow(column(width = 3,
                  tags$h3("Scatter Plot"),
                  strong("Select Variable #1 (x-axis)"),
                  selectInput("scatter1",
                              label = NULL,
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "runtime"),
                  strong("Select Variable #2 (y-axis)"),
                  selectInput("scatter2",
                              label = NULL, 
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "imdb_score")),
           column(width = 9, 
                  plotOutput("scatterplot"))
           )
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
                               TRUE ~ country)) %>% 
    select(title, type, release_year, age_certification, runtime, seasons, imdb_score, 
           imdb_votes, tmdb_popularity, tmdb_score, main_genre, country)
  #manipulation end
  
  df_subset <- reactive({
    a <- data %>% 
      filter(type == input$movie_type)
    return(a)
  })
  
  output$descriptions <- renderTable({
    names <- colnames(df_subset())
    descriptions <- c("Title of the film",
                      "Type of film (movie or show)",
                      "Year film was released",
                      "Film content rating",
                      "The length of the episode or movie (minutes)",
                      "Number of seasons",
                      "Score on IMDB (1-10)",
                      "Number of votes on IMDB ",
                      "Popularity on TMDB",
                      "Score on TMDB (1-10)",
                      "First listed genre",
                      "First listed production country")
    data.frame(Column = names,
               Description = descriptions)
  })
  
  output$sumstats <- renderTable({
    df_subset() %>% 
      select_if(is.numeric) %>% 
      st(out = "return")
  })
  
  output$totalobs <- renderPrint({
    nrow(df_subset())
  })
  
  output$missingobs <- renderTable({
    na_count <- df_subset() %>% 
      summarise_all(~sum(is.na(.)))
    Column <- rownames(t(na_count))
    cbind(Column, t(na_count)) %>% 
      as.data.frame() %>% 
      rename(Count = "V2")
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
  
  output$outliertable <- renderTable({
    vector_var <- df_subset() %>% 
      select(!!rlang::sym(input$densityvar)) %>% 
      pull()
    obs_in_row <- df_subset() %>% 
      select(!!rlang::sym(input$densityvar)) %>% 
      drop_na() %>% 
      nrow()
    
    lower_outliers <- sum(vector_var < 
                            summary(vector_var)[2] - (1.5*IQR(vector_var, na.rm = TRUE)), na.rm = TRUE)
    upper_outliers <- sum(vector_var > 
                            summary(vector_var)[5] + (1.5*IQR(vector_var, na.rm = TRUE)), na.rm = TRUE)
    
    outlierdata <- data.frame(first = c("Number of Outliers below Minimum Threshold",
                                        "Number of Outliers above Maximum Threshold"),
                              second = c(lower_outliers, upper_outliers))
    names(outlierdata) <- NULL
    outlierdata <- rbind(outlierdata, c("Total number of Observations", obs_in_row))
    outlierdata
  })
  
  output$densityplot <- renderPlot({
    
    if(input$outlierbox){
      
      no_outliers <- reactive(df_subset() %>% 
        filter(!!rlang::sym(input$densityvar) > summary(!!rlang::sym(input$densityvar))[2] - 
                 (1.5*IQR(!!rlang::sym(input$densityvar), na.rm = TRUE)) 
               & !!rlang::sym(input$densityvar) < summary(!!rlang::sym(input$densityvar))[5] + 
                 (1.5*IQR(!!rlang::sym(input$densityvar), na.rm = TRUE))))
      no_outliers() %>%
        ggplot(aes(x = !!rlang::sym(input$densityvar))) +
        geom_histogram(aes(y = ..density..),
                      fill = "dodgerblue") + 
        geom_density(size = 1) + 
        labs(y = "Density") +
        theme_bw()
      
      } else{
        vector_var <- df_subset() %>% 
          select(!!rlang::sym(input$densityvar)) %>% 
          pull()
        lower <- summary(vector_var)[2] - (1.5*IQR(vector_var, na.rm = TRUE))
        upper <- summary(vector_var)[5] + (1.5*IQR(vector_var, na.rm = TRUE))
        df_subset() %>% 
          ggplot(aes(x = !!rlang::sym(input$densityvar))) +
          geom_histogram(aes(y = ..density..),
                         fill = "dodgerblue") + 
          geom_density(size = 1) + 
          geom_vline(xintercept = lower,
                     size = 1) + 
          geom_vline(xintercept = upper,
                     size = 1) +
          labs(y = "Density",
               subtitle = "Vertical Lines Represent Bounds of Outliers") + 
          theme_bw()
        
      }
    
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
      ggplot(aes(x = fct_infreq(as.factor(!!rlang::sym(input$barvar))))) + 
      geom_bar(fill = "dodgerblue") + 
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(x = input$barvar)
  })
  
  output$groupbarplot <- renderPlot({
    groupdata <- df_subset() %>% 
      select(!!rlang::sym(input$groupbar1),
             !!rlang::sym(input$groupbar2)) %>% 
      drop_na()
    
    groupdata %>% 
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
           y = str_c(input$stat, " ", input$statvar)) + 
      theme_bw()
  })
  
  output$scatterplot <- renderPlot({
    scatterdata <- df_subset() %>% 
      select(!!rlang::sym(input$scatter1), 
             !!rlang::sym(input$scatter2))
    scatterdata %>% 
      ggplot() + 
      geom_point(aes(x = !!rlang::sym(input$scatter1),
                     y = !!rlang::sym(input$scatter2)
                     ),
                 color = "dodgerblue",
                 size = 1) + 
      theme_bw()
  })

  
}

shinyApp(ui = ui, server = server)
