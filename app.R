library(shiny)
library(tidyverse)
library(here)
library(DT)
library(vtable)

#this code is where the shiny app layout is designed
ui <- fluidPage(
  
  titlePanel("Interactive App to Explore the Characteristics of Netflix TV Shows and Movies"),
  
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
  "See the link for the Shiny App code:",
  tags$br(),
  tags$a(href = "https://github.com/hasiegler/Netflix_Analysis/blob/master/app.R",
         "Code for Shiny App"),
  tags$br(),
  "See the link for the data cleaning and manipulations:",
  tags$br(),
  tags$a(href = "https://github.com/hasiegler/Netflix_Analysis/blob/master/data_manipulation.md",
         "Code for Data Cleaning"),
  
  tags$hr(),
  
  #create user input for movie type
  selectInput("movie_type",
              "Choose Movie or TV Show for Analysis",
              choices = c("MOVIE", "SHOW"),
              selected = "MOVIE"),
  
  tags$hr(),
  
  #add in the variable descriptions table
  fluidRow(column(width = 3,
                  tags$h3("Variable Descriptions")),
           column(width = 9,
                  tableOutput(outputId = "descriptions"))
           ),
  
  tags$hr(),
  
  #add in the descriptive statistics table
  fluidRow(column(width = 3,
                  tags$h3("Descriptive Statistics")),
           column(width = 9,
                  tableOutput(outputId = "sumstats"))),
  
  tags$hr(),
  
  #add the number of missing observations by variable table
  fluidRow(column(width = 3,
                  tags$h3("Number of Missing Observations by Column"),
                  strong("Total Number of Observations in Analysis"),
                  verbatimTextOutput(outputId = "totalobs"),
                  em("Note: In the graphs below, 
                     if a movie/show is missing a value used in the graph, that observation will be omitted.")),
           column(width = 9,
                  tableOutput(outputId = "missingobs"))),
  
  tags$hr(),
  
  #add interactive table that shows unusual observations for the selected variable
  fluidRow(column(width = 3,
                  tags$h3("Extreme Observations"),
                  strong("Select variable to find extreme observations for:"),
                  #create user input
                  selectInput("extreme_var",
                              label = NULL,
                              #here are the user's options
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
  
  #add in the density plot
  fluidRow(column(width = 3,
                  tags$h3("Univariate Analysis"),
                  strong("Select variable for density plot"),
                  selectInput("densityvar",
                              label = NULL,
                              #user choices for which variable to select
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "imdb_score"),
                  strong("Check box to remove outliers"),
                  #add in box that the user can check to remove outliers
                  checkboxInput("outlierbox",
                                label = NULL),
                  tableOutput(outputId = "outliertable"),
                  em("Note: Outlier defined as a value more than 1.5 IQR below Q1
                     or more than 1.5 IQR above Q3.")
                  ),
           column(width = 9,
                  #add the plot
                  plotOutput(outputId = "densityplot"))
           ),
  
  tags$hr(),
  
  #add in the time trend graph and user inputs
  fluidRow(column(width = 3,
                  tags$h3("Time Trend Graph (Movie/Show Release Year)"),
                  strong("Select variable to display"),
                  selectInput("timetrendvar",
                              label = NULL,
                              #variables that the user can select
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "imdb_score"),
                  #this is a slider that the user can change to select the movie release years
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
  
  #add the time trend graph that shows the quantiles of the variable selected
  fluidRow(column(width = 3,
                  tags$h3("Quantile Time Trend Graph"),
                  strong("Select variable to display"),
                  selectInput("timequantvar",
                              label = NULL,
                              #variables that the user can select
                              choices = c("runtime",
                                          "seasons",
                                          "imdb_score",
                                          "imdb_votes",
                                          "tmdb_popularity",
                                          "tmdb_score"),
                              selected = "imdb_score"),
                  #this is a slider that the user can change to select the movie release years
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
  
  #add in barchart with the counts of all of the factors of the selected categorical variable
  fluidRow(column(width = 3,
                  tags$h3("Bar Chart"),
                  strong("Select factor"),
                  selectInput("barvar",
                              label = NULL,
                              #variables the user can select
                              choices = c("release_year",
                                          "age_certification",
                                          "seasons",
                                          "main_genre",
                                          "country"),
                              selected = "country"
                              ),
                  ),
           column(width = 9,
                  #add the plot
                  plotOutput(outputId = "barplot"))
           ),
  
  tags$hr(),
  
  #add in the grouped bar chart graph
  fluidRow(column(width = 3, 
                  tags$h3("Grouped Bar Chart"),
                  strong("Select factor for x-axis"),
                  #first variable the user can select
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
                  #second variable the user can select
                  selectInput("groupbar2",
                              label = NULL,
                              choices = c("release_year",
                                          "age_certification",
                                          "seasons",
                                          "main_genre",
                                          "country"),
                              selected = "age_certification"),
                  strong("Select grouping method"),
                  #add in the option for the user to select how the barchart is grouped
                  selectInput("groupoption",
                              label = NULL,
                              choices = c("fill",
                                          "stack"),
                              selected = "fill")
                  ),
           column(width = 9,
                  plotOutput(outputId = "groupbarplot"))),
  
  tags$hr(),
  
  #add in the by group statistics chart
  fluidRow(column(width = 3,
                  tags$h3("By Group Statistics"),
                  strong("Select variable to display"),
                  #numeric input for the user to choose
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
                  #categorical input for the user to choose
                  selectInput("groupbyvar",
                              label = NULL,
                              choices = c("release_year",
                                          "age_certification",
                                          "seasons",
                                          "main_genre",
                                          "country"),
                              selected = "age_certification"),
                  strong("Select statistic"),
                  #summary statistic for the user to choose
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
  
  #add in a scatter plot
  fluidRow(column(width = 3,
                  tags$h3("Scatter Plot"),
                  strong("Select Variable #1 (x-axis)"),
                  #x axis variable for the user to choose
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
                  #y axis variable for the user to choose
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

#this is where we can define the relationship between the inputs and outputs
#these outputs are used in the section above
server <- function(input, output) {
  
  #read in the cleaned data
  data <- read_csv(here("cleaned_data.csv"))
  
  #create a subset of the dataframe using either movies or TV shows, depending upon
  #what the user selects
  df_subset <- reactive({
    a <- data %>% 
      filter(type == input$movie_type)
    return(a)
  })
  
  #create a dataframe with column descriptions
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
  
  #create summary statistics for all numeric variable columns
  output$sumstats <- renderTable({
    df_subset() %>% 
      select_if(is.numeric) %>% 
      st(out = "return")
  })
  
  #save total number of observations in the subset of the dataframe
  output$totalobs <- renderPrint({
    nrow(df_subset())
  })
  
  output$missingobs <- renderTable({
    #count how many observations in each column have missing values
    na_count <- df_subset() %>% 
      summarise_all(~sum(is.na(.)))
    
    #save the rownames of the transposed dataframe
    Column <- rownames(t(na_count))
    
    #combine the column names with the missing observation count and rename the column to "Count"
    cbind(Column, t(na_count)) %>% 
      as.data.frame() %>% 
      rename(Count = "V2")
  })
  
  #create dataframe of the movies/TV shows with the highest and lowest values in a chosen column
  output$extreme <- renderTable({
    #select 5 observations with highest values of input column
    top <- df_subset() %>% 
      select(title, !!rlang::sym(input$extreme_var)) %>% 
      arrange(desc(!!rlang::sym(input$extreme_var))) %>% 
      head(5)
    #select 5 observations with lowest values of input column
    low <- df_subset() %>% 
      select(title, !!rlang::sym(input$extreme_var)) %>% 
      arrange(!!rlang::sym(input$extreme_var)) %>% 
      head(5) %>% 
      arrange(desc(!!rlang::sym(input$extreme_var)))
    
    #combine the highest and lowest values into one dataframe
    rbind(top, low)
  })
  
  
  output$outliertable <- renderTable({
    #create a vector of the column that is selected for the graph
    vector_var <- df_subset() %>% 
      select(!!rlang::sym(input$densityvar)) %>% 
      pull()
    
    #drop the missing values and find number of observations in the column
    obs_in_row <- df_subset() %>% 
      select(!!rlang::sym(input$densityvar)) %>% 
      drop_na() %>% 
      nrow()
    
    #save the number of observations with a value less than 1.5 IQRs less than the 1st Quartile,
    #which is the definition of an outlier
    lower_outliers <- sum(vector_var < 
                          summary(vector_var)[2] - (1.5*IQR(vector_var, na.rm = TRUE)), 
                          na.rm = TRUE)
    
    #save the number of observations with a value more than 1.5 IQRs more than the 3rd Quartile,
    #which is the definition of an outlier
    upper_outliers <- sum(vector_var > 
                          summary(vector_var)[5] + (1.5*IQR(vector_var, na.rm = TRUE)), 
                          na.rm = TRUE)
    
    #create a dataframe with number of low and high outliers
    outlierdata <- data.frame(first = c("Number of Outliers below Minimum Threshold",
                                        "Number of Outliers above Maximum Threshold"),
                              second = c(lower_outliers, upper_outliers))
    #remove column names
    names(outlierdata) <- NULL
    #add the total observation number to the dataframe for reference
    outlierdata <- rbind(outlierdata, c("Total number of Observations", obs_in_row))
    outlierdata
  })
  
  #create the density plot
  output$densityplot <- renderPlot({
    
    #if the user selects box to remove outliers
    if(input$outlierbox){
      
      #filter out the high and low outliers
      no_outliers <- reactive(df_subset() %>% 
        filter(!!rlang::sym(input$densityvar) > summary(!!rlang::sym(input$densityvar))[2] - 
                 (1.5*IQR(!!rlang::sym(input$densityvar), na.rm = TRUE)) 
               & !!rlang::sym(input$densityvar) < summary(!!rlang::sym(input$densityvar))[5] + 
                 (1.5*IQR(!!rlang::sym(input$densityvar), na.rm = TRUE))))
      
      #create the density plot with histogram overlayed
      no_outliers() %>%
        ggplot(aes(x = !!rlang::sym(input$densityvar))) +
        geom_histogram(aes(y = ..density..),
                      fill = "dodgerblue") + 
        geom_density(size = 1) + 
        labs(y = "Density") +
        theme_bw()
      
      } else{#if the user does not select the box to remove outliers
        #pull out the variable as a vector
        vector_var <- df_subset() %>% 
          select(!!rlang::sym(input$densityvar)) %>% 
          pull()
        #find the lower and upper outlier boundaries
        lower <- summary(vector_var)[2] - (1.5*IQR(vector_var, na.rm = TRUE))
        upper <- summary(vector_var)[5] + (1.5*IQR(vector_var, na.rm = TRUE))
        
        #create the density plot with histogram overlayed 
        df_subset() %>% 
          ggplot(aes(x = !!rlang::sym(input$densityvar))) +
          geom_histogram(aes(y = ..density..),
                         fill = "dodgerblue") + 
          geom_density(size = 1) + 
          #add vertical lines where the outlier boundaries are
          geom_vline(xintercept = lower,
                     size = 1) + 
          geom_vline(xintercept = upper,
                     size = 1) +
          labs(y = "Density",
               subtitle = "Vertical Lines Represent Bounds of Outliers") + 
          theme_bw()
        
      }
    
  })
  
  #create the timeplot graph
  output$timeplot <- renderPlot({
    #filter the data so that the movies are between the selected years
    time <- df_subset() %>% 
      filter(release_year >= input$timetrendyear[1],
             release_year <= input$timetrendyear[2]) %>% 
      #drop missing observations
      drop_na(!!rlang::sym(input$timetrendvar)) %>% 
      #group by year
      group_by(release_year) %>% 
      #find the average value in each year
      summarise(Mean = mean(!!rlang::sym(input$timetrendvar)))
    
    #plot the averages as a line
    ggplot(time, aes(x = as.integer(release_year), y = Mean)) + 
      geom_point() + 
      geom_line(color = "red") + 
      theme_bw() + 
      labs(x = "Release Year",
           y = str_c("Mean ", as.character(input$timetrendvar)))
  })
  
  #create the timeplot graph with different quantiles of the selected variable
  output$timequantplot <- renderPlot({
    #filter the data so that they are between the selected years
    timequant <- df_subset() %>% 
      filter(release_year >= input$timequantyear[1],
             release_year <= input$timequantyear[2]) %>% 
      #drop missing values
      drop_na(!!rlang::sym(input$timequantvar)) %>% 
      group_by(release_year) %>% 
      #find the 1st quartile, median, and 3rd quartile in each year
      summarize(enframe(quantile(!!rlang::sym(input$timequantvar), c(0.25, 0.5, 0.75)),
                        "quant",
                        "var"))
    
    #plot these quantiles in each year as lines
    ggplot(timequant, aes(x = as.integer(release_year), y = var, color = quant)) + 
      geom_point() + 
      geom_line() + 
      theme_bw() + 
      labs(x = "Release Year",
           y = str_c("Quantiles of ", as.character(input$timetrendvar)),
           color = "Quantile") + 
      #create the legend
      scale_color_discrete(breaks = c("75%", "50%", "25%"))
  })
  
  #create barplot of the counts of each factor in the categorical variable
  output$barplot <- renderPlot({
    df_subset() %>%
      #find the count of the values in each factor
      ggplot(aes(x = fct_infreq(as.factor(!!rlang::sym(input$barvar))))) + 
      geom_bar(fill = "dodgerblue") + 
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(x = input$barvar)
  })
  
  #create a grouped barplot, either stacked or filled to be 100%
  output$groupbarplot <- renderPlot({
    #select the selected columns and drop missing values
    groupdata <- df_subset() %>% 
      select(!!rlang::sym(input$groupbar1),
             !!rlang::sym(input$groupbar2)) %>% 
      drop_na()
    
    #plot the grouped barchart
    groupdata %>% 
      ggplot(aes(x = as.factor(!!rlang::sym(input$groupbar1)), 
                 fill = as.factor(!!rlang::sym(input$groupbar2)))) + 
      #position the bars as the user selected them (stacked or filled)
      geom_bar(position = input$groupoption) +
      theme_bw() + 
      labs(y = "") +
      #angle the x axis labels
      theme(axis.text.x = element_text(angle = 90)) + 
      labs(x = input$groupbar1, 
           fill = input$groupbar2)
  })
  
  #create a barchart graph for different summary statistics that the user can select
  output$statbarplot <- renderPlot({
    df_subset() %>% 
      #use the x and y variables that the user selects
      ggplot(aes(x = as.factor(!!rlang::sym(input$groupbyvar)),
                 y = !!rlang::sym(input$statvar))) + 
      geom_bar(stat = "summary",
               #use the mean, min, max, etc that the user selects
               fun = input$stat, 
               fill = "dodgerblue") + 
      labs(x = input$groupbyvar,
           y = str_c(input$stat, " ", input$statvar)) + 
      theme_bw()
  })
  
  #create a scatter plot
  output$scatterplot <- renderPlot({
    #select the variables
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


#create the Shiny App object
shinyApp(ui = ui, server = server)
