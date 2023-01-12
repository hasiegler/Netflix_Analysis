Data Cleaning and Manipulation
================
Henry Siegler

``` r
library(here)
library(tidyverse)
```

``` r
data <- read_csv(here("unclean_data.csv"))
head(data, 5)
```

    ## # A tibble: 5 × 15
    ##   id       title    type  descr…¹ relea…² age_c…³ runtime genres produ…⁴ seasons
    ##   <chr>    <chr>    <chr> <chr>     <dbl> <chr>     <dbl> <chr>  <chr>     <dbl>
    ## 1 ts300399 Five Ca… SHOW  "This …    1945 TV-MA        48 ['doc… ['US']        1
    ## 2 tm84618  Taxi Dr… MOVIE "A men…    1976 R           113 ['cri… ['US']       NA
    ## 3 tm127384 Monty P… MOVIE "King …    1975 PG           91 ['com… ['GB']       NA
    ## 4 tm70993  Life of… MOVIE "Brian…    1979 R            94 ['com… ['GB']       NA
    ## 5 tm190788 The Exo… MOVIE "12-ye…    1973 R           133 ['hor… ['US']       NA
    ## # … with 5 more variables: imdb_id <chr>, imdb_score <dbl>, imdb_votes <dbl>,
    ## #   tmdb_popularity <dbl>, tmdb_score <dbl>, and abbreviated variable names
    ## #   ¹​description, ²​release_year, ³​age_certification, ⁴​production_countries

### Cleaning the Movie Genre Column

``` r
#show a subset of genre column in the data
data %>% 
  select(genres) %>% 
  slice(181:185)
```

    ## # A tibble: 5 × 1
    ##   genres                     
    ##   <chr>                      
    ## 1 ['drama', 'european']      
    ## 2 ['comedy']                 
    ## 3 []                         
    ## 4 ['comedy', 'documentation']
    ## 5 ['documentation']

Some movies or TV shows have an empty bracket for the genre, so we will
remove those observations.

``` r
#filter out the movies with no genre
data_genres <- data %>% 
    filter(genres != "[]")
```

We would like to find the first genre listed for each of the movies, and
create a column with a single genre.

``` r
#create a list of all of the locations of the single quotes in each movie's genre column
locations <- str_locate_all(data_genres$genres, "'")

#create an empty vector to store the location of the second single quote in the genre column
loc <- c()
for (i in 1:nrow(data_genres)){
  loc[i] <- as.numeric(locations[[i]][2,1])
}

#create a main genre column with only the first genre listed
data_genres <- data_genres %>% 
  mutate(main_genre = str_sub(genres, 3, loc - 1))

data_genres %>% 
  select(main_genre) %>% 
  head()
```

    ## # A tibble: 6 × 1
    ##   main_genre   
    ##   <chr>        
    ## 1 documentation
    ## 2 crime        
    ## 3 comedy       
    ## 4 comedy       
    ## 5 horror       
    ## 6 comedy

Now we have a main genre for each of the movies that had at least 1
genre listed.

Now we will deal with the movies that have no genre listed.

``` r
#create a data frame of the movies with no genre listed
data_nogenre <- data %>% 
  filter(genres == "[]") %>% 
  #create a column and write the main genre as none
  mutate(main_genre = "None")

#combine the dataframe with no genre with the dataframe of the
data <- data_genres %>% 
  rbind(data_nogenre)
```

### Cleaning the Production Country Column

Now we will do the same thing for the column for the countries where the
movie was produced.

``` r
data %>% 
  select(production_countries) %>% 
  slice(11:18)
```

    ## # A tibble: 8 × 1
    ##   production_countries
    ##   <chr>               
    ## 1 ['US']              
    ## 2 ['US', 'GB']        
    ## 3 ['JP']              
    ## 4 ['US']              
    ## 5 ['US']              
    ## 6 ['US']              
    ## 7 ['EG']              
    ## 8 ['US']

The production country column lists multiple countries sometimes, so we
would like to take out only the first country and change all of these
country abbreviations to country names.

``` r
#create a dataframe with all movies with at least one country listed
data_countries <- data %>% 
  filter(production_countries != "[]")

#create a list of all of the locations of the single quotes in each movie's genre column
locations <- str_locate_all(data_countries$production_countries, "'")

#create an empty vector to store the location of the second single quote in the genre column
loc <- c()
for (i in 1:nrow(data_countries)){
  loc[i] <- as.numeric(locations[[i]][2,1])
}

#create a main country column with only the first production country abbreviation
data_countries <- data_countries %>% 
  mutate(main_country = str_sub(production_countries, 3, loc - 1))
```

``` r
#create dataframe of the movies with no production country listed
data_nocountry <- data %>% 
  filter(production_countries == "[]") %>% 
  #create the main country column and make all rows have a value of none
  mutate(main_country = "None")

#combine the dataframes
data <- data_countries %>% 
  rbind(data_nocountry)

#show the genre and country corrected
head(data) %>% 
  select(title, main_genre, main_country)
```

    ## # A tibble: 6 × 3
    ##   title                               main_genre    main_country
    ##   <chr>                               <chr>         <chr>       
    ## 1 Five Came Back: The Reference Films documentation US          
    ## 2 Taxi Driver                         crime         US          
    ## 3 Monty Python and the Holy Grail     comedy        GB          
    ## 4 Life of Brian                       comedy        GB          
    ## 5 The Exorcist                        horror        US          
    ## 6 Monty Python's Flying Circus        comedy        GB

However, we would like the dataframe have the country’s full name. For
example, we want it to say “United States” rather than US.

The dataframe below contains the abbreviations and the full name of
every country.

``` r
#read in country codes csv file
codes <- read_csv(here("countrycodes.csv"))

codes %>% 
  select(name, `alpha-2`) %>% 
  head()
```

    ## # A tibble: 6 × 2
    ##   name           `alpha-2`
    ##   <chr>          <chr>    
    ## 1 Afghanistan    AF       
    ## 2 Åland Islands  AX       
    ## 3 Albania        AL       
    ## 4 Algeria        DZ       
    ## 5 American Samoa AS       
    ## 6 Andorra        AD

``` r
#change the column name
codes <- codes %>% 
  select(name, `alpha-2`) %>% 
  rename("main_country" = `alpha-2`)

#join the full data with this country code dataframe by the country code
data <- left_join(data, codes, by = "main_country")

data %>% 
  select(main_country, name) %>% 
  head()
```

    ## # A tibble: 6 × 2
    ##   main_country name                                                
    ##   <chr>        <chr>                                               
    ## 1 US           United States of America                            
    ## 2 US           United States of America                            
    ## 3 GB           United Kingdom of Great Britain and Northern Ireland
    ## 4 GB           United Kingdom of Great Britain and Northern Ireland
    ## 5 US           United States of America                            
    ## 6 GB           United Kingdom of Great Britain and Northern Ireland

Some countries do not have very many movies, so we would like to combine
all the countries with less than 50 movies into a category called
“other”, so our graphs do not show every single country.

``` r
#create a vector of the countries with at least 51 movies created
countries <- count(data, name) %>% 
  filter(n > 50) %>% 
  pull(name)

#create a country column with the correct names
data <- data %>% 
  #if the country is not in the country vector, then it gets called "other"
  mutate(country = case_when(name %in% countries ~ name,
                             TRUE ~ "Other")) %>% 
  #change these countries to have shorter names
  mutate(country = case_when(country == "Korea, Republic of" ~ "Korea",
                             country == "Taiwan, Province of China" ~ "Taiwan",
                             country == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
                             country == "United States of America" ~ "USA",
                             TRUE ~ country)) %>% 
  #select only the columns we are using in the app
  select(title, type, release_year, age_certification, runtime, seasons, imdb_score, 
         imdb_votes, tmdb_popularity, tmdb_score, main_genre, country)

#example of some of the country column
data %>% 
  select(country) %>% 
  slice(11:20)
```

    ## # A tibble: 10 × 1
    ##    country
    ##    <chr>  
    ##  1 USA    
    ##  2 USA    
    ##  3 Japan  
    ##  4 USA    
    ##  5 USA    
    ##  6 USA    
    ##  7 Other  
    ##  8 USA    
    ##  9 Germany
    ## 10 USA

The data is cleaned and prepared for the shiny app, so we will save the
dataframe as a csv file.

``` r
write.csv(data, here("cleaned_data.csv"), row.names = FALSE)
```
