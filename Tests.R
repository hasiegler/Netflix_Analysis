data <- read_csv(here("titles.csv/titles.csv"))
codes <- read_csv(here("countrycodes.csv"))


data_movie <- data %>% 
  filter(type == "MOVIE")

num <- data_movie %>% 
           select_if(is.numeric) %>% 
  select(-seasons) %>% 
  drop_na()

test <- data_movie %>% 
  select(imdb_score, tmdb_score) %>% 
  drop_na() %>% 
  scale() %>% 
  as.data.frame()

data2 <- data %>% 
  select(-seasons) %>% 
  drop_na()




clust <- kmeans(as.matrix(test), 3)$clust

test <- cbind(test, clust)

ggplot(test, aes(x = imdb_score, y = tmdb_score, color = as.factor(clust))) + 
  geom_point()

#genre
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
#genre end
#countries
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
#countries end
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


stats <- data %>% 
  group_by(age_certification) %>% 
  summarise(stat = mean(runtime))

ggplot(data, aes(x = reorder(age_certification, runtime), y = runtime)) + 
  geom_bar(stat = "summary",
           fun = "max")


#time trend

time <- data_movie %>% 
  drop_na(runtime) %>% 
  group_by(release_year) %>% 
  summarize(enframe(quantile(runtime, c(0.75, 0.5, 0.25)),
                    "quant",
                    "runtime"))

time <- data_movie %>% 
  filter(release_year >= 1990) %>% 
  drop_na(imdb_score) %>% 
  group_by(release_year, age_certification) %>% 
  summarise(mean = mean(imdb_score))

ggplot(time, aes(x = release_year, y = mean, color = age_certification)) + 
  geom_point() + 
  geom_line()

time %>% 
  ggplot(aes(x = release_year, y = runtime, color = quant)) + 
  geom_point() + 
  geom_line() + 
  labs(color = "Quantile") + 
  scale_color_discrete(breaks = c("75%", "50%", "25%"))








