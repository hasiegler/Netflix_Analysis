data %>% 
  filter(type == "MOVIE") %>% 
  ggplot(aes(x = imdb_score)) +
  geom_histogram(aes(y = ..density..),
                 fill = "lightblue") + 
  geom_density() + 
  theme_bw()