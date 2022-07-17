movies <- data %>% 
  filter(type == "MOVIE")

movies %>% 
  ggplot(aes(x = imdb_score)) +
  geom_histogram(aes(y = ..density..),
                 fill = "dodgerblue") + 
  geom_density(size = 1) + 
  labs(y = "Density") +
  theme_bw()