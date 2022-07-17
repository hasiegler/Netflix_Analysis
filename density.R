movies <- data %>% 
  filter(type == "MOVIE")

movies %>% 
  ggplot(aes(x = imdb_score)) +
  geom_histogram(aes(y = ..density..),
                 fill = "dodgerblue") + 
  geom_density(size = 1) + 
  labs(y = "Density") +
  theme_bw()

Tmin <- summary(movies$imdb_score)[2] - (1.5*IQR(movies$imdb_score, na.rm = TRUE))
Tmin <- as.numeric(Tmin)
Tmax <- summary(movies$imdb_score)[5] + (1.5*IQR(movies$imdb_score, na.rm = TRUE))
Tmax <- as.numeric(Tmax)

movies$imdb_score[which(movies$imdb_score > Tmin & movies$imdb_score < Tmax)] 

movies %>% 
  filter(imdb_score > Tmin & imdb_score < Tmax) %>% 
  ggplot(aes(x = imdb_score, y = ..density..)) +
  geom_histogram(fill = "dodgerblue") + 
  geom_density(size = 1) + 
  geom
  labs(y = "Density") +
  theme_bw()
