if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")

library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#edx %>% summarize(Users = n_distinct(userId),
#                  Movies = n_distinct(movieId)) %>% 
#                  kable() %>%
#                  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                              position = "center",font_size = 10,full_width = FALSE)

edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

edx$yearOfRate <- format(edx$date,"%Y")

edx$monthOfRate <- format(edx$date,"%m")

validation$yearOfRate <- format(validation$date,"%Y")

validation$monthOfRate <- format(validation$date,"%m")

edx <- edx %>%
      mutate(title = str_trim(title)) %>%extract(title,c("titleTemp", "release"),
      regex = "^(.*) \\(([0-9 \\-]*)\\)$",remove = F) %>%
      mutate(release = if_else(str_length(release) > 4,
      as.integer(str_split(release, "-",simplify = T)[1]),
      as.integer(release))) %>%
        mutate(title = if_else(is.na(titleTemp),
          title,titleTemp)) %>%select(-titleTemp)

validation <- validation %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          c("titleTemp", "release"),
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(release = if_else(str_length(release) > 4,
                           as.integer(str_split(release, "-",
                                                simplify = T)[1]),
                           as.integer(release))
  ) %>%
  mutate(title = if_else(is.na(titleTemp),
                         title,
                         titleTemp)
  ) %>%
  select(-titleTemp)

edx <- edx %>%
  mutate(genre = fct_explicit_na(genres,
                                 na_level = "(no genres listed)")
  ) %>%
  separate_rows(genre,
                sep = "\\|")

validation <- validation %>%
  mutate(genre = fct_explicit_na(genres,
                                 na_level = "(no genres listed)")
  ) %>%
  separate_rows(genre,
                sep = "\\|")

edx <- edx %>% select(userId, movieId, rating, title, genre, release, yearOfRate, monthOfRate)
validation <- validation %>% select(userId, movieId, rating, title, genre, release, yearOfRate, monthOfRate)

edx$yearOfRate <- as.numeric(edx$yearOfRate)
edx$monthOfRate <- as.numeric(edx$monthOfRate)
edx$release <- as.numeric(edx$release)
validation$yearOfRate <- as.numeric(validation$yearOfRate)
validation$monthOfRate <- as.numeric(validation$monthOfRate)
validation$release <- as.numeric(validation$release)

#head(edx) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                position = "center",
#                font_size = 10,
#                full_width = FALSE)

#edx %>%
#  group_by(title) %>%
#  summarise(count = n()) %>%
#  arrange(desc(count)) %>%
#  head(n=25) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                position = "center",
#                font_size = 10,
#                full_width = FALSE)

#edx %>%
#  group_by(title) %>%
#  summarise(mean = mean(rating)) %>%
#  arrange(desc(mean)) %>%
#  head(n=25) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                position = "center",
#                font_size = 10,
#                full_width = FALSE)

#edx %>%
#  group_by(title) %>%
#  summarise(median = median(rating)) %>%
#  arrange(desc(median)) %>%
#  head(n=25) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                position = "center",
#                font_size = 10,
#                full_width = FALSE)


#edx %>%
#  group_by(genre) %>%
#  summarise(count = n()) %>%
#  arrange(desc(count)) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                position = "center",
#                font_size = 10,
#                full_width = FALSE)

#edx %>%
#  group_by(genre) %>%
#  summarise(mean = mean(rating)) %>%
#  arrange(desc(mean)) %>%
#  head(n=35) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                position = "center",
#                font_size = 10,
#                full_width = FALSE)

#edx %>%
#  group_by(genre) %>%
#  summarise(median = median(rating)) %>%
#  arrange(desc(median)) %>%
#  head(n=35) %>%
#  kable() %>%
#  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#                position = "center",
#                font_size = 10,
#                full_width = FALSE)

paste("The mean is:", as.character(mean(edx$rating)))

mu_hat <- mean(edx$rating)
rmse_mean_model_result <- RMSE(validation$rating, mu_hat)
results <- data.frame(model="Naive Mean-Baseline Model", RMSE=rmse_mean_model_result)
#------
mu_hat <- mean(edx$rating)
movie_avgs <- edx %>% group_by(movieId) %>%
              summarize(b_i = mean(rating - mu_hat))
rmse_movie_model <- validation %>%left_join(movie_avgs, by='movieId') %>%
                    mutate(pred = mu_hat + b_i) %>% pull(pred)
rmse_movie_model_result <- RMSE(validation$rating, rmse_movie_model)
results <- results %>% add_row(model="Movie-Based Model", RMSE=rmse_movie_model_result)
#------

mu_hat <- mean(edx$rating)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

rmse_movie_user_model <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
rmse_movie_user_model_result <- RMSE(validation$rating, rmse_movie_user_model)

results <- results %>% add_row(model="Movie+User Based Model", RMSE=rmse_movie_user_model_result)
#-------

mu_hat <- mean(edx$rating)

movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))
genre_pop <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genre) %>%
  summarize(b_u_g = mean(rating - mu_hat - b_i - b_u))

rmse_movie_user_genre_model <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_pop, by='genre') %>%
  mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
  pull(pred)
rmse_movie_user_genre_model_result <- RMSE(validation$rating, rmse_movie_user_genre_model)

results <- results %>% add_row(model="Movie+User+Genre Based Model", RMSE=rmse_movie_user_genre_model_result)
#-------=-

mu_hat <- mean(edx$rating)

lambdas <- seq(0, 10, 0.1)

rmses <- sapply(lambdas, function(lambda) {
  b_i <- edx %>%group_by(movieId) %>%
                summarize(b_i = sum(rating - mu_hat) / (n() + lambda))

  predicted_ratings <- validation %>%left_join(b_i, by='movieId') %>%
    mutate(pred = mu_hat + b_i) %>%pull(pred)
  
  return(RMSE(validation$rating, predicted_ratings))
})

df <- data.frame(RMSE = rmses, lambdas = lambdas)

min_lambda <- lambdas[which.min(rmses)]

rmse_regularized_movie_model <- min(rmses)

results <- results %>% add_row(model="Regularized Movie-Based Model", RMSE=rmse_regularized_movie_model)

mu_hat <- mean(edx$rating)

lambdas <- seq(0, 15, 0.1)

rmses <- sapply(lambdas, function(lambda) {
    b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu_hat + b_i + b_u) %>%
    pull(pred)
  

  return(RMSE(validation$rating, predicted_ratings))
})

df <- data.frame(RMSE = rmses, lambdas = lambdas)

min_lambda <- lambdas[which.min(rmses)]

rmse_regularized_movie_user_model <- min(rmses)

results <- results %>% add_row(model="Regularized Movie+User Based Model", RMSE=rmse_regularized_movie_user_model)


mu_hat <- mean(edx$rating)

lambdas <- seq(0, 15, 0.1)

rmses <- sapply(lambdas, function(lambda) {
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
  b_u_g <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genre) %>%
    summarize(b_u_g = sum(rating - b_i - mu_hat - b_u) / (n() + lambda))
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_u_g, by='genre') %>%
    mutate(pred = mu_hat + b_i + b_u + b_u_g) %>%
    pull(pred)
  return(RMSE(validation$rating, predicted_ratings))
})

df <- data.frame(RMSE = rmses, lambdas = lambdas)

min_lambda <- lambdas[which.min(rmses)]

rmse_regularized_movie_user_genre_model <- min(rmses)

results <- results %>% add_row(model="Regularized Movie+User+Genre Based Model", RMSE=rmse_regularized_movie_user_genre_model)

results %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)