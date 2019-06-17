#This is the final script based on MovieLens datasets and calculating ratings prediction based on 
#penalty parameter lambda, 
#regularized movie and user effects, b_i and b_u. The optimal lambda value 5.25 had been calculated using
#cross validation. Please load edx and validation datasets before executing the script. 

#The script source : Data Science : Machine Learning Cource by R.Irizzary
#Modified : Lvesselova

#Load libraries

library(tidyverse)
library(dslabs)
library(dplyr)
library(purrr)
library(ggplot2)
library(caret)

#Load datasets

load("rdas/edx.rda")
load("rdas/validation.rda")

#RMSE function to evaluate the prediction

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Optimal penalty parameter

lambda <- 5.25

#Ratings average

mu <- mean(edx$rating)

#Movie effect

b_i <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating-mu)/(n()+lambda))

#User Effect

b_u <- edx %>%
  left_join(b_i,by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u=sum(rating-b_i-mu)/(n()+lambda))

#Prediction

predicted_ratings <- 
  validation %>%
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  mutate(pred=mu+b_i+b_u) %>%
  pull(pred)

#RMSE Result

rmse_result <- RMSE(predicted_ratings, validation$rating)
rmse_result
