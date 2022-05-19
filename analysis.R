library(tidymodels)
library(readr)
library(rsample)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(skimr)
library(ggplot2)
weatherAUS <- read_csv("weatherAUS.csv")
weatherAUS <- na.omit(weatherAUS)
#去除NA

weatherAUS %>%
  select(Sunshine, RainTomorrow) %>%
  ggplot(aes(Sunshine, fill = RainTomorrow)) +
  geom_density( alpha = 0.3)

weatherAUS %>% 
  select(MinTemp, RainTomorrow) %>% 
  drop_na() %>% 
  ggplot(aes(MinTemp, fill = RainTomorrow)) + 
  geom_density( alpha = 0.3)



weatherAUS = weatherAUS %>% 
  mutate_at(vars(Location, WindGustDir, WindDir9am, WindDir3pm, RainToday, RainTomorrow), as.factor)
View(weatherAUS)
data_split <- initial_split(weatherAUS, prop=(3/4))
view(data_split)
train_data <- training(data_split)
test_data  <- testing(data_split)
#分开train和test
weather_rec <- 
  recipe(RainTomorrow ~ ., data = train_data)
summary(weather_rec)

lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")
#选择逻辑递归
lr_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(weather_rec)
lr_wflow

doParallel::registerDoParallel()

lr_fit <- 
  lr_wflow %>% 
  fit(data=train_data)


lr_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

result <- predict(lr_fit, test_data)

ftable <- select(test_data, -RainTomorrow)

ftable <- cbind(ftable, result)

view(ftable)

write.csv(ftable, "ftable.csv")
