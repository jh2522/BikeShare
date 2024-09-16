library(tidyverse)
library(tidymodels)
install.packages("skimr")
install.packages("DataExplorer")
install.packages("vroom")
install.packages("GGally")
install.packages("patchwork")
install.packages("poissonreg")
library(poissonreg)
library(patchwork)
library(GGally)
library(skimr)
library(DataExplorer)
library(vroom)
sample <- "sampleSubmission.csv"
test <- "test.csv"
train <- "train.csv"
sample1 <- vroom(sample)
test1 <- vroom(test)
train1 <- vroom(train)
glimpse(test1)
skim(test1)
plot_intro(train1)
plot1 <- plot_correlation(train1)
plot_bar(train1)
plot_histogram(train1)
ggpairs(train1)
plot2 <- ggplot(data=train1, mapping=aes(x=weather, color=)) +
  geom_bar()
plot3 <- ggplot(data=train1, mapping=aes(x=windspeed, y=count)) +
  geom_point() 
plot4 <- ggplot(data=train1, mapping=aes(x=season, y=count)) +
  geom_col() 
fourth
(plot2 + plot3) / (plot4 + plot1)
first + third



my_linear_model <- linear_reg() %>% #create linear model
  set_engine("lm") %>% 
  set_mode("regression") %>% 
  fit(formula=count~temp + humidity + season + windspeed, data=train1)
bike_predictions <- predict(my_linear_model,
                            new_data=test1) #predict based on the test data set
bike_predictions
kaggle_submission <- bike_predictions %>% 
  bind_cols(., test1) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime))) #prepare data in required format
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",") #upload a csv file


train1$season <- as.factor(train1$season)
test1$season <- as.factor(test1$season)
my_poisson_model <- poisson_reg() %>% 
  set_engine("glm") %>% 
  set_mode("regression") %>% 
  fit(formula=count~temp + season + windspeed + humidity, data = train1)

bike_predictions <- predict(my_poisson_model, new_data=test1)
bike_predictions

kaggle_submission <- bike_predictions %>% 
  bind_cols(., test1) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(datetime=as.character(format(datetime)))
vroom_write(x=kaggle_submission, file="./Poisson_reg.csv", delim=",") #upload a csv file

mycleandata <- train1 %>% 
  select(-casual,-registered) %>% 
  mutate(count=log(count))

mycleandata
test1
my_recipe <- recipe(count~.,data=mycleandata) %>% 
  step_mutate(weather=ifelse(weather==4,3,weather)) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny","Misty","Rainy"))) %>%
  step_time(datetime, features="hour") %>%
  step_mutate(datetime_hour=factor(datetime_hour)) %>% 
  step_mutate(season=factor(season, levels=1:4, labels=c("Winter","Spring","summer","Fall"))) %>%
  step_rm(datetime, atemp)
prepped_recipe <- prep(my_recipe)
mycleandata
bake(prepped_recipe, new_data=mycleandata)
bake(prepped_recipe, new_data=test1)
test1
mycleandata

lin_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

bike_workflow <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(lin_model) %>% 
  fit(data=mycleandata)
lin_preds <- predict(bike_workflow, new_data=test1)
lin_preds <- exp(lin_preds)
mycleandata
kaggle_submission <- lin_preds %>% 
  bind_cols(., test1) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime))) #prepare data in required format
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")
