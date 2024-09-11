library(tidyverse)
library(tidymodels)
install.packages("skimr")
install.packages("DataExplorer")
install.packages("vroom")
install.packages("GGally")
install.packages("patchwork")
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
  fit(formula=log(count)~temp + humidity + season + windspeed, data=train1)
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
