library(tidyverse)
library(tidymodels)
install.packages("skimr")
install.packages("DataExplorer")
install.packages("vroom")
install.packages("GGally")
install.packages("patchwork")
install.packages("poissonreg")
install.packages("glmnet")
install.packages("rpart")
library(rpart)
library(glmnet)
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
plot1
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

#linear regression

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

#poisson regression

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

#clean and then linear regression

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


#Penalized Regression

mycleandata <- train1 %>% 
  select(-casual,-registered) %>% 
  mutate(count=log(count))

mycleandata
my_recipe <- recipe(count~.,data=mycleandata) %>% 
  step_mutate(weather=ifelse(weather==4,3,weather)) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny","Misty","Rainy"))) %>%
  step_time(datetime, features="hour") %>%
  step_date(datetime, features="dow") %>% 
  step_mutate(datetime_dow=factor(datetime_dow)) %>% 
  step_mutate(datetime_hour=factor(datetime_hour)) %>% 
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring","summer","Fall","Winter"))) %>%
  step_rm(datetime, temp, holiday, workingday) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(my_recipe)
mycleandata
bake(prepped_recipe, new_data=mycleandata)
bake(prepped_recipe, new_data=test1)
test1
mycleandata

preg_model <- linear_reg(penalty=.0001,mixture=.75) %>% 
  set_engine("glmnet")
preg_wf <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(preg_model) %>% 
  fit(mycleandata)
prediction <- predict(preg_wf,new_data=test1)
prediction <- exp(prediction)


kaggle_submission <- prediction %>% 
  bind_cols(., test1) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime))) #prepare data in required format
vroom_write(x=kaggle_submission, file="./pregmodel.csv", delim=",")


#Cross Validation
mycleandata <- train1 %>% 
  select(-casual,-registered) %>% 
  mutate(count=log(count))

mycleandata
my_recipe <- recipe(count~.,data=mycleandata) %>% 
  step_mutate(weather=ifelse(weather==4,3,weather)) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny","Misty","Rainy"))) %>%
  step_time(datetime, features="hour") %>%
  step_date(datetime, features="dow") %>% 
  step_mutate(datetime_dow=factor(datetime_dow)) %>% 
  step_mutate(datetime_hour=factor(datetime_hour)) %>% 
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring","summer","Fall","Winter"))) %>%
  step_rm(datetime, temp, holiday, workingday) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% 
  set_engine("glmnet")
preg_model
preg_wf <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(preg_model)

grid_of_tuning_params <- grid_regular(penalty(), mixture(), levels = 10)
grid_of_tuning_params
folds <- vfold_cv(mycleandata, v = 5,repeats=1)
folds
CV_results <- preg_wf %>% 
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae))

collect_metrics(CV_results) %>% 
  filter(.metric=="rmse") %>% 
  ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
  geom_line()

bestTune <- CV_results %>% 
  select_best(metric="rmse")
bestTune
final_wf <- preg_wf %>% 
  finalize_workflow(bestTune) %>% 
  fit(data=mycleandata)

predict <- final_wf %>% 
  predict(new_data=test1)
predict <- exp(predict)
kaggle_submission <- predict %>% 
  bind_cols(., test1) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime))) #prepare data in required format
vroom_write(x=kaggle_submission, file="./pregmodel22.csv", delim=",")

#regression tree
mycleandata <- train1 %>% 
  select(-casual,-registered) %>% 
  mutate(count=log(count))

my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")
mycleandata$datetime
my_recipe <- recipe(count~.,data=mycleandata) %>% 
  step_mutate(weather=ifelse(weather==4,3,weather)) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny","Misty","Rainy"))) %>%
  step_time(datetime, features="hour") %>%
  step_date(datetime, features="dow") %>% 
  step_mutate(datetime_dow=factor(datetime_dow)) %>% 
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring","summer","Fall","Winter"))) %>%
  step_rm(datetime, temp, holiday, workingday) %>% 
  step_mutate(datetime_dow=ifelse(datetime_dow=="Sun",1,datetime_dow)) %>% 
  step_interact(terms = ~ datetime_dow:datetime_hour) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

my_recipe <- prep(my_recipe)
testingit <- bake(my_recipe, new_data=mycleandata)
testingit$datetime_dow_x_datetime_hour
preg_wf <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(my_mod)

grid_of_tuning_params <- grid_regular(tree_depth(), cost_complexity(), min_n(), levels = 8)
folds <- vfold_cv(mycleandata, v = 5,repeats=1)
CV_results <- preg_wf %>% 
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse))
bestTune <- CV_results %>% 
  select_best(metric="rmse")
bestTune
final_wf <- preg_wf %>% 
  finalize_workflow(bestTune) %>% 
  fit(data=mycleandata)
predict <- final_wf %>% 
  predict(new_data=test1)
predict <- exp(predict)
predict
kaggle_submission <- predict %>% 
  bind_cols(., test1) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime))) #prepare data in required format
vroom_write(x=kaggle_submission, file="./treemodel22.csv", delim=",")
