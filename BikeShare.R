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
sample <- "BikeShare/sampleSubmission.csv"
test <- "BikeShare/test.csv"
train <- "BikeShare/train.csv"
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
