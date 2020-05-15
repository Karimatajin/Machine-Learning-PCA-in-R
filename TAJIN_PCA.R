# Author : Karima Tajin
# Date : 6 April 2020
# Machine Learning in R 
# Project 4 : PCA (Principal Component Analysis)


###########################################
###### Principal Component Analysis #######
###########################################

# Principal Component Analysis is one of the popular algorithms in Machine Learning for exploratory analysis, it is helpful in the case of "wide" datasets
# it can be used as a dimensionality reduction method which can help to minimize the number of the variables without losing much of the original information
# there are two options to reduce dimensionality either by using feature elimination or feature extraction

###Assignment4 -PCA
# Perform a Principal Component Analysis on soccer data:
# 1- Explore the data with scatter plots or histograms or other graphs
# 2- Choose the appropriate features for PCA
# 3- Perform a Principal Component Analysis 
# 4- Create a Scree Plot in R
# 5- Choose an appropiate number of principal components
# 6- Select an appropiate subset of the features from the original variables
# 7- Discuss how well(or poorly) the new components and features represent the data


# import tidyverse library
library(tidyverse)
# import factoextra library which I will use it for fviz_screeplot and fviz_contrib
library(factoextra)
# get the working directory:
setwd("/Users/karimaidrissi/Desktop/DSSA 5201 ML/PCA")
# load the dataset:
soccer19<- read.csv("data.csv")

# dimmension of the dataset:
dim(soccer19) # there'r 18207 observations and 89 variables 

# checking missing values in the dataset:
sum(is.na(soccer19)) # 1836 missing values

# remove missing values:
soccer19<- na.omit(soccer19)

# structure of the dataset:
str(soccer19)
# take a look at the structure of our dataset can help us to figure out which features to choose for PCA analysis
# the features that we might be thinking to choose are Overall, Potential, Age, Heights, position and the wage of the players

###Exploration###

# Histogram of Player Rattings:
soccer19 %>% 
  ggplot(aes(x=Overall, fill = factor(Overall))) + geom_bar(color = "grey20")+ guides(fill = FALSE) +
  ggtitle("Player Ratings") 
# Players Ratings are normally distributed, we can see that the overall points are mainly higher bw 60 and 80
# and the minimum rating is 45, the maximum is 95

# Combining Position into Position_group

fwd <- c('RF', 'ST', 'LF', 'RS', 'RW', 'LS', 'LW', 'CF')
mid <- c('RCM', 'LCM', 'LDM', 'CAM', 'CDM', 'RM', 'LAM', 'LM', 'RDM', 'CM', 'RAM')
def <- c('RCB', 'CB', 'LCB', 'LB', 'RB', 'RWB', 'LWB')
gk <- 'GK'

soccer19[soccer19$Position %in% fwd, 'position_group'] <- 'FWD'
soccer19[soccer19$Position %in% mid, 'position_group'] <- 'MID'
soccer19[soccer19$Position %in% def, 'position_group'] <- 'DEF'
soccer19[soccer19$Position %in% gk, 'position_group'] <- 'GK'

soccer19 <- soccer19 %>% 
  mutate(position_group = factor(position_group, levels = c('FWD', 'MID', 'DEF', 'GK')))

# Ages of football players vs their Agility filled by their position:
soccer19 %>% 
  ggplot(aes(x=Age, y=Agility, fill=position_group, color = position_group)) + geom_point() +
  ggtitle("Age Vs Agility colored by the Position")

# Soccer requires players to have higher agility, the graph shows that the peak age are bw 20 and 30
# where players with the age of more than 30 have less agility to react quickly 

# Ages of football players vs their wages filled by their position:
# convert wage column to integers
soccer19$Wage <- as.integer(soccer19$Wage)
soccer19$Value <- as.integer(soccer19$Value)

higher_income <- soccer19[soccer19$Wage >'200',]
higher_income %>%
  ggplot(aes(x=Age, y=Wage, fill = position_group, color= position_group )) + geom_point() + ggtitle("Age Vs Wage colored by the Position")
# As we can conclude from the plot that Forwards , Midfielders and Defense have higher wage more than GoalKeeper
# also the oldest players are either Goalkeeper or Defense. 

# Barplot of preferred leg for players
barplot(summary(soccer19$Preferred.Foot), col = rainbow(2), main = "Preferred Leg for Players",
        xlab = "Leg", ylab = "Frequency", ylim = c(0,15000))

# The left-footed players is less used compared to the right-footed players, means more than 10000 in frequency prefered the right leg more than the left

# Some Question that I would like to answer with this dataset is,
# Which features are highly correlated with a player's overall rating by player position?
# find out which features bring the most success to the team?


# PCA works best with numerical data, we will exclude all categorical variables
# all numerics columns in our data that can be used in PCA 2,4,8,9,12:14, 16:18,23, 29:88 
# using prcomp() function to assign the exclude variables, also tell R to center and scale our data by setting them to be TRUE
# some of my targets variables for this dataset are: Age,Overall,Potential,Wage position group,... 
# So the columns that I would like to use are soccer2019 <-soccer19[, c(4,8,9,13,66:89)]

soccer19_PCA <- prcomp(soccer19[,c(4,8,9,12,13,55:88)], center= TRUE, scale.=TRUE)
# summary of soccer19_PCA dataset:
summary(soccer19_PCA) # there'r 39 principal components 
#  there'r 3 variables:
# Standard deviation which simply means eigenvalues.For example for the first principal component the eigenvalues is equal to 4.37
# Proportion of Variance means the amount of variance the component account for in the data, PC1 accounts for more than 49% of total variance in the soccer19_PCA dataset
# Cumulative Proportion is simply the accumulated amount of the variance.For example, if we used the first 2 components 
# we would be count for more than 61% of total variance, which means that nearly two-thirds of the information in the dataset can be encapsulated by just that two Principal Components

# structure of soccer19_PCA dataset:
str(soccer19_PCA)
#PCA contains the following information
#($center) is the center point
#($scale) is scalling 
#(sdev) is the standard deviation of each principal component
#($rotation) is the relationship bw the intial variables and principal components
#($x) is the values of each sample in terms of the principal components

# Creating Scree plot in R 
# Screeplot is plotting the variance against the number of the principal component
# Screeplot can help us to assess how many principal components we want to keep
fviz_screeplot(soccer19_PCA, ncp =10)
# From looking at the scree plot, choosing 4 principale components might be a good choice

# looking at the column of data that contributes the most to the first principal component
contrib <- soccer19_PCA$rotation[,1] / sum(soccer19_PCA$rotation[,1])
head(contrib[order(-contrib)])
# BallControl contribute more followed by Dribbling
# Using fviz_contrib() to see the contributions to each principal component
# with axes equal the first principal component, showing only the top 8 components 
fviz_contrib(soccer19_PCA, choice="var", sort.val = c("desc"), axes=1, top=8)
# BallControl contribute more than Dribbling and ShortPassing while ShotPower, Curve and Long

# looking at the column of data that contributes the most to the second principal componentShots contribute less
contrib <- soccer19_PCA$rotation[,2] / sum(soccer19_PCA$rotation[,2])
head(contrib[order(-contrib)])
# SlidingTackle contribute more here
fviz_contrib(soccer19_PCA, choice="var", sort.val = c("desc"), axes=2, top=8)
# Here, SlidingTackle, Interceptions and StandingTackle seems to contribute more 

# looking at the column of data that contributes the most to the third principal component
contrib <- soccer19_PCA$rotation[,3] / sum(soccer19_PCA$rotation[,3])
head(contrib[order(-contrib)])
# Overall contribute more followed by Reactions
fviz_contrib(soccer19_PCA, choice="var", sort.val = c("desc"), axes=3, top=8)
# with the third dimension, I Can see clearly that Overall contribute more 

# looking at the column of data that contributes the most to the fourth principal component
contrib <- soccer19_PCA$rotation[,4] / sum(soccer19_PCA$rotation[,4])
head(contrib[order(-contrib)])
# Balance contribute more 
# Overall contribute more followed by Reactions
fviz_contrib(soccer19_PCA, choice="var", sort.val = c("desc"), axes=4, top=8)
# The feature that contribute the most in this Soccer_PCA are BallControl, Sliding Tackle and Overall

# making sure that our features aren't correlated
cor(soccer19$BallControl, soccer19$SlidingTackle) # 38% correlation bw sliding Tackle and BallControl
cor(soccer19$Overall, soccer19$BallControl) # 46% correlation bw Overall and BallControl

# plotting The Overall vs BallControl colored by positon_group
soccer19 %>%
  ggplot(aes(x=BallControl, y=Overall, fill= position_group, color = position_group))+ geom_point()+
  labs(title = "Ball Control vs Overall Players Colored by Position ")
# from plot, we can see clearly that midfields and forwards have control over the ball more.

# plotting The Overall vs BallControl colored by Wage
soccer19 %>%
  ggplot(aes(x=BallControl, y=Overall, fill= Wage, color = Wage))+ geom_point(alpha=0.5)+
  labs(title = "Ball Control vs Overall Players Colored by Wage ")

# the plot reveals that the players get paid more when they have high Ballcontrol and Overall rate. 

###########################################
#### Evaluating PCA using RnadomForest ####
###########################################

#Before we build our model, let’s separate our data into testing and training sets.
# I tried to use original dataset soccer19 but it gives me an error that can't handle 
# categorical predictors with more than 53 categories.
set.seed(123) # set random seed to make results reproducible
soccer19_PCA <- soccer19[,c(4,8,9,12,13,55:88,90)] # load the dataset 
samp <- sample(nrow(soccer19_PCA), 0.7 * nrow(soccer19_PCA)) # use 70% to split our dataset into training and testing 
train <- soccer19_PCA[samp, ] 
test <- soccer19_PCA[-samp, ]

#I placed 60% of the observations in the original dataset into train and the remaining 40% of the observations into test.

# start building our model, by downloading randomForest library
library("randomForest") # import the package 
model_train <- randomForest(position_group ~ ., data = train) # perform the training 
model_train
# there'r 500 trees built in our model,6 is the number of variables tried at each split
# it also shows a matrix containing prediction vs actual
# as well as calssification error for each class
# also OOB estimate of error rate which equal to 11.95%

# the overall accuracy of soccer19_PCA dataset is dividing the sum of all True positives  by the total number of observations
sum(1836+4071+3833+1444)/nrow(train)
# the accuracy of our model using training dataset is 88.04%

# I will test the model again using test dataset 
model_test <- randomForest(position_group ~ ., data = test)
model_test
# OOB estimate of error rate which equal to 11.79%
# the overall accuracy of soccer19_PCA dataset is dividing the sum of all True positives  by the total number of observations 
sum(794+1814+1614+581)/nrow(test)
# the accuracy of our model using training dataset is 88.20%

# By using randomForset model I was able to predict the accuracy of player's position wich is approximately around 88% in soccer19_PCA dataset
# To sum up, PCA is very useful in cutting down the features so by adding more features to our dataset it could help us to increase the accuracy of our model
