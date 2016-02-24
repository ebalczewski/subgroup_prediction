# requires two packages to be installed, randomForest and FSelector

#install.packages("randomForest")
#install.packages("FSelector")
library("randomForest")
library("FSelector")

# change to appropriate directory for your laptop
setwd("C:/Users/Anne/Downloads/subgroup_prediction")

data <- read.csv("Data/Training_Data.csv")

data2 <- data[data$dataset == 2,]
#data2[,5:24] <- ifelse(data2[,5:24] > 0, 1, 0) # if we want to collapse 2's and 1's

x <- data2[,c(3,5:ncol(data2))] # covariates plus trt column
y <- data2$y # response column

# create datasets for virtual twin predictions
data2_all_trt <- data2
data2_all_ctrl <- data2
data2_all_trt$trt <- 1
data2_all_ctrl$trt <- 0

# random forest regression
blah <- randomForest(x,y)
trt.pred <- predict(blah, data2_all_trt)
ctrl.pred <- predict(blah, data2_all_ctrl)

# virtual twin treatment effect estimates
z <- trt.pred - ctrl.pred

# subgroup identified by Innocentive
subgrp <- data2$x29 < 51.3
boxplot(z[subgrp], z[!subgrp])

# Now do feature selection for only the data with bottom or top 30% of z estimates
alpha <- .3 # identifies x29 if alpha <= .3

test <- data2[,5:ncol(data2)] # will use for feature selection
test$target <- ifelse(z < quantile(z, alpha), 1, 0) # bottom or top

test <- test[z < quantile(z, alpha) | z > quantile(z, 1-alpha),] # subset

#boxplot(test$x29[test$target == 1], test$x29[test$target == 0])
#boxplot(test$x21[test$target == 1], test$x21[test$target == 0])

weights <- chi.squared(target ~ ., test)
# weights # calculates usefulness for each column of data

which(weights > 0) # will print 29
# yay, this identifies column x29 as the only one having an effect!
