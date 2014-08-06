library(car)
library(MASS)
library(glmnet)

makelines <- function(x, y) {
    points(x, y, pch=2)
    abline(lsfit(x, y), col="red")
}
pairs(mtcars, panel=makelines)

set.seed(47)
n.train <- 24
train.idcs <- sample(1:nrow(mtcars), n.train)
cars <- list()
cars$train <- mtcars[train.idcs, ]
cars$test <- mtcars[-train.idcs, ]

fit.full <- lm(mpg~., data=cars$train)
mse <- function(predicted, truth) { #mean square error function
    n <- length(predicted)
    return((1/n) * sum((predicted - truth) ** 2))
}
predictions <- predict(fit.full, cars$test)
full.mse <- mse(predictions, cars$test$mpg)

stepAIC(fit.full) #removes some variables
fit.aic <- lm(mpg ~ drat + wt + gear + carb, data=cars$train)
predictions <- predict(fit.aic, cars$test)
aic.mse <- mse(predictions, cars$test$mpg)
