library(e1071)
library(ggplot2)
data <- read.csv("Test.csv", TRUE, ",")
head(data)
class(data)
plot(data$Year, data$September, ylab="Rainfall in mm ", xlab = "Year",main='Plot of Linear
and SVR model for September' ,col="black", pch=20)
# Create a linear regression model
model <- lm(September ~ Year, data)
# Add the fitted line
# make a prediction for each Year
predictedSeptember <- predict(model, data)
# display the predictions
rmse <- function(error)
error <- data$September - predictedSeptember
predictedRMSESeptember <- rmse(error)
model <- svm(September ~ Year , data)
svrpredictedSeptember <- predict(model, data)
rmse <- function(error)
error <- data$September - svrpredictedSeptember
svrPredictionRMSEseptember <- rmse(error)

# perform a grid search
tuneResult <- tune(svm, September ~ Year, data = data, ranges = list(epsilon = seq(0,1,0.1),
cost = 2^(2:9) ))
print(tuneResult)
plot(tuneResult)
summary(tuneResult)
tunedModel <- tuneResult$best.model
tunedModelSeptember <- predict(tunedModel, data)
rmse <- function(error)
error <- data$September - tunedModelSeptember
tunedModelRMSEseptember <- rmse(error)
print(tuneResult)
plot(tuneResult)

36

plot(data$Year, data$September, ylab="Rainfall in mm(september)", xlab =
"Year",main='Plot for SVR and Tuned SVR model', col="black", pch=20)
tunedModel <- tuneResult$best.model
tunedModelSeptember <- predict(tunedModel, data)
points(data$Year, svrpredictedSeptember, col = "red", pch=20)
lines(data$Year, svrpredictedSeptember, col = "red", pch=20)
points(data$Year, tunedModelSeptember, col = "blue", pch=20)
lines(data$Year, tunedModelSeptember, col = "blue", pch=20)
points(data$Year, predictedSeptember, col = "green", pch=20)
lines(data$Year, predictedSeptember, col = "green", pch=20)
Code:
library(e1071)
library(ggplot2)
data <- read.csv("Test.csv", TRUE, ",")
head(data)
class(data)
plot(data$Year, data$May, ylab="Rainfall in mm ", xlab = "Year",main='Plot of Linear and
SVR model for May' ,col="black", pch=20)
# Create a linear regression model
model <- lm(May ~ Year, data)
# Add the fitted line
# make a prediction for each Year
predictedMay <- predict(model, data)
# display the predictions
rmse <- function(error)
error <- data$May - predictedMay
predictedRMSEmay <- rmse(error)
model <- svm(May ~ Year , data)
svrpredictedMay <- predict(model, data)
rmse <- function(error)
error <- data$May - svrpredictedMay
svrPredictionRMSEmay <- rmse(error)

# perform a grid search
tuneResult <- tune(svm, May ~ Year, data = data, ranges = list(epsilon = seq(0,1,0.1), cost =
2^(2:9) ))

37

print(tuneResult)
plot(tuneResult)
summary(tuneResult)
tunedModel <- tuneResult$best.model
tunedModelMay <- predict(tunedModel, data)
rmse <- function(error)
error <- data$May - tunedModelMay
tunedModelRMSEmay <- rmse(error)
print(tuneResult)
plot(tuneResult)
plot(data$Year, data$May, ylab="Rainfall in mm(may)", xlab = "Year",main='Plot for SVR
and Tuned SVR model', col="black", pch=20)
tunedModel <- tuneResult$best.model
tunedModelMay <- predict(tunedModel, data)
points(data$Year, svrpredictedMay, col = "red", pch=20)
lines(data$Year, svrpredictedMay, col = "red", pch=20)
points(data$Year, tunedModelMay, col = "blue", pch=20)
lines(data$Year, tunedModelMay, col = "blue", pch=20)
points(data$Year, predictedMay, col = "green", pch=20)
lines(data$Year, predictedMay, col = "green", pch=20)
library(e1071)
library(ggplot2)
data <- read.csv("Test.csv", TRUE, ",")
head(data)
class(data)
plot(data$Year, data$October, ylab="Rainfall in mm ", xlab = "Year",main='Plot of Linear
and SVR model for October' ,col="black", pch=20)
# Create a linear regression model
model <- lm(October ~ Year, data)
# Add the fitted line
# make a prediction for each Year
predictedOctober <- predict(model, data)
# display the predictions
points(data$Year, predictedOctober, col = "green", pch=20)
lines(data$Year, predictedOctober, col = "green", pch=20)
rmse <- function(error)
error <- data$October - predictedOctober
predictedRMSEoctober <- rmse(error)
model <- svm(October ~ Year , data)

38

svrpredictedOctober <- predict(model, data)
points(data$Year, svrpredictedOctober, col = "red", pch=20, na.action = na.exclude)
points(data$Year, svrpredictedOctober, col = "red", pch=20)
lines(data$Year, svrpredictedOctober, col = "red", pch=20)
rmse <- function(error)
error <- data$October - svrpredictedOctober
svrPredictionRMSEoctober <- rmse(error)
# perform a grid search
tuneResult <- tune(svm, October ~ Year, data = data, ranges = list(epsilon = seq(0,1,0.1),
cost = 2^(2:9) ))
print(tuneResult)
plot(tuneResult)
summary(tuneResult)
tunedModel <- tuneResult$best.model
tunedModelOctober <- predict(tunedModel, data)
rmse <- function(error)
error <- data$October - tunedModelOctober
tunedModelRMSEoctober <- rmse(error)
print(tuneResult)
plot(tuneResult)
plot(data$Year, data$October, ylab="Rainfall in mm(october)", xlab = "Year",main='Plot for
SVR and Tuned SVR model', col="black", pch=20)
tunedModel <- tuneResult$best.model
tunedModelOctober <- predict(tunedModel, data)
points(data$Year, svrpredictedOctober, col = "red", pch=20)
lines(data$Year, svrpredictedOctober, col = "red", pch=20)
points(data$Year, tunedModelOctober, col = "blue", pch=20)
lines(data$Year, tunedModelOctober, col = "blue", pch=20)
points(data$Year, predictedOctober, col = "green", pch=20)
lines(data$Year, predictedOctober, col = "green", pch=20)

39

Matlab code:
rng('default')
X = rand(100,1);
X = tan(pi*X - pi/2);
modelfun = @(b,x) b(1) * ...
(pi/2 + atan((x - b(2))/b(3)));
y = modelfun([12 5 10],X) + randn(100,1);
beta0 = [1 1 1]; % An arbitrary guess
mdl = fitnlm(X,y,modelfun,beta0)
plotSlice(mdl)
Xnew = [-15;5;12];
[ynew,ynewci] = predict(mdl,Xnew)
ds = dataset({X,'X'},{y,'y'});
mdl2 = fitnlm(ds,modelfun,beta0);
Xnew = [-15;5;12];
ynew = feval(mdl2,Xnew)
Xnew = [-15;5;12];
ysim = random(mdl,Xnew)