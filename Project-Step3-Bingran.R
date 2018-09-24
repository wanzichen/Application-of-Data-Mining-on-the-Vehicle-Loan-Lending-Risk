## Read dataset
library(Matrix)
setwd(dir="/Users/bingranchen/Documents/Macbook13''/UCF/MSDA-Courses/STA 6704 DataMingingII/Project/project-code")
movie = read.csv(file="movie_clean.csv", stringsAsFactors = F)


#### Separate category data and numeric data
cat_var <- names(movie)[which(sapply(movie, is.character))]
numeric_var <- names(movie)[which(sapply(movie, is.numeric))]
movie_cat <- movie[cat_var]
movie_num <- movie[numeric_var]
dim(movie_cat)
dim(movie_num)


## 4. Build Model
movie1 <-movie_num
### train set and test set (75%, 25%)
set.seed(123)
smp_size <- floor(0.75 * nrow(movie1))
index <- sample(seq_len(nrow(movie1)),size=smp_size)
training <- movie1[index, ]
testing <- movie1[-index, ]

### Linear regression
summary(training)
#### Fit model:
fit.lm = lm(imdb_score~., data=training)
summary(fit.lm)
#### Prediction of Rating
testing_x <- testing[ , -which(names(testing) %in% c("imdb_score"))]
pred.lm = predict(fit.lm, data.frame(testing_x))

#### Prediction of coefficient
summary(fit.lm)

#### testing MSE
MSE.lm <- mean((pred.lm - testing$imdb_score)^2)

#### Plot pred vs true
plot(testing$imdb_score, pred.lm)



### SVR
library(e1071)
#### Linear kernel
training=data.frame(training)
#### tune cost
set.seed(1)
tune.outlm=tune(svm, imdb_score~., data=training, kernel="linear", ranges= list(cost=2^(-5:5)))
summary(tune.outlm) #10-fold cost=4

#### Fit model:
fit.svmlm = svm(imdb_score ~ ., data=training, kernel = "linear", cost = 1)
#### Prediction of Rating
pred.svmlm = predict(fit.svmlm, testing)
#### MSE
MSE.svmlm <- mean((pred.svmlm - testing$imdb_score)^2)
MSE.svmlm

#### Plot
plot(testing$imdb_score, pred.svmlm)

#### Polynomial kernel
#### tune cost and gamma
tune.outply=tune(svm, imdb_score~., data=training, kernel="polynomial", ranges= list(cost=2^(-5:5), gamma=2^(-5:5)))
summary(tune.outply) #10-fold cost=
#### Fit model:
fit.svmply = svm(imdb_score ~ ., data=training, kernel = "polynomial", cost = 4)
#### Prediction of Rating
pred.svmply = predict(fit.svmply, testing)
#### MSE
MSE.svmply <- mean((pred.svmply - testing$imdb_score)^2)
MSE.svmply


#### Radial kernel
#### tune cost and gamma
set.seed(1)
tune.outrad=tune(svm, imdb_score~., data=training, kernel="radial", ranges= list(cost=2^(-5:5), gamma=2^(-5:5)))
summary(tune.outrad) #10-fold cost=4 gamma=0.0625
#### Fit model:
fit.svmrad = svm(imdb_score ~ ., data=training, kernel = "radial", cost = 8, gamma=0.0625)
#### Prediction of Rating
pred.svmrad = predict(fit.svmrad, testing)
#### MSE
MSE.svmrad <- mean((pred.svmrad - testing$imdb_score)^2)
MSE.svmrad
#### Plot
plot(testing$imdb_score, pred.svmrad)


### RF
training_x <- training[ , -which(names(training) %in% c("imdb_score"))]
library(randomForest)
set.seed(1)
#### tune mtry and ntree
bestmtry1 <- tuneRF(training_x, training$imdb_score, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry1)

set.seed(1)
bestmtry2 <- tuneRF(training_x, training$imdb_score, stepFactor=1.5, improve=1e-5, ntree=100)
print(bestmtry2)

set.seed(1)
bestmtry3 <- tuneRF(training_x, training$imdb_score, stepFactor=1.5, improve=1e-5, ntree=30)
print(bestmtry3)

set.seed(1)
bestmtry4 <- tuneRF(training_x, training$imdb_score, stepFactor=1.5, improve=1e-5, ntree=1000)
print(bestmtry4)

#### Fit model:
fit.rf = randomForest(imdb_score~., data=training, mtry=6, ntree=1000, importantce= T)

#### Prediction of Rating
pred.rf = predict(fit.rf, s= bestlam, newdata=testing)

#### MSE
MSE.rf <- mean((pred.rf - testing$imdb_score)^2)
MSE.rf

#### Plot pred vs. true
#### Plot
plot(testing$imdb_score, pred.rf)
#### Variable importance
importance(fit.rf)
varImpPlot(fit.rf)


### 6. Discuss and Compare Results

#### 6.1 Plots of prediction vs. true 
plot(testing$imdb_score, pred.lm)


#### 6.2 Coefficients and MSE
###### RMSE
library(hydroGOF)
RMSE.lm = rmse(pred.lm, testing$imdb_score)
#RMSE.lasso = rmse(pred.lasso,y.test)###??????????
RMSE.svmlm = rmse(pred.svmlm, testing$imdb_score)
RMSE.svmply = rmse(pred.svmply, testing$imdb_score)
RMSE.svmrad = rmse(pred.svmrad, testing$imdb_score)
RMSE.rf = rmse(pred.rf, testing$imdb_score)
