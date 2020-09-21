#task 1
library(datasets)
data(iris)

linearmod1 <- lm(iris$Sepal.Length ~ iris$Petal.Length, data = iris)
summary(linearmod1)

linearmod2 <- lm(iris$Sepal.Length ~ iris$Sepal.Width, data = iris)
summary(linearmod2)

linearmod3 <- lm(iris$Sepal.Length ~ iris$Petal.Width, data = iris)
summary(linearmod3)

#task 2
iris[,6] <- as.numeric(iris$Species == "setosa")
iris[,7] <- as.numeric(iris$Species == "versicolor")
iris <- iris[,-5]
dt = sort(sample(nrow(iris),nrow(iris)*.6))
train <- iris[dt,]
test <- iris[-dt,]
#without normalising
linearmod4 <- lm(train$Sepal.Length~.,train)
p1 <- predict(linearmod4,test)
p1-test$Sepal.Length
r2 <- 1-(sum(p1-test$Sepal.Length)^2)/(sum((p1-mean(test$Sepal.Length))^2))
