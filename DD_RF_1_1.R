library(easypackages)
libraries("mlbench","randomForest","tictoc")

########## Boston Housing Example -- Regression ##########
data(BostonHousing)
summary(BostonHousing)

## Split the data into training and test sets
set.seed(42) #set random seed reproducible
indx <- sample(1:506,size=506,replace=F)
bh.train <- BostonHousing[indx[1:400],]
bh.test <- BostonHousing[indx[401:506],]

## Run RF using default settings
set.seed(1) #set random seed to make the results reproducible
tic()
fit1 <- randomForest(medv~.,data=bh.train, xtest=bh.test[,-14], ytest=bh.test$medv,keep.forest=TRUE)
# Here our response variable is medv and the "." means all the input variables are taken into account in the model
# "data =" is indicating which dataset is used to train the model
# xtest is the input variables use for testing the model
# ytest is the response variable the model predict using the xtest
# "keep.forest = TRUE" the model will contain the aggregated predictions and the details of each individual tree in the forest.
# default hyperparameters of RF in R: 
  # mtry: for classification (sqrt(p) where p is number of variables in x) and regression (p/3)
  # nodesize: for classification (1) and regression (5)
  # ntree: 500
fit1
toc()
## Predict on the training samples using all trees
pred.train <- predict(fit1,bh.train)
mean((pred.train-bh.train$medv)^2) #MSE using all trees
1-sum((pred.train-bh.train$medv)^2)/(399*var(bh.train$medv)) #R-square using all trees



## Prediction error (Test set MSE)
pred.test <- predict(fit1,bh.test)
acc=mean((pred.test-bh.test$medv)^2) #MSE using all trees
acc
acc=fit1$test$mse[length(fit1$test$mse)] # (iterative) Takes first predicted, subtract it from the actual, then square it and divide by n.
acc

## Plot MSE on OOB samples 
plot(fit1, main="MSE on OOB samples") 
# as number of trees increases, the OOB error converges
# can use the number of trees after the major point of the first elbow

## Bagging tree with mtry=p
tic()
set.seed(1) 
fit2 <- randomForest(medv~.,data=bh.train, xtest=bh.test[,-14], ytest=bh.test$medv,mtry=13)
fit2
toc()
## Show relative variable importance in RF. Node impurity represents how well the variable split the data
importance(fit1)
varImpPlot(fit1, sort=TRUE)

## Plot partial dependence plots for the two most important variables
par(mfrow=c(1,2))
partialPlot(fit1, bh.train, 'lstat') # Using PDP you can see how an input variable interacts with the response when the other variables are fixed
partialPlot(fit1, bh.train, 'rm')

## Show the improvement of averaging individual trees in RF
pred.test <- predict(fit1,bh.test,predict.all=T)
FUN <- function(x){
  mean((x-bh.test$medv)^2) # MSE calculations
}
par(mfrow=c(1,1))
hist(apply(pred.test$ind,2,FUN),xlim=c(0,100),xlab="MSE on test set",
     main="Histogram of MSE on individual trees from RF")
abline(v=8.4,col="red") # Red line is the mse using the entire RF (not individual trees)

## Compare with linear regression model
lm.fit <- lm(medv~.,data=bh.train)
lm.pred.test <- predict(lm.fit,newdata=bh.test)
mean((lm.pred.test-bh.test$medv)^2)
# we can see that LR outperformed a lot of the individual trees



########## Satellite Image Example -- Classification ##########
data(Satellite)
summary(Satellite)
## Split the data into training and test sets
N <- nrow(Satellite)
set.seed(1) #set random seed reproducible
indx     <- sample(1:N, size=N, replace=F)
si.train <- Satellite[indx[1:4000],]
si.test  <- Satellite[indx[4001:N],]

## RF using default settings 
tic()
set.seed(1) #set random seed to make the results reproducible
fit1 <- randomForest(classes~.,data=si.train, xtest=si.test[,-37], ytest=si.test$classes,ntree=200,keep.forest=TRUE)
fit1 # has trouble predicting damp grey soil accurately
toc()
## Plot OOB error rates 
plot(fit1, main="Error rates on OOB samples")
legend("topright",legend=c("OOB",levels(si.train$classes)),col=1:7,lty=1:7)

## Plot relative variable importance
varImpPlot(fit1, sort=TRUE)

##===================== Extra Statistical Analysis =============================


## Compare the partial dependence plots for x.17 and x.20 on two soil types
par(mfrow=c(2,2))
partialPlot(fit1, si.train, 'x.17',which.class='red soil',main="PDP of x.17 for red soil")
partialPlot(fit1, si.train, 'x.17',which.class='grey soil',main="PDP of x.17 for grey soil")
partialPlot(fit1, si.train, 'x.20',which.class='red soil',main="PDP of x.20 for red soil")

## Density plots on x.17 for red and grey soils
d1 <- density(si.train$x.17[which(si.train$classes=='red soil')])
d2 <- density(si.train$x.17[which(si.train$classes=='grey soil')])
par(mfrow=c(1,2))
plot(d1,xlim=range(d1$x,d2$x), ylim=range(d1$y,d2$y), col="red",
     main="Density plot for x.17 on red/grey soil",
     xlab="x.17",lwd=2)
lines(d2,col="grey",lwd=2,lty=2)
legend("topleft",legend=c("red soil","grey soil"),lty=c(1,2),col=c("red","grey"),lwd=2)

## Density plots on x.17 for red and grey soils
d3 <- density(si.train$x.20[which(si.train$classes=='red soil')])
plot(d1,xlim=range(d1$x,d3$x), ylim=range(d1$y,d3$y), col="red",
     main="Density plot for x.17 and x.20 on red soil",
     xlab="",lwd=2)
lines(d3,col="blue",lwd=2,lty=2)
legend("topleft",legend=c("x.17","x.20"),lty=c(1,2),col=c("red","blue"),lwd=2)
