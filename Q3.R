
source("my.prediction.stats.R")
source("wrappers.R")
library(kknn)
library(boot)

ms.train = read.csv("ms.train.ass3.2020.csv", header = TRUE)
ms.test = read.csv("ms.test.ass3.2020.csv", header = TRUE)

#3.1)a)

cal_prediction <- function(the_k){
  
  int.test.hat = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = "optimal", k = the_k) )
  return(int.test.hat)
}


cal_error <- function(the_k){
  
  int.test.hat = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = "optimal", k = the_k) )
  my.error = mean((int.test.hat - ms.test$intensity)^2)
  return(my.error)
}

int.predictions <- seq(1,25,1)
result <- seq(1,25,1)
my_k <- seq(1,25,1)
for(val in my_k){
  int_pred = cal_prediction(val)
  int.predictions[val] = int_pred
  error = cal_error(val)
  result[val] = error 
}

#intensity predictions for different k
int.predictions
#mean squared errors for different k
result

plot(my_k, result, col = "blue", lwd=2.5, type ="o", xlab = "k", ylab = "Mean-Squared Error", main = "Mean-Squared Error against k" )

#3.1)b
#when k = 2
int.test.hat1 = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = "optimal", k = 2))
plot(ms.train$MZ, ms.train$intensity, col ="black", type = "l", xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity",main = "Relative Intensity against Mass/Charge (k = 2)" , lwd=2.5)
lines(ms.test$MZ, ms.test$intensity, col = "blue", type = "l", lwd=2.5)
lines(ms.test$MZ, int.test.hat1, col = "red",type = "l", lwd=2.5)

legend(x=8600,y=100,c("train","test","predicted"), lty=c(1,1,1), 
       pch=c("","",""), col=c("black","blue","red"), lwd=c(2.5,2.5,2.5))

#when k = 5
int.test.hat1 = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = "optimal", k = 5))
plot(ms.train$MZ, ms.train$intensity, col ="black", type = "l", xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity", main = "Relative Intensity against Mass/Charge (k = 5)" , lwd=2.5)
lines(ms.test$MZ, ms.test$intensity, col = "blue", type = "l", lwd=2.5)
lines(ms.test$MZ, int.test.hat1, col = "red",type = "l", lwd=2.5)

legend(x=8600,y=100,c("train","test","predicted"), lty=c(1,1,1), 
       pch=c("","",""), col=c("black","blue","red"), lwd=c(2.5,2.5,2))

#when k = 10
int.test.hat1 = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = "optimal", k = 10))
plot(ms.train$MZ, ms.train$intensity, col ="black", type = "l", xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity" , main = "Relative Intensity against Mass/Charge (k = 10)", lwd=2.5)
lines(ms.test$MZ, ms.test$intensity, col = "blue", type = "l", lwd=2.5)
lines(ms.test$MZ, int.test.hat1, col = "red",type = "l", lwd=2.5)

legend(x=8600,y=100,c("train","test","predicted"), lty=c(1,1,1), 
       pch=c("","",""), col=c("black","blue","red"), lwd=c(2.5,2.5,2))


#when k = 25
int.test.hat1 = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = "optimal", k = 25))
plot(ms.train$MZ, ms.train$intensity, col ="black", type = "l", xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity" , main = "Relative Intensity against Mass/Charge (k = 25)", lwd=2.5)
lines(ms.test$MZ, ms.test$intensity, col = "blue", type = "l", lwd=2.5)
lines(ms.test$MZ, int.test.hat1, col = "red",type = "l", lwd=2.5)

legend(x=8600,y=100,c("train","test","predicted"), lty=c(1,1,1), 
       pch=c("","",""), col=c("black","blue","red"), lwd=c(2.5,2.5,2))


#3.2
knn = train.kknn(intensity ~ ., data = ms.train, kmax=25, kernel="optimal")
int.test.hat = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = knn$best.parameters$kernel, k = knn$best.parameters$k) )
n = nrow(ms.test)
the.error = mean((int.test.hat - ms.test$intensity)^2)
my_var = the.error/n
my_var
knn$best.parameters$k
plot(ms.test$MZ, int.test.hat, col = "red",type = "l",xlab = "Mass/Charge (MZ)", ylab = "Relative Intensity",main = "Relative Intensity against Mass/Charge" , lwd=2.5)
max(int.test.hat)
int.test.hat[283]
max_mz = ms.test$MZ[283]
max_mz


#3.6
int.mean = boot(int.test.hat, function(x,I) { return(max(x[I])) }, 6000)
int.mean
k.6 = boot.ci(int.mean, 0.95, type="bca")
k.6
int.test.hat.k3 = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = "optimal", k = 3))
int.mean2 = boot(int.test.hat.k3, function(x,I) { return(max(x[I])) }, 6000)
k.3 = boot.ci(int.mean2, 0.95, type="bca")
k.3

int.test.hat.k25 = fitted( kknn(intensity~ ., ms.train, ms.test, kernel = "optimal", k = 25))
int.mean3 = boot(int.test.hat.k25, function(x,I) { return(max(x[I])) }, 6000)
k.25 = boot.ci(int.mean3, 0.95, type="bca")
k.25
