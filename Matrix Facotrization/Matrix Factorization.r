

### SDS 385 Exercise 1
install.packages("Matrix")
install.packages("microbenchmark")

##Question C##

# y = xb + e

#Inversion method
#   X: Design matrix, N x p, vector of x values
#   Y: Response vector, N x 1, vector of y values
#   W: Diagnal weight matrix, N X N
inversion_method <- function(X, W, y){
  B_hat <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y
  return(B_hat)
  }

# Checking the performance of Inversion Method
ptm <- proc.time()
inversion_method(5000,1000)
proc.time()

ptm <- proc.time()
inversion_method(10000,2000)
proc.time()


#Cholesky method
#   X: Design matrix, N x p, vector of x values
#   Y: Response vector, N x 1, vector of y values
#   W: Diagnal weight matrix, N X N
Cholesky_method <- function (X, W, y){
  A = (t(X) * diag(W)) %*% X
  b = (t(X) * diag(W)) %*% y
  R <- chol(A)
  z <- solve(t(R)) %*% b
  B_hat_chol <- solve(R) %*% z
  return(B_hat_chol)
}

# Checking the performance of Cholesky Method
ptm2 <- proc.time()
Cholesky_method(5000,1000)
proc.time()

ptm2 <- proc.time()
Cholesky_method(10000,2000)
proc.time()

##Benchmarking: Simulate data from the linear model for a range of values of N and P
##By assuming weights are all 1
##Comparing performance of two methods
library(microbenchmark)

N <- c (10, 20, 40, 80,  100, 500, 1000)
P <- N/2
comp_resutls <- list()
for (i in 1:length(N)){
  n <- N[i]
  p <- P[i]
  
  print(n)
  
  X <- matrix(rnorm(n*p), nrow=n, ncol=p)
  y <- rnorm(n)
  W <- diag(1, nrow=n)
  
  comp_resutls[[i]] <- microbenchmark(inversion_method(X, W, y), Cholesky_method(X, W, y), unit='ms')
}


comp_resutls

### SDS 385 - Exercise 1, Part B.

library(Matrix)

#Part B:

#Reading data
rm(data)
data = read.csv("wdbc.csv", header = FALSE)

##making variables

y = data[,2]
X = as.matrix(cbind(1, scale(data[,3:12])))

##convert y values to 1/0
Y = rep(0, length(y)); Y[y=='M']=1

##setting up vector of sample sizes
s <- rep(1, nrow(X))

##Making Binomial Negative Logliklihood Function
log_l <- function(X, Y, beta, s){
  w <- 1 / (1 + exp(-X %% beta))
  log_l <- sum(Y*log(w) + (s-Y))
  return(log_l)
}

##Gradient
gradient <- function (X, Y, beta, s){
  w <- 1 / (1 + exp(-X %*% beta))  ###calculating probabilities w_i
  gradient <- array(NA, dim=length(beta)) 
  gradient <- -apply(X*as.numeric(Y-s*w),2,sum)
  return(gradient)
}
