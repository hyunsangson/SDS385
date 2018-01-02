install.packages("RCurl")
library(RCurl)

myfile <- getURL('https://raw.githubusercontent.com/jgscott/SDS385/master/data/co2.csv') ## obtain 209631 obs, 4 variables

data <- read.csv(textConnection(myfile), header=T)

### load libraries
library(Matrix)

### Kernel Smoothing


### Laplacian smoothing problem for fMRI data
myfile2 <- getURL('https://raw.githubusercontent.com/jgscott/SDS385/master/data/fmri_z.csv')
data2 <- read.csv(textConnection(myfile2), header=T)

library(Matrix)
install.packages("gplots") #for heatmap
library(gplots)

## (C)
## 1. direct slover
direct.solver = function (dim1, dim2, lamda){
  n = dim1*dim2
  D1 = bandSparse(n, k = c(0, 1),
                  diagonals = list(rep(-1, n), rep(1, n - 1)))
  D1 = D1[(seq(1, n)%%dim1) != 0, ]
  D2 = bandSparse(n - dim1, m = n, k = c(0, dim1), diagonals = list(rep(-1, n), rep(1, n - 1)))
  D = rBind(D1, D2)
  crossprod(D, D)*lamda+bandSparse(n, k=0, diagonals = list(rep(1, n)))
}

##2. Gauss-Seidel method (referencing https://gist.github.com/ameenzhao/e6ee1399ed9fe62854bb, https://www.r-bloggers.com/iterative-ols-regression-using-gauss-seidel/)
gauss.seidel = function


##3. Jacobi iterative method
jacobi.solver = function (A, b, x0 = rep(1, length(b)), maxiter=100){
  xprev = x0
  Din = 1/diag(A)
  R = A
  diag(R) = 0
  for (i in 1:maxiter){
    xupdate = Din * (b - crossprod(R, xprev)) #update new X
    if(max(abs(xupdate-xprev)) < 0){break;} #cehck onvergence
    xprev=xupdate #update x
  }
  return(xnew)
}

## rearrange data2 (MRI data)
data2 <- as.matrix(read.csv(textConnection(myfile2), header=T))
data2.mat = Matrix(data2)

## heat map of noisy MRI data
image(t(data2.mat), sub='', xlab='', ylab='', cuts=80)
