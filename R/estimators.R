dugen.estimator <- function(file)
{
  f <- read.table(file)
  mymax=max(f)
  mymin=min(f)
  return(c(mymin, mymax))


}

cugen.estimator <- function(file)
{
  return(c(0,1))
}

brgen.estimator <- function(file)
{
  f <- read.table(file)
  n=nrow(f)*ncol(f)
  ones=sum(f)
  p=ones/n
  return(c(p))
}

bigen.estimator <- function(file)
{
  f <- read.table(file)
  n <- max(f)
  p <- sum(f)/(n*nrow(f)*ncol(f))

  print(n)
  print(p)

  c <- c()
  for (i in 1:1000)
    c[i] <- bigen(p, n)


  p1<-qplot(as.vector(as.matrix(f)))
  p2<-qplot(as.data.frame(c))
  ggarrange(p1,p2, nrow =2)

  #return(c(n, p))
}

expgen.estimator <- function(file)
{
  f <- read.table(file)
  #return(mean(colMeans(f)))

  lambda = mean(colMeans(f))
  lambda
  c <- c()
  for (i in 1:1000)
    c[i] <- expgen(lambda)


  p1<-qplot(as.vector(as.matrix(f)))
  p2<-qplot(as.data.frame(c))
  ggarrange(p1,p2, nrow =2)
}

gegen.estimator <- function(file)
{
  library(ggplot2)

  f <- read.table(file)
  p = 1/mean(colMeans(f))
  c <- c()
  for (i in 1:1000)
    c[i] <- gegen(p)


  p1<-qplot(as.vector(as.matrix(f)))
  p2<-qplot(as.data.frame(c))
  ggarrange(p1,p2, nrow =2)
  #return(1/mean(colMeans(f)))

}

pogen.estimator <- function(file){
  library(ggplot2)

  results <- read.table(file, sep =" ")
  sum <- sum(results)
  n <- ncol(results)*nrow(results)

  t = 1
  lambda = sum/n

  c <- c()
  for (i in 1:10000)
    c[i] <- pogen(t, lambda)


  p1<-qplot(as.vector( as.matrix(results)))
  p2<-qplot(as.data.frame(c))
  ggarrange(p1,p2, nrow =2)

  #return(c(t,lambda))
}


gagen.estimator <- function(file){
  library(ggplot2)

  k = 1
  results <- read.table(file)
  sum <- sum(results)
  n <- ncol(results)*nrow(results)
  lambda = n/sum

  c <- c()
  for (i in 1:1000)
    c[i] <- gagen(k, lambda)


  p1<-qplot(as.vector(as.matrix(results)))
  p2<-qplot(as.data.frame(c))
  ggarrange(p1,p2, nrow =2)
  #return(c(1,lambda))
}

nogen.estimator <- function (file){
  results <- read.table(file)
  sum <- sum(results)
  n <- ncol(results)*nrow(results)
  mean <- sum/n
  variance <- (sum((results - mean)^2) )/n


  c <- c()
  for (i in 1:1000)
    c[i] <- nogen(mean, variance)

  p1<-qplot(as.vector(as.matrix(results)))
  p2<-qplot(as.data.frame(c))
  ggarrange(p1,p2, nrow =2)

  #return(c(mean,variance))
}


read <- function(file){
  library(ggplot2)
  results <- read.table(file,sep = " ")
  results
  qplot( as.vector( as.matrix(results)) ) + geom_histogram(binwidth = 0.1)
}
