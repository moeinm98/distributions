bigen.estimator <- function(file)
{
  f <- read.table(file)
  n <- max(f)
  p <- sum(f)/(n*nrow(f)*ncol(f))
  return(c(n, p))
}

expgen.estimator <- function(file)
{
  f <- read.table(file)
  return(mean(colMeans(f)))
}

gegen.estimator <- function(file)
{
  f <- read.table(file)
  return(1/mean(colMeans(f)))
}




pogen.estimator <- function(file){
  results <- read.table(file)
  sum <- sum(results)
  n <- ncol(results)*nrow(results)
  return(sum/n)
}


gagen.estimator <- function(file){
  results <- read.table(file)
  sum <- sum(results)
  n <- ncol(results)*nrow(results)
  return(c(1,n/sum))
}

normal.estimator <- function (file){
  results <- read.table(file)
  sum <- sum(results)
  n <- ncol(results)*nrow(results)
  mean <- sum/n
  variance <- (sum((results - mean)^2) )/n
  return(c(mean,variance))
}
