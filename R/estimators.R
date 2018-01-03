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
