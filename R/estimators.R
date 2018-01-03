bigen.estimator <- function(file)
{
  f <- read.table(file)
  n <- max(f)
  p <- sum(f)/n
  return(c(n, p))
}
