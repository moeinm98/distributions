bigen <- function(p, n)
{
  k = 0
  numbers <-
    for (i in 1:n)
    {
      if (brgen(p) == 1)
      {k <- k + 1}
    }
  return(k)
}
bigen.visual <- function(p, n)
{
  library(ggplot2)
  c <- c()
  for (i in 1:10000)
    c[i] <- bigen(p, n)
  qplot(c, geom = "bar",fill=..count..) +
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Binomial Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
}
