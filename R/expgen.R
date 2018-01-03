expgen <- function(lambda)
{
  return(-log(cugen(), base = exp(1))/lambda)
}
expgen.visual <- function(lambda)
{
  library(ggplot2)
  c <- c()
  for (i in 1:1000)
    c[i] <- expgen(lambda)
  qplot(c, geom = "histogram",fill=..count..) +
    theme_classic()+scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Exponential Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
}
