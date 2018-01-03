gegen <- function(p)
{
  k = 0
  while (brgen(p) != 1)
    k <- k + 1

  return(k)
}
gegen.visual <- function(p)
{
  library(ggplot2)
  c <- c()
  for (i in 1:1000)
    c[i] <- gegen(p)
  qplot(c, geom = "bar",fill=..count..) +
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Geometric Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
}
