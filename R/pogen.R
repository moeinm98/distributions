pogen = function(t,lambda){
  i = 0.0
  result = 0

  i = expgen(lambda) + i
  while(i <= t){
    result = result +1
    i = expgen(lambda) + i
  }
  return(result)
}
pogen.visual <- function(t, lambda)
{
  library(ggplot2)
  c <- c()
  for (i in 1:10000)
    c[i] <- pogen(t, lambda)
  qplot(c,fill=..count..,geom="histogram")+
    theme_classic()+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Poisson Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
}
