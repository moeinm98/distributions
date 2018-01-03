gagen = function(k,lambda){
  i = 0
  result = 0
  for (i in 1:k){
    result = result + expgen(lambda)
  }
  return (result)
}
gagen.visual <- function(k, lambda)
{
  library(ggpubr)
  library(ggplot2)
  c <- c()
  for (i in 1:1000)
    c[i] <- gagen(k, lambda)
  q1=qplot(c,fill=..count..,geom="histogram")+
    theme_classic()+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Gamma Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  q2=qplot(c,geom="density")+
    theme_classic()+
    labs(x="value",y="count",title="Gamma Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(q1,q2,nrow = 2)
}
