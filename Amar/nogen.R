nogen = function(u,s){
  dev = sqrt(s)
  result = pogen(10,10)
  result = result * dev /10
  result = result + (u-10*dev)
  return (result)
}
nogen.visual <- function(u, s)
{
  library(ggplot2)
  c <- c()
  for (i in 1:10000)
    c[i] <- nogen(u, s)
  q1=qplot(c,fill=..count..,geom="histogram")+
    theme_classic()+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Normal Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  q2=qplot(c,geom="density")+
    theme_classic()+
    labs(x="value",y="count",title="Normal Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(q1,q2,nrow = 2)
}
