dugen = function(min , max){
  return(rgenerator(min , max , 1))
}
dugen.visual <- function(min , max){
  library(ggplot2)
  result=c()
  for (i in 1:10000){
    result=c(result,dugen(min, max))
  }
  qplot(result,fill=..count.., geom = "bar") +
    theme_classic()+geom_histogram(binwidth = 0.1)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Uniform Numbers")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
}
