brgen=function(p){
  s=cugen()
  if(s>p){
    return(1)
  }
  else{
    return(0)
  }
}
brgen.visual <- function(p){
  library(ggplot2)
  result=c()
  for (i in 1:10000){
    result=c(result,brgen(p))
  }
  qplot(result, geom = "bar",fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Bernoulli Numbers")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
}
