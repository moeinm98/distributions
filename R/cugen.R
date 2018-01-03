cugen= function(){
  result=c(rgenerator(0,1,1))
  return(result)
}
cugen.visual <- function(){
  result=c()
  for (i in 1:10000){
    result=c(result,cugen())
  }
  qplot(result,fill=..count.., geom = "bar") +
    theme_classic()+geom_histogram(binwidth = 0.1)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Uniform Numbers")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
}
