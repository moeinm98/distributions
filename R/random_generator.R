rgenerator <- function(min=0 , max=1 , number=1){
  a=25214903917;
  m=2^48;
  c=11;
  x_0=as.numeric(Sys.time())*1000;
  for(i in 1:500){
    x_0=((a*x_0)+c)%% m
  }
  s<-c()
  for(i in 1:number){
    x_0=((a*x_0)+c)%% m
    s<-c(s,min+(max-min)*x_0/m)
  }
  return(s)
}

dugen = function(min , max){
  return(rgenerator(min , max , 1))
}
dugen.visual <- function(min , max){
  library(ggplot2)
  result=c()
  for (i in 1:10000){
    result=c(result,dugen(min, max))
  }
  qplot(result,fill=..count.., geom = "bar")+scale_fill_gradient(low="blue", high="red") + theme_classic()+geom_histogram(binwidth = 0.1)
}

cugen= function(){
  result=c(rgenerator(0,1,1))
  return(result)
}
cugen.visual <- function(){
  result=c()
  for (i in 1:10000){
    result=c(result,cugen())
  }
  qplot(result,fill=..count.., geom = "bar") + theme_classic()+geom_histogram(binwidth = 0.01)+scale_fill_gradient(low="blue", high="red")
}

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
  qplot(result, geom = "bar",fill=..count..)+ theme_classic()+geom_histogram(binwidth = 0.01)+scale_fill_gradient(low="blue", high="red")
}

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
  qplot(c, geom = "bar",fill=..count..) + theme_classic()+geom_histogram(binwidth = 0.01)+scale_fill_gradient(low="blue", high="red")
}

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
  qplot(c, geom = "histogram",fill=..count..) + theme_classic()+scale_fill_gradient(low="blue", high="red")
}

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
  qplot(c, geom = "bar",fill=..count..) + theme_classic()+geom_histogram(binwidth = 0.01)+scale_fill_gradient(low="blue", high="red")
}

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
  q1=qplot(c,fill=..count..,geom="histogram")+ theme_classic()+scale_fill_gradient(low="blue", high="red")
  q2=qplot(c,geom="density")+ theme_classic()
  ggarrange(q1,q2,nrow = 2)
}

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
  qplot(c,fill=..count..,geom="histogram")+ theme_classic()+scale_fill_gradient(low="blue", high="red")
}


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
  q1=qplot(c,fill=..count..,geom="histogram")+ theme_classic()+scale_fill_gradient(low="blue", high="red")
  q2=qplot(c,geom="density")+ theme_classic()
  ggarrange(q1,q2,nrow = 2)
}

