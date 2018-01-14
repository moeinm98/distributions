dugen.estimator <- function(file)
{
  f <- read.table(file)
  mymax=max(f)
  mymin=min(f)

  c=c()
  for (i in 1:10000){
    c=c(c,dugen(mymin, mymax))
  }
  p1<-qplot(as.vector(as.matrix(f)),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.1)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Uniform Numbers",caption=paste("min = ", mymin , " max = ", mymax))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  p2<-qplot(as.data.frame(c),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.1)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Uniform Numbers")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(p1,p2, nrow =2)
  #return(c(mymin, mymax))
}

cugen.estimator <- function(file)
{
  f <- read.table(file)
  c=c()
  for (i in 1:10000){
    c=c(c,cugen())
  }
  p1<-qplot(as.vector(as.matrix(f)),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.1)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Uniform Numbers",caption=paste("min = ", 0 , " max = ", 1))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  p2<-qplot(as.data.frame(c),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.1)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Uniform Numbers")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(p1,p2, nrow =2)
  #return(c(0,1))
}

brgen.estimator <- function(file)
{
  f <- read.table(file)
  n=nrow(f)*ncol(f)
  ones=sum(f)
  p=ones/n
  c=c()
  for (i in 1:10000){
    c=c(c,brgen(p))
  }
  p1<-qplot(as.vector(as.matrix(f)),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Bernoulli Numbers",caption=paste("Probability = ", p))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  p2<-qplot(as.data.frame(c),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Bernoulli Numbers")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(p1,p2, nrow =2)
  #return(c(p))
}

bigen.estimator <- function(file)
{
  f <- read.table(file)
  f <- as.integer(f[,1])
  n <- max(f)
#  p <- sum(f)/(n*nrow(f)*ncol(f))
  p <- 1-var(f)/mean(f)
  n <- max(n, as.integer(mean(f)/p))
  
  c <- c()
  for (i in 1:10000)
    c[i] <- bigen(p, n)


  p1<-qplot(as.vector(as.matrix(f)),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Binomial Distribution",caption=paste("Probability = ", p," number = ",n))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  p2<-qplot(as.data.frame(c),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Binomial Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(p1,p2, nrow =2)

  #return(c(n, p))
}

expgen.estimator <- function(file)
{
  f <- read.table(file)
  #return(mean(colMeans(f)))

  lambda = mean(colMeans(f))
  lambda
  c <- c()
  for (i in 1:1000)
    c[i] <- expgen(lambda)


  p1<-qplot(as.vector(as.matrix(f)),fill=..count..)+
    theme_classic()+scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Exponential Distribution",caption=paste("Lambda = ", lambda))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  p2<-qplot(as.data.frame(c),fill=..count..)+
    theme_classic()+scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Exponential Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(p1,p2, nrow =2)
}

gegen.estimator <- function(file)
{
  library(ggplot2)

  f <- read.table(file)
  p = 1/mean(colMeans(f))
  c <- c()
  for (i in 1:1000)
    c[i] <- gegen(p)


  p1<-qplot(as.vector(as.matrix(f)),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Geometric Distribution",caption=paste("Probability = ", p))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  p2<-qplot(as.data.frame(c),fill=..count..)+
    theme_classic()+geom_histogram(binwidth = 0.01)+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Geometric Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(p1,p2, nrow =2)
  #return(1/mean(colMeans(f)))

}

pogen.estimator <- function(file){
  library(ggplot2)

  results <- read.table(file, sep =" ")
  sum <- sum(results)
  n <- ncol(results)*nrow(results)

  t = 1
  lambda = sum/n

  c <- c()
  for (i in 1:10000)
    c[i] <- pogen(t, lambda)


  p1<-qplot(as.vector( as.matrix(results)),fill=..count..)+
    theme_classic()+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Poisson Distribution",caption=paste("Lambda = ", lambda," Time = ",t))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  p2<-qplot(as.data.frame(c),fill=..count..)+
    theme_classic()+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Poisson Distribution")+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
  ggarrange(p1,p2, nrow =2)

  #return(c(t,lambda))
}


gagen.estimator <- function(file){
  library(ggplot2)

  k = 1
  results <- read.table(file)
  sum <- sum(results)
  n <- ncol(results)*nrow(results)
  lambda = n/sum

  c <- c()
  for (i in 1:1000)
    c[i] <- gagen(k, lambda)


  p1<-qplot(as.vector(as.matrix(results)),fill=..count..)+
    theme_classic()+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Gamma Distribution",caption=paste("k = ", 1," Lambda =",lambda))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
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
  ggarrange(p1,q1,q2, nrow =3)
  #return(c(1,lambda))
}

nogen.estimator <- function (file){
  results <- read.table(file)
  sum <- sum(results)
  n <- ncol(results)*nrow(results)
  mean <- sum/n
  variance <- (sum((results - mean)^2) )/n


  c <- c()
  for (i in 1:1000)
    c[i] <- nogen(mean, variance)

  p1<-qplot(as.vector(as.matrix(results)),fill=..count..)+
    theme_classic()+
    scale_fill_gradient(low="blue", high="red")+
    labs(x="value",y="count",title="Normal Distribution",caption=paste("Mean = ", mean," Variance = ", variance))+
    theme(plot.background = element_rect(fill = "linen",color = "blue"))+
    theme(panel.background = element_rect(fill = "linen",color = "linen"))
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
  ggarrange(p1,q1,q2, nrow =3)

  #return(c(mean,variance))
}
