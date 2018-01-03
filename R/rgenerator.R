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
