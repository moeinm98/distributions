if("shiny" %in% rownames(installed.packages()) == FALSE)
{install.packages("shiny")}

if("shinyFiles" %in% rownames(installed.packages()) == FALSE)
{install.packages("shinyFiles")}

library("shinythemes")
library("shinyFiles")

ui <- navbarPage(
  theme = shinytheme("cerulean"),
  "Distributions",

  tabPanel("Uniform",


           #Uniform - dugen TAB



           sidebarLayout(
             sidebarPanel(
               helpText("Select parameters:"),
               sliderInput("uniform_range", "Random number Range:", 0, 20, c(0,1)),
               tags$h5("Random number Generator:"),
               helpText("Tap 'Generate' button for generating a Uniform Random Number'"),

               actionButton("uniform_rand_gen_btn", "Generate", class = "btn-primary"),
               tags$p("Random generated number: ", textOutput("uniform_rand_num")),



               tags$h5("Draw Plot:"),
               actionButton("uniform_plot_btn", "Draw Plot", class = "btn-primary"),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that u upload I can estimate parameters of this distribution"),
               shinyFilesButton("uniform_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"),

               textOutput("uniform_txt_file")
             ),
             mainPanel(
               tags$h4("Uniform Plot:"),
               plotOutput("uniform_plot")

             )
           )
  ),



  tabPanel("Bernouli",


           #Bernouli - brgen TAB

           sidebarLayout(
             sidebarPanel(
               helpText("Select parameter:"),
               numericInput("bernouli_input", "Bernouli parameter :", value = 0.1,step = 0.1, min = 0, max = 1),
               tags$h5("Random number Generator:"),
               helpText("Tap 'Generate' button for generating a Bernouli Random Number'"),

               actionButton("bernouli_rand_gen_btn", "Generate", class = "btn-primary"),
               tags$p("Random generated number: ", textOutput("bernouli_rand_num")),



               tags$h5("Draw Plot:"),
               actionButton("bernouli_plot_btn", "Draw Plot", class = "btn-primary"),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that u upload I can estimate parameters of this distribution"),
               shinyFilesButton("bernouli_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"),

               textOutput("bernouli_txt_file")
             ),
             mainPanel(
               tags$h4("Bernouli Plot:"),
               plotOutput("bernouli_plot")

             )
           )

  ),
  tabPanel("Binomial",


           #Binomial - bigen TAB


           sidebarLayout(
             sidebarPanel(
               helpText("A binomial random variable can be seen as the result of repeated Bernoulli Trials."),
               numericInput("binomial_prob", "Binomial probability :", value = 0.1,step = 0.1, min = 0, max = 1),
               numericInput("binomial_n_trials", "Binomial number of trials", value = 1),
               tags$h5("Random number Generator:"),
               helpText("Tap 'Generate' button for generating a Binomial Random Number'"),

               actionButton("binomial_rand_gen_btn", "Generate", class = "btn-primary"),
               tags$p("Random generated number: ", textOutput("binomial_rand_num")),



               tags$h5("Draw Plot:"),
               actionButton("binomial_plot_btn", "Draw Plot", class = "btn-primary"),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that u upload I can estimate parameters of this distribution"),
               shinyFilesButton("binomial_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"),

               textOutput("binomial_txt_file")
             ),
             mainPanel(
               tags$h4("Binomial Plot:"),
               plotOutput("binomial_plot")

             )
           )

  ),
  tabPanel("Geometric",


           #Geometric - gegen TAB

           sidebarLayout(
             sidebarPanel(
               helpText("The number of failures in Bernoulli trials, between two wins, follows the geometric distributions."),
               helpText("Select Geometric Distribution parameter:"),
               numericInput("geometric_input", "Geometric parameter :", value = 0.1,step = 0.1, min = 0, max = 1),
               tags$h5("Random number Generator:"),
               helpText("Tap 'Generate' button for generating a Geometric Random Number'"),

               actionButton("geometric_rand_gen_btn", "Generate", class = "btn-primary"),
               tags$p("Random generated number: ", textOutput("geometric_rand_num")),



               tags$h5("Draw Plot:"),
               actionButton("geometric_plot_btn", "Draw Plot", class = "btn-primary"),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that u upload I can estimate parameters of this distribution"),
               shinyFilesButton("geometric_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"),

               textOutput("geometric_txt_file")
             ),
             mainPanel(
               tags$h4("Geometric Plot:"),
               plotOutput("geometric_plot")

             )
           )

  ),
  tabPanel("Exponential",


           #Exponential - expgen TAB

           sidebarLayout(
             sidebarPanel(
               helpText("Exponential distribution is a popular distribution which is used to model waiting times and memoryless processes."),
               helpText("Select Exponential Distribution parameter:"),
               numericInput("exponential_input", "Exponential parameter(λ) :", value = 0.1, step = 0.1, min = 0.1),
               tags$h5("Random number Generator:"),
               helpText("Tap 'Generate' button for generating a Exponential Random Number'"),

               actionButton("exponential_rand_gen_btn", "Generate", class = "btn-primary"),
               tags$p("Random generated number: ", textOutput("exponential_rand_num")),



               tags$h5("Draw Plot:"),
               actionButton("exponential_plot_btn", "Draw Plot", class = "btn-primary"),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that u upload I can estimate parameters of this distribution"),
               shinyFilesButton("exponential_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"),

               textOutput("exponential_txt_file")
             ),
             mainPanel(
               tags$h4("Exponential Plot:"),
               plotOutput("exponential_plot")

             )
           )

  ),
  tabPanel("Gamma",


           #Gamma - gagen TAB

           sidebarLayout(
             sidebarPanel(
               helpText("Summation of k i.i.d exponential random variables leads to a gamma distributed random variable."),
               helpText("Select Exponential Distribution parameter (λ) and k:"),
               numericInput("gamma_input", "Gamma parameter :", value = 0.1, step = 0.1, min = 0.1),
               numericInput("gamma_k", "k", value = 1, min = 1),
               tags$h5("Random number Generator:"),
               helpText("Tap 'Generate' button for generating a Gamma Random Number'"),

               actionButton("gamma_rand_gen_btn", "Generate", class = "btn-primary"),
               tags$p("Random generated number: ", textOutput("gamma_rand_num")),



               tags$h5("Draw Plot:"),
               actionButton("gamma_plot_btn", "Draw Plot", class = "btn-primary"),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that you upload I can estimate parameters of this distribution"),
               shinyFilesButton("gamma_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"),

               textOutput("gamma_txt_file")
             ),
             mainPanel(
               tags$h4("Gamma Plot:"),
               plotOutput("gamma_plot")

             )
           )

  ),
  tabPanel("Poisson",


           #Poisson - pogen TAB

           sidebarLayout(
             sidebarPanel(
               helpText("If an exponentially distributed variable is modeled as the waiting time before an arrival, the Poisson distributed variable can be modeled as the number of arrivals during a period of time of length t ."),
               helpText("Select Poisson Distribution lambda and t:"),
               numericInput("poisson_input", "Poisson parameter (λ) :", value = 0.1, step = 0.1, min = 0.1),
               numericInput("poisson_t", "time (t)", value = 0.1, min = 0.1, step = 0.1),
               tags$h5("Random number Generator:"),
               helpText("Tap 'Generate' button for generating a Poisson Random Number'"),

               actionButton("poisson_rand_gen_btn", "Generate", class = "btn-primary"),
               tags$p("Random generated number: ", textOutput("poisson_rand_num")),



               tags$h5("Draw Plot:"),
               actionButton("poisson_plot_btn", "Draw Plot", class = "btn-primary"),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that you upload I can estimate parameters of this distribution"),
               shinyFilesButton("poisson_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"),

               textOutput("poisson_txt_file")
             ),
             mainPanel(
               tags$h4("Poisson Plot:"),
               plotOutput("poisson_plot")

             )
           )

  ),
  tabPanel("Normal",


           #Normal - nogen TAB

           sidebarLayout(
             sidebarPanel(
               helpText("The Poisson (λ) distribution can be considered as an approximation of N(λ,λ)"),
               helpText("Select Normal Distribution mean (u) and variance (s):"),
               numericInput("normal_u", "Normal parameter mean (u) :", value = 0.1, step = 0.1, min = 0),
               numericInput("normal_s", "Normal parameter variance (s)", value = 0.1, step = 0.1, min = 0),
               tags$h5("Random number Generator:"),
               helpText("Tap 'Generate' button for generating a Normal Random Number'"),

               actionButton("normal_rand_gen_btn", "Generate", class = "btn-primary"),
               tags$p("Random generated number: ", textOutput("normal_rand_num")),



               tags$h5("Draw Plot:"),
               actionButton("normal_plot_btn", "Draw Plot", class = "btn-primary"),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that you upload I can estimate parameters of this distribution"),
               shinyFilesButton("normal_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"),

               textOutput("normal_txt_file")
             ),
             mainPanel(
               tags$h4("Normal Plot:"),
               plotOutput("normal_plot")

             )
           )

  )

)




server = function(input, output, session) {
  ##step 1
  rgenerator <- function(min , max , number){
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


  ##step 2
  dugen = function(min , max){
    return(rgenerator(min , max , 1))
  }
  ##step 3
  cugen= function(){
    result=c(rgenerator(0,1,1))
    return(result)
  }
  ##step 10
  dugen.visual <- function(min , max){
    library(ggplot2)
    result=c()
    for (i in 1:1000){
      result=c(result,dugen(min, max))
    }
    qplot(result)
  }
  cugen.visual <- function(){
    result=c()
    for (i in 1:1000){
      result=c(result,cugen())
    }
    qplot(result)
  }

  #step3
  brgen=function(p){
    s=cugen()
    if(s>p){
      return(1)
    }
    else{
      return(0)
    }
  }
  #step 10
  brgen.visual <- function(p){
    library(ggplot2)
    result=c()
    for (i in 1:1000){
      result=c(result,brgen(p))
    }
    qplot(result)
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
    for (i in 1:1000)
      c[i] <- bigen(p, n)
    qplot(c)
  }

  expgen <- function(lambda)
  {
    return(-log(cugen(), base = exp(1))/lambda)
  }

  expgen.visual <- function(p, n)
  {
    library(ggplot2)
    c <- c()
    for (i in 1:1000)
      c[i] <- expgen(p)
    qplot(c)
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
    qplot(c)
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
    library(ggplot2)
    c <- c()
    for (i in 1:1000)
      c[i] <- gagen(k, lambda)
    qplot(c)
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
    for (i in 1:1000)
      c[i] <- pogen(t, lambda)
    qplot(c)
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
    for (i in 1:1000)
      c[i] <- nogen(u, s)
    qplot(c)
  }


  ## dugen - uniform Tab


  output$uniform_rand_num <- renderText({ input$uniform_rand_gen_btn
    isolate(dugen(input$uniform_range[1], input$uniform_range[2])) })


  output$uniform_plot <- renderPlot({input$uniform_plot_btn
    isolate(dugen.visual(input$uniform_range[1], input$uniform_range[2])) })

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "uniform_choose_file", roots = volumes, session = session)

    if(!is.null(input$uniform_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$uniform_choose_file)
      output$uniform_txt_file <- renderText(as.character(file_selected$datapath))
    }
  })


  ## brgen - Bernouli Tab


  output$bernouli_rand_num <- renderText({ input$bernouli_rand_gen_btn
    isolate(brgen(input$bernouli_input)) })


  output$bernouli_plot <- renderPlot({input$bernouli_plot_btn
    isolate(brgen.visual(input$bernouli_input)) })

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "bernouli_choose_file", roots = volumes, session = session)

    if(!is.null(input$bernouli_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$bernouli_choose_file)
      output$bernouli_txt_file <- renderText(as.character(file_selected$datapath))
    }
  })


  ## binomial - Binomial Tab


  output$binomial_rand_num <- renderText({ input$binomial_rand_gen_btn
    isolate(bigen(input$binomial_prob, input$binomial_n_trials)) })


  output$binomial_plot <- renderPlot({input$binomial_plot_btn
    isolate(bigen.visual(input$binomial_prob, input$binomial_n_trials)) })

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "binomial_choose_file", roots = volumes, session = session)

    if(!is.null(input$binomial_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$binomial_choose_file)
      output$binomial_txt_file <- renderText(as.character(file_selected$datapath))
    }
  })


  ## gegen - Geometric Tab


  output$geometric_rand_num <- renderText({ input$geometric_rand_gen_btn
    isolate(gegen(input$geometric_input)) })


  output$geometric_plot <- renderPlot({input$geometric_plot_btn
    isolate(gegen.visual(input$geometric_input)) })

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "geometric_choose_file", roots = volumes, session = session)

    if(!is.null(input$geometric_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$geometric_choose_file)
      output$geometric_txt_file <- renderText(as.character(file_selected$datapath))
    }
  })



  ## expgen - Exponential Tab


  output$exponential_rand_num <- renderText({ input$exponential_rand_gen_btn
    isolate(expgen(input$exponential_input)) })


  output$exponential_plot <- renderPlot({input$exponential_plot_btn
    isolate(expgen.visual(input$exponential_input)) })

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "exponential_choose_file", roots = volumes, session = session)

    if(!is.null(input$exponential_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$exponential_choose_file)
      output$exponential_txt_file <- renderText(as.character(file_selected$datapath))
    }
  })



  ## gagen - Gamma Tab


  output$gamma_rand_num <- renderText({ input$gamma_rand_gen_btn
    isolate(gagen(input$gamma_k,input$gamma_input)) })


  output$gamma_plot <- renderPlot({input$gamma_plot_btn
    isolate(gagen.visual(input$gamma_k,input$gamma_input)) })

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "gamma_choose_file", roots = volumes, session = session)

    if(!is.null(input$gamma_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$gamma_choose_file)
      output$gamma_txt_file <- renderText(as.character(file_selected$datapath))
    }
  })


  ## pogen - Poisson Tab


  output$poisson_rand_num <- renderText({ input$poisson_rand_gen_btn
    isolate(pogen(input$poisson_t,input$poisson_input)) })


  output$poisson_plot <- renderPlot({input$poisson_plot_btn
    isolate(pogen.visual(input$poisson_t,input$poisson_input)) })

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "poisson_choose_file", roots = volumes, session = session)

    if(!is.null(input$poisson_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$poisson_choose_file)
      output$poisson_txt_file <- renderText(as.character(file_selected$datapath))
    }
  })


  ## nogen - Normal Tab


  output$normal_rand_num <- renderText({ input$normal_rand_gen_btn
    isolate(nogen(input$normal_u,input$normal_s)) })


  output$normal_plot <- renderPlot({input$normal_plot_btn
    isolate(nogen.visual(input$normal_u,input$normal_s)) })

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "normal_choose_file", roots = volumes, session = session)

    if(!is.null(input$normal_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$normal_choose_file)
      output$normal_txt_file <- renderText(as.character(file_selected$datapath))
    }
  })

}
#λ
shinyApp(ui = ui, server = server)
