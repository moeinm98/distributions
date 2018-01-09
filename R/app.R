if("shiny" %in% rownames(installed.packages()) == FALSE)
{install.packages("shiny")}

if("shinyFiles" %in% rownames(installed.packages()) == FALSE)
{install.packages("shinyFiles")}

#install.packages("ggpubr")
library("ggpubr")
library("ggplot2")
library("shinythemes")
library("shinyFiles")

source("dugen.R")
source("cugen.R")
source("bigen.R")
source("brgen.R")
source("expgen.R")
source("gagen.R")
source("gegen.R")
source("nogen.R")
source("pogen.R")
source("rgenerator.R")
source("estimators.R")




ui <- navbarPage(
  theme = shinytheme("united"),
  "Distributions",

  tabPanel("Uniform",


           #Uniform - dugen TAB



           sidebarLayout(
             sidebarPanel(
               helpText("Select minimum and maximum of uniform distribution:"),
               fluidRow(
                 column(6,
                        numericInput("uniform_min", "Min:", value = 0)),
                 column(6,
                        numericInput("uniform_max", "Max:", value = 1))
               ),

               tags$h5("Random number Generator:"),
               actionButton("uniform_rand_gen_btn", "Generate Random", class = "btn-primary"),
               textOutput("uniform_rand_num"),

               tags$h5("Draw Plot:"),
               actionButton("uniform_plot_btn", "Draw Plot", class = "btn-primary", icon = icon("bar-chart")),

               tags$h5("Choose the estimation file:"),
               helpText("with a estimation file that u upload I can estimate parameters of this distribution"),

               shinyFilesButton("uniform_choose_file", "Choose a file" ,
                                title = "Please select a file:", multiple = FALSE,
                                buttonType = "default", class = "btn-primary"
               ),

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
               helpText("Tap 'Generate' button for generating a Bernouli Random Number'"),

               actionButton("bernouli_rand_gen_btn", "Generate Random", class = "btn-primary"),
               textOutput("bernouli_rand_num"),



               tags$h5("Draw Plot:"),
               actionButton("bernouli_plot_btn", "Draw Plot", class = "btn-primary", icon = icon("bar-chart")),

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
               helpText("Tap 'Generate' button for generating a Binomial Random Number'"),

               actionButton("binomial_rand_gen_btn", "Generate Random", class = "btn-primary"),
               textOutput("binomial_rand_num"),

               tags$h5("Draw Plot:"),
               actionButton("binomial_plot_btn", "Draw Plot", class = "btn-primary", icon = icon("bar-chart")),

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
               helpText("Tap 'Generate' button for generating a Geometric Random Number'"),

               actionButton("geometric_rand_gen_btn", "Generate Random", class = "btn-primary"),
               textOutput("geometric_rand_num"),


               tags$h5("Draw Plot:"),
               actionButton("geometric_plot_btn", "Draw Plot", class = "btn-primary", icon = icon("bar-chart")),

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
               helpText("Tap 'Generate' button for generating a Exponential Random Number'"),

               actionButton("exponential_rand_gen_btn", "Generate Random", class = "btn-primary"),
               textOutput("exponential_rand_num"),



               tags$h5("Draw Plot:"),
               actionButton("exponential_plot_btn", "Draw Plot", class = "btn-primary", icon = icon("bar-chart")),

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
               helpText("Tap 'Generate' button for generating a Gamma Random Number'"),

               actionButton("gamma_rand_gen_btn", "Generate Random", class = "btn-primary"),
               textOutput("gamma_rand_num"),

               tags$h5("Draw Plot:"),
               actionButton("gamma_plot_btn", "Draw Plot", class = "btn-primary", icon = icon("bar-chart")),

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
               textOutput("poisson_rand_num"),



               tags$h5("Draw Plot:"),
               actionButton("poisson_plot_btn", "Draw Plot", class = "btn-primary", icon = icon("bar-chart")),

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
               textOutput("normal_rand_num"),



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


  ## dugen - uniform Tab


  observeEvent(input$uniform_min, {
    if(input$uniform_min >= input$uniform_max){
      updateNumericInput(session, "uniform_max", value = input$uniform_min + 1)

    }
  })
  observeEvent(input$uniform_max, {
    if(input$uniform_min >= input$uniform_max){
      updateNumericInput(session, "uniform_min", value = input$uniform_max - 1)

    }
  })



  output$uniform_rand_num <- eventReactive(input$uniform_rand_gen_btn,{
    ans <- dugen(input$uniform_min, input$uniform_max)
    paste("Random number is:", ans)
  })


  uplot <- eventReactive(input$uniform_plot_btn,{
    dugen.visual(input$uniform_min, input$uniform_max)

  })
  output$uniform_plot <- renderPlot(
    uplot()
  )

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "uniform_choose_file", roots = volumes, session = session)

    if(!is.null(input$uniform_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$uniform_choose_file)
      estimated <- dugen.estimator(as.character(file_selected$datapath))
      output$uniform_plot <- renderPlot(
        estimated
      )
      output$uniform_txt_file <- renderText(as.character(file_selected$datapath))

    }
  })

  ## brgen - Bernouli Tab

  output$bernouli_rand_num <- eventReactive(input$bernouli_rand_gen_btn,{
    br_ans <- brgen(input$bernouli_input)
    paste("Random number is:", br_ans)
  })


  brplot <- eventReactive(input$bernouli_plot_btn,{
    brgen.visual(input$bernouli_input)

  })
  output$bernouli_plot <- renderPlot(
    brplot()
  )


  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "bernouli_choose_file", roots = volumes, session = session)

    if(!is.null(input$bernouli_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$bernouli_choose_file)
      estimated <- brgen.estimator(as.character(file_selected$datapath))
      output$bernouli_plot <- renderPlot(
        estimated
      )
    }
  })


  ## binomial - Binomial Tab

  output$binomial_rand_num <- eventReactive(input$binomial_rand_gen_btn,{
    bi_ans <- bigen(input$binomial_prob, input$binomial_n_trials)
    paste("Random number is:", bi_ans)
  })


  biplot <- eventReactive(input$binomial_plot_btn,{
    bigen.visual(input$binomial_prob, input$binomial_n_trials)

  })
  output$binomial_plot <- renderPlot(
    biplot()
  )


  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "binomial_choose_file", roots = volumes, session = session)

    if(!is.null(input$binomial_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$binomial_choose_file)
      estimated <- bigen.estimator(as.character(file_selected$datapath))
      output$binomial_plot <- renderPlot(
        estimated
      )
    }
  })


  ## gegen - Geometric Tab


  output$geometric_rand_num <- eventReactive(input$geometric_rand_gen_btn,{
    ge_ans <- gegen(input$geometric_input)
    paste("Random number is:", ge_ans)
  })


  geplot <- eventReactive(input$geometric_plot_btn,{
    gegen.visual(input$geometric_input)

  })
  output$geometric_plot <- renderPlot(
    geplot()
  )




  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "geometric_choose_file", roots = volumes, session = session)

    if(!is.null(input$geometric_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$geometric_choose_file)
      estimated <- gegen.estimator(as.character(file_selected$datapath))
      output$geometric_plot <- renderPlot(
        estimated
      )
    }
  })



  ## expgen - Exponential Tab

  output$exponential_rand_num <- eventReactive(input$exponential_rand_gen_btn,{
    exp_ans <- expgen(input$exponential_input)
    paste("Random number is:", exp_ans)
  })


  expplot <- eventReactive(input$exponential_plot_btn,{
    expgen.visual(input$exponential_input)

  })
  output$exponential_plot <- renderPlot(
    expplot()
  )


  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "exponential_choose_file", roots = volumes, session = session)

    if(!is.null(input$exponential_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$exponential_choose_file)
      estimated <- expgen.estimator(as.character(file_selected$datapath))
      output$exponential_plot <- renderPlot(
        estimated
      )
    }
  })



  ## gagen - Gamma Tab

  output$gamma_rand_num <- eventReactive(input$gamma_rand_gen_btn,{
    ga_ans <- gagen(input$gamma_k,input$gamma_input)
    paste("Random number is:", ga_ans)
  })


  gaplot <- eventReactive(input$gamma_plot_btn,{
    gagen.visual(input$gamma_k,input$gamma_input)

  })
  output$gamma_plot <- renderPlot(
    gaplot()
  )


  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "gamma_choose_file", roots = volumes, session = session)

    if(!is.null(input$gamma_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$gamma_choose_file)
      estimated <- gagen.estimator(as.character(file_selected$datapath))
      output$gamma_plot <- renderPlot(
        estimated
      )    }
  })


  ## pogen - Poisson Tab

  output$poisson_rand_num <- eventReactive(input$poisson_rand_gen_btn,{
    po_ans <- pogen(input$poisson_t,input$poisson_input)
    paste("Random number is:", po_ans)
  })


  poplot <- eventReactive(input$poisson_plot_btn,{
    pogen.visual(input$poisson_t,input$poisson_input)

  })
  output$poisson_plot <- renderPlot(
    poplot()
  )

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "poisson_choose_file", roots = volumes, session = session)

    if(!is.null(input$poisson_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$poisson_choose_file)
      estimated <- pogen.estimator(as.character(file_selected$datapath))
      output$poisson_plot <- renderPlot(
        estimated
      )    }
  })


  ## nogen - Normal Tab

  output$normal_rand_num <- eventReactive(input$normal_rand_gen_btn,{
    no_ans <- nogen(input$normal_u,input$normal_s)
    paste("Random number is:", no_ans)
  })


  noplot <- eventReactive(input$normal_plot_btn,{
    nogen.visual(input$normal_u,input$normal_s)

  })
  output$normal_plot <- renderPlot(
    noplot()
  )

  volumes = getVolumes()
  observe({
    shinyFileChoose(input, "normal_choose_file", roots = volumes, session = session)

    if(!is.null(input$normal_choose_file)){
      # browser()
      file_selected<-parseFilePaths(volumes, input$normal_choose_file)
      estimated <- nogen.estimator(as.character(file_selected$datapath))
      output$normal_plot <- renderPlot(
        estimated
      )    }
  })

}
#λ
shinyApp(ui = ui, server = server)
