
#install.packages('rsconnect')
#install.packages('shinyWidgets')
library(rsconnect)
library(markdown)
library(knitr)
library(shinyWidgets)

# rsconnect::setAccountInfo(name='kimberlycardenas',
#                           token='1BFB8E11895A9A3016A3FF1BED945501',
#                           secret='Zdwim0dRiv8ZUgoTl/gJKr8bMQBflgYvHvSlUh/S')
# rsconnect::deployApp('new/workonthisone')
library(shiny)

knit("basic.rmd")

ui <- navbarPage(position = ("static-top"), collapsible = TRUE, fluid = TRUE,  title="", setBackgroundColor(
  color = c("#AFD5EB", "#AFD5EB"), gradient = "radial", direction = c("top", "left")), 
               
                 
                 
                #this changes whole background where the tabs are, not individual tabs 
                 #tags$style(HTML("
                # .navbar { background-color:blue;}
               #  ")),  
               #wont use this
               #ffffff00 just makes it transparent
               #I want different color pallet?? to match sliders but doesnt let me 
               
               #h1("Colored Tabs"),
               tags$style(HTML("
      .navbar { background-color:#ffffff00;}
     .nav > li > a[data-value='Introduction']                  {background-color: #FFCE54;  color:black}
     .nav > li > a[data-value='Basic Disease Explainer'] {background-color: #ED5564;   color:white}
     .nav > li > a[data-value='Basic Disease (Interactive)'] {background-color: #ED5564;   color:white}
     .nav > li > a[data-value='Protective Behavior Explainer'] {background-color: #A0D568;  color:white}
     .nav > li > a[data-value='Protective Behavior (Interactive)'] {background-color: #A0D568;  color:white}
     .nav > li > a[data-value='Awareness Explainer'] {background-color: #4FC1E8; color:white}
     .nav > li > a[data-value='Awareness (Interactive)'] {background-color: #4FC1E8; color:white}
     .nav > li > a[data-value='Groups Explainer'] {background-color: #AC92EB; color:white}
     .nav > li > a[data-value='Groups (Interactive)'] {background-color: #AC92EB; color:white}
     .nav > li > a[data-value='Credits']                  {background-color: #FFCE54;  color:black}
  ")),
              

        #sidebarLayout(sidebarPanel, position = "left", fluid=TRUE),       
                 tabPanel(title = "Introduction", 
                  uiOutput("Overview")),
                
                 
                 tabPanel(title="Basic Disease Explainer",
                          withMathJax(HTML(markdown::markdownToHTML(knit("basic.md", quiet=TRUE))))),
                 
                 tabPanel(title = "Basic Disease (Interactive)",
                          withMathJax(),
                          # tags$div(HTML("<script type='text/x-mathjax-config'>
                          #                   MathJax.Hub.Config({
                          #                   tex2jax: {inlineMath: [['$','$']]}
                          #                   });
                          #                   </script>")),
                          #"#A0D568", "#A0D568", "#A0D568", "#4FC1E8", "#4FC1E8", "#4FC1E8", "#4FC1E8", "#4FC1E8", "#4FC1E8", "#4FC1E8", "#AC92EB", "#AC92EB", "#AC92EB", "#ED5564", "#D55E00"
                          setSliderColor(c("#ED5564", "#ED5564", "#ED5564", "#ED5564",
                                           "#A0D568", "#A0D568", "#ED5564", "#ED5564", "#ED5564", "#ED5564",
                                           "#4FC1E8", "#4FC1E8", "#4FC1E8", "#A0D568", "#A0D568", "#ED5564", "#ED5564", "#ED5564", "#ED5564",
                                           "#AC92EB", "#AC92EB", "#AC92EB", "#AC92EB", "#4FC1E8", "#4FC1E8", "#4FC1E8", "#A0D568", "#ED5564", "#ED5564", "#ED5564", "#ED5564"), 
                                         1:31),
                          
                          fluidRow(
                            column(7, plotOutput("SIRPlot_basic")),
                            column(4,htmlOutput("text_basic"))
                          ),
                          sliderInput(inputId = "b1",
                                      label = withMathJax("Transmission coefficient (β)"),
                                      value = 0.2, min = 0, max = 1),
                          sliderInput(inputId = "r1",
                                      label = withMathJax("Infectious period (1/ρ)"),
                                      value = 10, min = 1, max = 30),
                          sliderInput(inputId = "m1",
                                      label =  withMathJax("Fatality probability (μ)"),
                                      value = .00, min = 0, max = 0.2),
                          sliderInput(inputId = "t1",
                                      label =  withMathJax("Simulation duration (days)"),
                                      value = 200, min = 0, max = 2500)
                 ),
                 tabPanel(title="Protective Behavior Explainer",
                          uiOutput("explain_protective")),
                 tabPanel(title = "Protective Behavior (Interactive)",
                          fluidRow(
                            column(7, plotOutput("SIRPlot_protective")),
                            column(4,htmlOutput("text_protective"))
                          ),
                          
                          fluidRow(
                            column(5,
                                   
                                   sliderInput(inputId = "kappa2",
                                               label =HTML("Protection efficacy (κ)<br/>Note: greater values mean the behavior is less protective."),
                                               value = 0.3, min = 0, max = 1),
                                   sliderInput(inputId = "P02",
                                               label =  withMathJax("Initial protection prevalence (P₀)"),
                                               value = 0.5, min = 0, max = 1)
                                   
                                   
                            ),
                            column(5,
                                   sliderInput(inputId = "b2",
                                               label = withMathJax("Transmission coefficient (β)"),
                                               value = 0.2, min = 0, max = 1),
                                   sliderInput(inputId = "r2",
                                               label = withMathJax("Infectious period (1/ρ)"),
                                               value = 10, min = 1, max = 30),
                                   sliderInput(inputId = "m2",
                                               label =  withMathJax("Fatality probability (μ)"),
                                               value = .00, min = 0, max = 0.2),
                                   sliderInput(inputId = "t2",
                                               label =  withMathJax("Simulation duration (days)"),
                                               value = 200, min = 0, max = 2500)
                            ))
                          
                 ),
                 
                 tabPanel(title="Awareness Explainer",
                          uiOutput("explain_awareness")),
                 tabPanel(title = "Awareness (Interactive)",
                          fluidRow(
                            column(7, plotOutput("SIRPlot_awareness")),
                            column(4,htmlOutput("text_awareness"))
                          ),
                          
                          # change color sliders and add number 
                          
                          fluidRow(
                            column(4,
                                   
                                   sliderInput(inputId = "theta3",
                                               label = withMathJax("Responsiveness (θ)"),
                                               value = 100, min = 0, max = 1000),
                                   sliderInput(inputId = "phi3",
                                               label =  withMathJax("Fatigue (Φ)"),
                                               value = 0, min = 0, max = 1),    
                                   sliderInput(inputId = "ell3",
                                               label =  withMathJax("Memory (ℓ)"),
                                               value = 1, min = 0, max = 1000)  
                                   
                            ),
                            column(4,
                                   
                                   sliderInput(inputId = "kappa3",
                                               HTML("Protection efficacy (κ)<br/>Note: greater values mean the behavior is less protective."),
                                               value = 0.3, min = 0, max = 1),
                                   sliderInput(inputId = "P03",
                                               label =  withMathJax("Initial protection prevalence (P₀)"),
                                               value = 0.5, min = 0, max = 1)     
                                   
                            ),
                            column(4,
                                   sliderInput(inputId = "b3",
                                               label = withMathJax("Transmission coefficient (β)"),
                                               value = 0.2, min = 0, max = 1),
                                   sliderInput(inputId = "r3",
                                               label = withMathJax("Infectious period (ρ)"),
                                               value = 10, min = 1, max = 30),
                                   sliderInput(inputId = "m3",
                                               label =  withMathJax("Fatality probability (μ)"),
                                               value = .01, min = 0, max = 1),
                                   sliderInput(inputId = "t3",
                                               label =  withMathJax("Simulation duration (days)"),
                                               value = 200, min = 0, max = 2500)
                            ))),
                 
                 tabPanel(title="Groups Explainer",
                          uiOutput("explain_groups")),
                 tabPanel(title = "Groups (Interactive)",
                          fluidRow(
                            column(7, plotOutput("SIRPlot_groups")),
                            column(4,htmlOutput("text_groups"))
                          ),
                          
                          fluidRow(
                            column(3,
                                   
                                   sliderInput(inputId = "epsilon4",
                                               label = withMathJax("Awareness separation (ε)"),
                                               value = 0.99, min = 0.5, max = 1),
                                   sliderInput(inputId = "h4",
                                               label =  withMathJax("Mixing separation (h)"),
                                               value = 0.99, min = 0.5, max = 1),
                                   sliderInput(inputId = "I0_a4",
                                               label =  HTML(paste0("Initial group a infection prevalence (I₀",tags$sub("a"),")")),
                                               value = 0.001, min = 0, max = .5),
                                   sliderInput(inputId = "I0_b4",
                                               label =  HTML(paste0("Initial group b infection prevalence (I₀",tags$sub("b"),")")),
                                                value = 0, min = 0, max = .5)    
                                   
                            ),
                            column(3,
                                   
                                   sliderInput(inputId = "theta4",
                                               label = withMathJax("Responsiveness (θ)"),
                                               value = 100, min = 0, max = 1000),
                                   
                                   sliderInput(inputId = "phi4",
                                               label =  withMathJax("Fatigue (Φ)"),
                                               value = 0, min = 0, max = 1),    
                                   sliderInput(inputId = "ell4",
                                               label =  withMathJax("Memory (ℓ)"),
                                               value = 1, min = 0, max = 1000)  
                                   
                            ),
                            column(3,
                                   
                                   sliderInput(inputId = "kappa4",
                                               HTML("Protection efficacy (κ)<br/>Note: greater values mean the behavior is less protective."),
                                               value = 0.3, min = 0, max = 1) 
                                   
                            ),
                            column(3,
                                   sliderInput(inputId = "b4",
                                               label = withMathJax("Transmission coefficient (β)"),
                                               value = 0.2, min = 0, max = 1),
                                   sliderInput(inputId = "r4",
                                               label = withMathJax("Infectious period (1/ρ)"),
                                               value = 10, min = 1, max = 30),    
                                   sliderInput(inputId = "mu4",
                                               label =  withMathJax("Fatality probability (μ)"),
                                               value = .01, min = 0, max = 1),
                                   sliderInput(inputId = "t4",
                                               label =  withMathJax("Simulation duration (days)"),
                                               value = 200, min = 0, max = 2500)
                            )
                          )
                 ),
                 
                 tabPanel(title="Credits",
                          uiOutput("Credits")),
                 
                 
)


server <- function(input,output){
  
 # output$explain_basic  <- renderUI({
  
    #withMathJax(HTML(markdown::markdownToHTML("test.md")))
  #}) 
  # output$explain_basic <- renderUI({
   #   
   # })
  
  output$SIRPlot_basic <- renderPlot({
    library(ggthemes)
    library(magrittr)
    library(dplyr)
    library(deSolve)
    library(ggplot2)
    library(extrafont)
    SIR_run <- function(b=.2,r=.1, m=.005, I0=.05, t=50){
      model <- function (time, init, parms) {
        with(as.list(c(init, parms)), {
          dS <- -b*S*I
          dI <-  b*S*I-r*I
          dR <- r*(1-m)*I
          dD <- r*m*I
          list(c(dS, dI, dR, dD))
        })
      }
      init<-c(S=1-I0, I=I0, R=0, D=0)
      parms <- c(b=b, r=r, m=m)
      time <- seq(1, t, 1)
      sir_df <- ode(init, time, model,parms)
      return(data.frame(sir_df))
    }
    df <- SIR_run(t=input$t1, r=1/input$r1 , b=input$b1, m=input$m1) 
    
    ggplot() +
      geom_line(data = df, size = 1.5, alpha =0.9, aes(x = time, y = I), col="black")+
      # geom_line(data = data.frame(outbreak), size = 1.5, alpha =0.9, aes(x = time, y = S,  col="Blue"))+
      labs(title = "",
           x="Time",
           y="Infections")+
      theme_fivethirtyeight()+
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.title = element_text(),
            legend.position = "none")
  })
  
  output$text_basic <- renderUI({
    library(ggthemes)
    library(magrittr)
    library(dplyr)
    library(deSolve)
    library(ggplot2)
    library(extrafont)
    SIR_run <- function(b=.2,r=.1, m=.005, I0=.05, t=50){
      model <- function (time, init, parms) {
        with(as.list(c(init, parms)), {
          dS <- -b*S*I
          dI <-  b*S*I-r*I
          dR <- r*(1-m)*I
          dD <- r*m*I
          list(c(dS, dI, dR, dD))
        })
      }
      init<-c(S=1-I0, I=I0, R=0, D=0)
      parms <- c(b=b, r=r, m=m)
      time <- seq(1, t, 1)
      sir_df <- ode(init, time, model,parms)
      return(data.frame(sir_df))
    }
    df <- SIR_run(t=input$t1, r=1/input$r1 , b=input$b1, m=input$m1) 
    SIR_analyze <- function(df) { 
      herd_immunity_threshold <- df |>
        mutate(cases = I + R + D) |>
        filter(time == which.max(df$I)) |>
        select(cases)|>
        as.numeric()  
      cumulative <- df |>
        mutate(cases = I + R) |>
        tail(1) |>
        select(cases) |>
        as.numeric()  
      cum_deaths <- tail(df$D, 1)
      PII <- max(df$I)
      when_peak <- which.max(df$I)
      # deaths <- df |> 
      #   select(D)|>
      #   tail(1)
      # as.numeric()
      
      #delete/reinsert separate r script . focus on mutate places and line thAT comes before it and whether what I give to mutate makes sense. By wednesday havent figured it out send it. 
      
      
      return(list(HIT = herd_immunity_threshold,
                  CI = cumulative,
                  PII = PII,
                  Peak = when_peak,
                  CD = cum_deaths))
    }
    
    SIRvalues<-SIR_analyze(df)
    
    
    string_cuminf <- paste("Cumulative infection incidence:", signif(SIRvalues$CI, digits = 3))
    string_peakinf <- paste("Peak infection prevalence:", signif(SIRvalues$PII, digits = 3))
    string_whenpeak <- paste("Infection prevalence peak on day:", signif(SIRvalues$Peak, digits = 3))
    string_cumdth <- paste("Cumulative mortality:", signif(SIRvalues$CD, digits = 3))
    string_herdimm <-paste("Herd immunity threshold:", signif(SIRvalues$HIT, digits=3))
    string_R0 <- paste("R0:", signif(input$b1*input$r1, digits=3))
    HTML(paste("<br/><br/><br/>",string_cuminf, string_peakinf, string_whenpeak, string_cumdth, string_herdimm, string_R0, sep = '<br/>'))
    
  })
  
  
  
  output$explain_protective <- renderUI({
    withMathJax(HTML(markdown::markdownToHTML(knit("protective.rmd", quiet=TRUE))))
  })
  
  
  
  output$SIRPlot_protective <- renderPlot({
    library(ggthemes)
    library(magrittr)
    library(dplyr)
    library(deSolve)
    library(ggplot2)
    library(extrafont)
    SIR_w_pb_run <- function(b=.2,r=.1,m=.005, I0=.05, P0= 0.5, k=0.4, t=50){
      model <- function (time, init, parms) {
        with(as.list(c(init, parms)), {
          dSU <- -b*SU*(IU+k*IP)
          dSP <- -b*k*SP*(IU+k*IP)
          dIU <-  b*SU*(IU+k*IP)-r*IU
          dIP <-  b*k*SP*(IU+k*IP)-r*IP
          dRU <- (1-m)*(r)*(IU)
          dRP <- (1-m)*(r)*(IP)
          dDU <- m*r*IU
          dDP <-m*r*IP
          list(c(dSU, dSP, dIU, dIP, dRU, dRP, dDU, dDP))
        })
      }
      init<-c(SU=(1-P0)*(1-I0), SP=P0*(1-I0), IU=(1-P0)*I0, IP=P0*I0, RU=0, RP=0, DU=0, DP=0)
      parms <- c(b=b, r=r, k=k, m=m)
      time <- seq(1, t, 1)
      sir_df <- ode(init, time, model,parms)
      return(data.frame(sir_df))
    }
    df <- SIR_w_pb_run(t=input$t2, k=input$kappa2 , P0=input$P02, r=1/input$r2 , b=input$b2, m=input$m2)|>
      mutate(S=SU+SP,
             I=IU+IP,
             R=RU+RP,
             D=DU+DP)%>%
      mutate(N=S+I+R+D)
    ggplot() +
      geom_line(data = data.frame(df), size = 1.5, alpha =0.9, aes(x = time, y = I), col="black")+
      # geom_line(data = data.frame(outbreak), size = 1.5, alpha =0.9, aes(x = time, y = S,  col="Blue"))+
      labs(title = "",
           x="Time",
           y="Infections")+
      theme_fivethirtyeight()+
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.title = element_text(),
            legend.position = "none")
  })
  
  
  output$text_protective <- renderUI({
    library(ggthemes)
    library(magrittr)
    library(dplyr)
    library(deSolve)
    library(ggplot2)
    library(extrafont)
    SIR_w_pb_run <- function(b=.2,r=.1,m=.005, I0=.05, P0= 0.5, k=0.4, t=50){
      model <- function (time, init, parms) {
        with(as.list(c(init, parms)), {
          dSU <- -b*SU*(IU+k*IP)
          dSP <- -b*k*SP*(IU+k*IP)
          dIU <-  b*SU*(IU+k*IP)-r*IU
          dIP <-  b*k*SP*(IU+k*IP)-r*IP
          dRU <- (1-m)*(r)*(IU)
          dRP <- (1-m)*(r)*(IP)
          dDU <- m*r*IU
          dDP <-m*r*IP
          list(c(dSU, dSP, dIU, dIP, dRU, dRP, dDU, dDP))
        })
      }
      init<-c(SU=(1-P0)*(1-I0), SP=P0*(1-I0), IU=(1-P0)*I0, IP=P0*I0, RU=0, RP=0, DU=0, DP=0)
      parms <- c(b=b, r=r, k=k, m=m)
      time <- seq(1, t, 1)
      sir_df <- ode(init, time, model,parms)
      return(data.frame(sir_df))
    }
    df <- SIR_w_pb_run(t=input$t2, k=input$kappa2 , P0=input$P02, r=1/input$r2 , b=input$b2, m=input$m2)|>
      mutate(S=SU+SP,
             I=IU+IP,
             R=RU+RP,
             D=DU+DP)%>%
      mutate(N=S+I+R+D)
    SIR_analyze <- function(df) { 
      herd_immunity_threshold <- df |>
        mutate(cases = I + R) |>
        filter(time == which.max(df$I)) |>
        select(cases) |>
        as.numeric()  
      cumulative <- df |>
        mutate(cases = I + R + D) |>
        tail(1) |>
        select(cases) |>
        as.numeric()  
      PII <- max(df$I)
      when_peak <- which.max(df$I)
      cum_deaths <- tail(df$D, 1)
      # deaths <- df |>
      # select(D)|>
      #tail(1)
      # as.numeric()
      
      return(list(HIT = herd_immunity_threshold,
                  CI = cumulative,
                  PII = PII,
                  Peak = when_peak,
                  CD = cum_deaths))
    }
    
    
    SIRvalues2<-SIR_analyze(df)
    
    string_cuminf <- paste("Note: Changing parameters may change y-axis scale.<br/>Cumulative infection incidence:", signif(SIRvalues2$CI, digits = 3))
    string_peakinf <- paste("Peak infection prevalence:", signif(SIRvalues2$PII, digits = 3))
    #paste(" total number of deaths are", signif(SIRvalues2$D, digits = 3))
    string_whenpeak <- paste("Infection prevalence peak on day:", signif(SIRvalues2$Peak, digits = 3))
    string_cumdth <- paste("Cumulative mortality:", signif(SIRvalues2$CD, digits = 3))
    
    HTML(paste("<br/><br/><br/>",string_cuminf, string_peakinf, string_whenpeak, string_cumdth, sep = '<br/>'))
    
  })
  
  output$explain_awareness <- renderUI({
    withMathJax(HTML(markdown::markdownToHTML(knit("awareness.Rmd", quiet=TRUE))))
  })
  output$SIRPlot_awareness <- renderPlot({
    library(ggthemes)
    library(magrittr)
    library(dplyr)
    library(deSolve)
    library(ggplot2)
    library(extrafont)
    SIR_w_pbandawareness_run <- function(b=.2,r=.1,I0=.05, P0= 0.5, k=0.4, m=.005, time=50, theta=10,phi=0.01, ell=30){
      model <- function (time, init, parms) {
        with(as.list(c(init, parms)), {
          if(time<(ell+1)){
            lag<-rep(0, 8)
          }
          else{
            lag<-lagvalue(time-ell)
          }
          
          dSU <- -b*SU*(IU+k*IP)-theta*SU*(DU+DP-lag[7]-lag[8])+phi*SP
          dSP <- -b*k*SP*(IU+k*IP)+theta*SU*(DU+DP-lag[7]-lag[8])-phi*SP
          dIU <- b*SU*(IU+k*IP)-(r)*IU-theta*IU*(DU+DP-lag[7]-lag[8])+phi*IP
          dIP <- b*k*SP*(IU+k*IP)-(r)*IP+theta*IU*(DU+DP-lag[7]-lag[8])-phi*IP
          dRU <- r*(1-m)*IU-theta*RU*(DU+DP-lag[7]-lag[8])+phi*RP
          dRP <- r*(1-m)*IP+theta*RU*(DU+DP-lag[7]-lag[8])-phi*RP
          dDU <- r*m*IU
          dDP <- r*m*IP
          list(c(dSU, dSP, dIU, dIP, dRU, dRP, dDU, dDP))
        })
      }
      init<-c(SU=(1-P0)*(1-I0), SP=P0*(1-I0), IU=(1-P0)*I0, IP=P0*I0, RU=0, RP=0, DU=0, DP=0)
      parms <- c(b=b, r=r, k=k, m=m, theta=theta, phi=phi, ell=ell)
      time <- seq(1, time, 1)
      sir_df <- dede(init, time, model,parms)
      return(data.frame(sir_df))
    }
    
    df<- SIR_w_pbandawareness_run(time=input$t3, k=input$kappa3, P0=input$P03, b=input$b3, r=1/input$r3, theta=input$theta3, phi=input$phi3, ell=input$ell3, m=input$m3)|>
      mutate(S=SU+SP,
             I=IU+IP, 
             R=RU+RP,
             D=DU+DP)|>
      mutate(N=S+I+R+D)
    
    ggplot() +
      geom_line(data = data.frame(df), size = 1.5, alpha =0.9, aes(x = time, y = I), col="black")+
      # geom_line(data = data.frame(outbreak), size = 1.5, alpha =0.9, aes(x = time, y = S,  col="Blue"))+
      labs(title = "",
           x="Time",
           y="Infections")+
      theme_fivethirtyeight()+
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            axis.title = element_text())
  })
  
  output$text_awareness <- renderUI({
    library(ggthemes)
    library(magrittr)
    library(dplyr)
    library(deSolve)
    library(ggplot2)
    library(extrafont)
    SIR_w_pbandawareness_run <- function(b=.2,r=.1,I0=.05, P0= 0.5, k=0.4, m=.005, time=50, theta=10,phi=0.01, ell=30){
      model <- function (time, init, parms) {
        with(as.list(c(init, parms)), {
          if(time<(ell+1)){
            lag<-rep(0, 8)
          }
          else{
            lag<-lagvalue(time-ell)
          }
          
          dSU <- -b*SU*(IU+k*IP)-theta*SU*(DU+DP-lag[7]-lag[8])+phi*SP
          dSP <- -b*k*SP*(IU+k*IP)+theta*SU*(DU+DP-lag[7]-lag[8])-phi*SP
          dIU <- b*SU*(IU+k*IP)-(r)*IU-theta*IU*(DU+DP-lag[7]-lag[8])+phi*IP
          dIP <- b*k*SP*(IU+k*IP)-(r)*IP+theta*IU*(DU+DP-lag[7]-lag[8])-phi*IP
          dRU <- r*(1-m)*IU-theta*RU*(DU+DP-lag[7]-lag[8])+phi*RP
          dRP <- r*(1-m)*IP+theta*RU*(DU+DP-lag[7]-lag[8])-phi*RP
          dDU <- r*m*IU
          dDP <- r*m*IP
          list(c(dSU, dSP, dIU, dIP, dRU, dRP, dDU, dDP))
        })
      }
      init<-c(SU=(1-P0)*(1-I0), SP=P0*(1-I0), IU=(1-P0)*I0, IP=P0*I0, RU=0, RP=0, DU=0, DP=0)
      parms <- c(b=b, r=r, k=k, m=m, theta=theta, phi=phi, ell=ell)
      time <- seq(1, time, 1)
      sir_df <- dede(init, time, model,parms)
      return(data.frame(sir_df))
    }
    df <- SIR_w_pbandawareness_run(time=input$t3, k=input$kappa3, P0=input$P03, b=input$b3, r=1/input$r3, theta=input$theta3, phi=input$phi3, ell=input$ell3, m=input$m3)|>
      mutate(S=SU+SP,
             I=IU+IP, 
             R=RU+RP,
             D=DU+DP)|>
      mutate(N=S+I+R+D)
    SIR_analyze <- function(df) { 
      herd_immunity_threshold <- df |>
        mutate(cases = I + R) |>
        filter(time == which.max(df$I)) |>
        select(cases) |>
        as.numeric()  
      cumulative <- df |>
        mutate(cases = I + R + D) |>
        tail(1) |>
        select(cases) |>
        as.numeric()  
      PII <- max(df$I)
      when_peak <- which.max(df$I)
      
      cum_deaths <- df |>
        select(D)|>
        tail(1)
      as.numeric()
      
      return(list(HIT = herd_immunity_threshold,
                  CI = cumulative,
                  PII = PII,
                  Peak = when_peak,
                  CD = cum_deaths))
    }
    SIRvalues3<-SIR_analyze(df)
    
    
    string_cuminf <- paste("Cumulative infection incidence:", signif(SIRvalues3$CI, digits = 3))
    string_peakinf <- paste("Peak infection prevalence:", signif(SIRvalues3$PII, digits = 3))
    #paste(" total number of deaths are", signif(SIRvalues3$D, digits = 3))
    string_whenpeak <- paste("Infection prevalence peak on day:", signif(SIRvalues3$Peak, digits = 3))
    string_cumdth <- paste("Cumulative mortality:", signif(SIRvalues3$CD, digits = 3))
    HTML(paste("<br/><br/><br/>",string_cuminf, string_peakinf, string_whenpeak, sep = '<br/>'))
    
  })  
  
  
  
  #}
  output$explain_groups <- renderUI({
    withMathJax(HTML(markdown::markdownToHTML(knit("groups.Rmd", quiet=TRUE))))
  })
  
  
  output$SIRPlot_groups <- renderPlot({
    library(deSolve)
    library(tidyverse)
    library(reshape2)
    #does this name need to be changed?
    SIR_groups_run <- function(h=.99, epsilon=.5, mu=.05, kappa=.1, beta=.5, rho=.1, 
                               theta=5, phi=.1, 
                               ell=1, I0_a=.005/2, I0_b=.005/2, time=500){
      
      model <- function(t, init, parms){
        
        with(as.list(c(init, parms)), {
          
          if(t<(ell+1)){
            lag<-rep(0, 16)
          }
          else{
            lag<-lagvalue(t-ell)
          }
          
          dSUa <- -sqrt(beta)*SUa*(sqrt(beta)*h*(IUa+kappa*IPa)+sqrt(beta)*(1-h)*(IUb+kappa*IPb)) - 
            theta * SUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * SPa  
          
          dSPa <- -sqrt(beta)*kappa*SPa*(sqrt(beta)*h*(IUa+kappa*IPa)+sqrt(beta)*(1-h)*(IUb+kappa*IPb)) + 
            theta * SUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16]) ) - phi * SPa 
          
          dIUa <- sqrt(beta)*SUa*(sqrt(beta)*h*(IUa+kappa*IPa)+sqrt(beta)*(1-h)*(IUb+kappa*IPb)) - 
            theta * IUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * IPa -
            (rho) * IUa
          
          dIPa <- sqrt(beta)*kappa*SPa*(sqrt(beta)*h*(IUa+kappa*IPa)+sqrt(beta)*(1-h)*(IUb+kappa*IPb)) + 
            theta * IUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16])  ) - phi * IPa -
            (rho) * IPa
          
          dRUa <- rho * (1-mu) * IUa - 
            theta * RUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * RPa  
          
          dRPa <- rho * (1-mu) * IPa + 
            theta * RUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16])  ) - phi * RPa  
          
          dDUa <- rho*mu*IUa
          
          dDPa <- rho*mu*IPa
          
          #transitions for group b
          
          dSUb <- -sqrt(beta)*SUb*(sqrt(beta)*(1-h)*(IUa+kappa*IPa)+sqrt(beta)*(h)*(IUb+kappa*IPb)) - 
            theta * SUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * SPb
          
          dSPb <- -sqrt(beta)*kappa*SPb*(sqrt(beta)*(1-h)*(IUa+kappa*IPa)+sqrt(beta)*(h)*(IUb+kappa*IPb)) + 
            theta * SUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16])  ) - phi * SPb 
          
          dIUb <- sqrt(beta)*SUb*(sqrt(beta)*(1-h)*(IUa+kappa*IPa)+sqrt(beta)*(h)*(IUb+kappa*IPb)) - 
            theta * IUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16])  ) + phi * IPb -
            (rho) * IUb
          
          dIPb <- sqrt(beta)*kappa*SPb*(sqrt(beta)*(1-h)*(IUa+kappa*IPa)+sqrt(beta)*(h)*(IUb+kappa*IPb)) +
            theta * IUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16]) ) - phi * IPb -
            (rho) * IPb
          
          dRUb <- (1-mu) * rho * IUb - 
            theta * RUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * RPb
          
          dRPb <- (1-mu) * rho * IPb + 
            theta * RUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16])  ) - phi * RPb
          
          dDUb <- rho*mu*IUb
          
          dDPb <- rho*mu*IPb
          
          
          list(c(dSUa, dSPa, dIUa, dIPa, dRUa, dRPa, dDUa, dDPa,
                 dSUb, dSPb, dIUb, dIPb, dRUb, dRPb, dDUb, dDPb))
          
        })
      }
      parms<-c("kappa"=kappa,  "theta"=theta,  "beta"=beta, "rho"=rho, "phi"=phi, 
               "mu"=mu, "epsilon"=epsilon, "h"=h, "ell"=ell, "time"=time, "I0_a"=I0_a, "I0_b"=I0_b) 
      init<-c( 
        
        #group a
        SUa=1-I0_a,
        SPa=0,
        IUa=I0_a,
        IPa=0,
        RUa=0,
        RPa=0,
        DUa=0,
        DPa=0,
        
        
        #group b
        SUb=1-I0_b,
        SPb=0,
        IUb=I0_b,
        IPb=0,
        RUb=0,
        RPb=0,
        DUb=0,
        DPb=0
      )
      
      time <- seq(1, time, 1)
      sir_df <- dede(y=init, times=time, func=model,parms=parms)
      return(data.frame(sir_df))
    }
    
    
    df <- SIR_groups_run(time=input$t4, k=input$kappa4, b=input$b4, r=1/input$r4, theta=input$theta4, phi=input$phi4, ell=input$ell4, mu=input$mu4, epsilon=input$epsilon4, h=input$h4, I0_a=input$I0_a4, I0_b=input$I0_b4)%>%
      mutate(Ia=IUa+IPa,
             Ib=IUb+IPb,
             I=IUa+IPa+IUb+IPb,
             Ra=RUa+RPa,
             Rb=RUb+RPb,
             R=RUa+RPa+RUb+RPb,
             Da=DUa+DPa,
             Db=DUb+DPb,
             D=DUa+DPa+DUb+DPb)
    
    
    
    ggplot() +
      geom_line(data = data.frame(df), size = 1.5, alpha =0.9, aes(x = time, y = Ia, color="group *a*"))+
      geom_line(data = data.frame(df), size = .8, alpha =0.9, aes(x = time, y = Ib,  color="group *b*"))+
      scale_colour_manual(name="", 
                          labels = c(expression("group"~italic(a)), expression("group"~italic(b))),
                          values = c("group *a*"="#f4a896", "group *b*"="#358597"))+
      labs(title = "",
           x="Time",
           y="Infections")+
      theme_fivethirtyeight()+
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            axis.title = element_text())
    
    
  })
  
  output$text_groups <- renderUI({
    library(ggthemes)
    library(magrittr)
    library(dplyr)
    library(deSolve)
    library(ggplot2)
    library(extrafont)
    SIR_groups_run <- function(h=.99, epsilon=.5, mu=.05, kappa=.1, beta=.5, rho=.1, 
                               theta=5, phi=.1, 
                               ell=1, I0_a=.005/2, I0_b=.005/2, time=500){
      
      model <- function(t, init, parms){
        
        with(as.list(c(init, parms)), {
          
          if(t<(ell+1)){
            lag<-rep(0, 16)
          }
          else{
            lag<-lagvalue(t-ell)
          }
          
          dSUa <- -sqrt(beta)*SUa*(sqrt(beta)*h*(IUa+kappa*IPa)+sqrt(beta)*(1-h)*(IUb+kappa*IPb)) - 
            theta * SUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * SPa  
          
          dSPa <- -sqrt(beta)*kappa*SPa*(sqrt(beta)*h*(IUa+kappa*IPa)+sqrt(beta)*(1-h)*(IUb+kappa*IPb)) + 
            theta * SUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16]) ) - phi * SPa 
          
          dIUa <- sqrt(beta)*SUa*(sqrt(beta)*h*(IUa+kappa*IPa)+sqrt(beta)*(1-h)*(IUb+kappa*IPb)) - 
            theta * IUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * IPa -
            (rho) * IUa
          
          dIPa <- sqrt(beta)*kappa*SPa*(sqrt(beta)*h*(IUa+kappa*IPa)+sqrt(beta)*(1-h)*(IUb+kappa*IPb)) + 
            theta * IUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16])  ) - phi * IPa -
            (rho) * IPa
          
          dRUa <- rho * (1-mu) * IUa - 
            theta * RUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * RPa  
          
          dRPa <- rho * (1-mu) * IPa + 
            theta * RUa * (epsilon * (DUa+DPa-lag[7]-lag[8]) + (1-epsilon) * (DUb+DPb-lag[15]-lag[16])  ) - phi * RPa  
          
          dDUa <- rho*mu*IUa
          
          dDPa <- rho*mu*IPa
          
          #transitions for group b
          
          dSUb <- -sqrt(beta)*SUb*(sqrt(beta)*(1-h)*(IUa+kappa*IPa)+sqrt(beta)*(h)*(IUb+kappa*IPb)) - 
            theta * SUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * SPb
          
          dSPb <- -sqrt(beta)*kappa*SPb*(sqrt(beta)*(1-h)*(IUa+kappa*IPa)+sqrt(beta)*(h)*(IUb+kappa*IPb)) + 
            theta * SUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16])  ) - phi * SPb 
          
          dIUb <- sqrt(beta)*SUb*(sqrt(beta)*(1-h)*(IUa+kappa*IPa)+sqrt(beta)*(h)*(IUb+kappa*IPb)) - 
            theta * IUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16])  ) + phi * IPb -
            (rho) * IUb
          
          dIPb <- sqrt(beta)*kappa*SPb*(sqrt(beta)*(1-h)*(IUa+kappa*IPa)+sqrt(beta)*(h)*(IUb+kappa*IPb)) +
            theta * IUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16]) ) - phi * IPb -
            (rho) * IPb
          
          dRUb <- (1-mu) * rho * IUb - 
            theta * RUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16]) ) + phi * RPb
          
          dRPb <- (1-mu) * rho * IPb + 
            theta * RUb * ((1-epsilon) * (DUa+DPa-lag[7]-lag[8]) + (epsilon) * (DUb+DPb-lag[15]-lag[16])  ) - phi * RPb
          
          dDUb <- rho*mu*IUb
          
          dDPb <- rho*mu*IPb
          
          
          list(c(dSUa, dSPa, dIUa, dIPa, dRUa, dRPa, dDUa, dDPa,
                 dSUb, dSPb, dIUb, dIPb, dRUb, dRPb, dDUb, dDPb))
          
        })
      }
      parms<-c("kappa"=kappa,  "theta"=theta,  "beta"=beta, "rho"=rho, "phi"=phi, 
               "mu"=mu, "epsilon"=epsilon, "h"=h, "ell"=ell, "time"=time, "I0_a"=I0_a, "I0_b"=I0_b) 
      init<-c( 
        
        #group a
        SUa=1-I0_a,
        SPa=0,
        IUa=I0_a,
        IPa=0,
        RUa=0,
        RPa=0,
        DUa=0,
        DPa=0,
        
        
        #group b
        SUb=1-I0_b,
        SPb=0,
        IUb=I0_b,
        IPb=0,
        RUb=0,
        RPb=0,
        DUb=0,
        DPb=0
      )
      
      time <- seq(1, time, 1)
      sir_df <- dede(y=init, times=time, func=model,parms=parms)
      return(data.frame(sir_df))
    }
    #where am i piping func into a mutate
    
    df <- SIR_groups_run(time=input$t4, k=input$kappa4, b=input$b4, r=1/input$r4, theta=input$theta4, phi=input$phi4, ell=input$ell4, mu=input$mu4, epsilon=input$epsilon4, h=input$h4, I0_a=input$I0_a4, I0_b=input$I0_b4)|>
      mutate(Ia=IUa+IPa,
             Ib=IUb+IPb,
             I=IUa+IPa+IUb+IPb,
             Ra=RUa+RPa,
             Rb=RUb+RPb,
             R=RUa+RPa+RUb+RPb,
             Da=DUa+DPa,
             Db=DUb+DPb,
             D=DUa+DPa+DUb+DPb)
    
    SIR_analyze <- function(df) { 
      herd_immunity_threshold_a <- df |>
        mutate(cases = Ia + Ra) |>
        filter(time == which.max(df$Ia)) |>
        select(cases) |>
        as.numeric()  
      cumulative_a <- df |>
        mutate(cases = Ia + Ra + Da) |>
        tail(1) |>
        select(cases) |>
        as.numeric()  
      PII_a <- max(df$Ia)
      when_peak_a <- which.max(df$Ia)
      cum_deaths_a <- df |>
        select(Da)|>
        tail(1)
      as.numeric()
      herd_immunity_threshold_b <- df |>
        mutate(cases = Ib + Rb) |>
        filter(time == which.max(df$Ib)) |>
        select(cases) |>
        as.numeric()  
      cumulative_b <- df |>
        mutate(cases = Ib + Rb + Db) |>
        tail(1) |>
        select(cases) |>
        as.numeric()  
      PII_b <- max(df$Ib)
      when_peak_b <- which.max(df$Ib)
      cum_deaths_b <- df |>
        select(D)|>
        tail(1)
      as.numeric()
      herd_immunity_threshold <- df |>
        mutate(cases = I + R) |>
        filter(time == which.max(df$I)) |>
        select(cases) |>
        as.numeric()  
      cumulative <- df |>
        mutate(cases = I + R + D) |>
        tail(1) |>
        select(cases) |>
        as.numeric()  
      PII <- max(df$I)
      when_peak <- which.max(df$I)
      cum_deaths <- df |>
        select(D)|>
        tail(1)
      as.numeric()
      
      return(list(HIT = herd_immunity_threshold/2,
                  HITa = herd_immunity_threshold_a,
                  HITb = herd_immunity_threshold_b,
                  CI = cumulative/2,
                  CI_a = cumulative_a,
                  CI_b = cumulative_b,
                  PII = PII/2,
                  PII_a = PII_a,
                  PII_b = PII_b,
                  Peak = when_peak,
                  Peak_a = when_peak_a,
                  Peak_b = when_peak_b,
                  CD = cum_deaths, 
                  CD_a = cum_deaths_a, 
                  CD_b = cum_deaths_b
      ))
    }       
    
    SIRvalues4 <- SIR_analyze(df)
    
    
    string_cuminfa <- paste("Cumulative infection incidence in group a:", signif(SIRvalues4$CI_a, digits = 3))
    
    string_cuminfb <- paste("Cumulative infection incidence in group b:", signif(SIRvalues4$CI_b, digits = 3))
    
    string_cuminf <- paste("Cumulative infection incidence in full population:", signif(SIRvalues4$CI, digits = 3))
    
    string_peakinfa <- paste("Peak infection prevalence in group a:", signif(SIRvalues4$PII_a, digits = 3))
    #paste(" total number of deaths are", signif(SIRvalues3$D, digits = 3))
    
    string_peakinfb <- paste("Peak infection prevalence in group b:", signif(SIRvalues4$PII_b, digits = 3))
    #paste(" total number of deaths are", signif(SIRvalues3$D, digits = 3))
    
    string_peakinf <- paste("Peak infection prevalence in full population:", signif(SIRvalues4$PII, digits = 3))
    #paste(" total number of deaths are", signif(SIRvalues3$D, digits = 3)) 
    
    string_whenpeaka <- paste("Infection prevalence peak in group a on day:", signif(SIRvalues4$Peak_a, digits = 3))
    
    string_whenpeakb <- paste("Infection prevalence peak in group b on day:", signif(SIRvalues4$Peak_b, digits = 3))
    
    string_whenpeak <- paste("Infection prevalence peak in full population on day:", signif(SIRvalues4$Peak, digits = 3))
    
    string_cumdeathsa <- paste("Cumulative mortality in group a:", signif(SIRvalues4$CD_a, digits = 3))
    
    string_cumdeathsb <- paste("Cumulative mortality in group b:", signif(SIRvalues4$CD_b, digits = 3))
    
    string_cumdeaths <- paste("Cumulative mortality in full population:", signif(SIRvalues4$CD, digits = 3))
    
    HTML(paste(string_cuminfa, string_cuminfb, string_cuminf, string_peakinfa, string_peakinfb, 
               string_peakinf, string_whenpeaka, string_whenpeakb, string_whenpeak, 
               string_cumdeathsa, string_cumdeathsb, string_cumdeaths, sep = '<br/>'))
    
  }) 
  output$Credits <- renderUI({
    withMathJax(HTML(markdown::markdownToHTML(knit("Credits.Rmd", quiet=TRUE))))
  })
  output$Overview <- renderUI({
    withMathJax(HTML(markdown::markdownToHTML(knit("overview.Rmd", quiet=TRUE))))
  })
}




shinyApp(ui=ui, server =server)    



