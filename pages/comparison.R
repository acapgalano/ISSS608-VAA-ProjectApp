comparisonTab <- function(id){
  
  ns <- NS(id)
  
  options(spinner.color="#f07167", 
            spinner.color.background="#ffffff", 
            spinner.size=2)
  
  tabPanel("Comparison",
           tabName = 'comparison_page',
           icon = icon('scale-balanced'),
           value = 'comparison_page',
          
           tabsetPanel(type = 'tabs',
                       id = ns("comparison_tabset"),
                       tabPanel("Introduction",
                                value = 'comparison_intro_tab',
                                icon = icon('circle-info'),
                                style = 'background-color: #ffffff;',
                                fluidRow(
                                  column(6,
                                         style = "padding: 20px 10px 10px 30px;",
                                         h2("What is ANOVA?"),
                                         p("ANOVA, which stands for Analysis of Variance, 
                                    is a statistical test used to analyze the difference 
                                    between the means of more than two groups. One may use 
                                           ANOVA to test a particular hypothesis. You would 
                                           use ANOVA to help you understand how different groups 
                                           respond, with a null hypothesis stating eqaulity, or no difference. 
                                           If there is a significant result, then we reject the hypothesis
                                           and conclude that there is a significant difference."),
                                         h2("Why do I need to use this?"),
                                         p("You may want to explore ANOVA from both the perspective of a developer and 
                                    an investor. In our app, we use this statistical analysis to compare game development related metrics
                                    and to see if there are significant differences. In particular, we provide the comparison between game genres and
                                    game developers. This will further be elaborated on in the other tabs.")
                                         ),
                                  column(6,
                                         style = "padding: 10px 20px 10px 10px;",
                                         img(id = 'anova',
                                             src = 'img/anova-diagram.svg',
                                             style = 'width: 100%')),
                                )
                         
                       ),
                       # For Developers Tab
                       tabPanel("For Developers",
                                value = 'comparison_developer_tab', 
                                icon = icon('code'),
                                style = 'background-color: #ffffff; padding-bottom: 20px;',
                                fluidRow(
                                  style = 'padding-left: 30px; padding-right: 30px',
                                  h3("Compare the Performance of Game Genres",
                                     style = 'color: #0081a7;'),
                                  p("In this tab, we take a look at the difference in the performance of two different
                                    game genres in terms of player count, rating, and price. This information
                                    aims to help game developers who are in the planning phase
                                    and are considering between different kinds of games to develop. By
                                    seeing the performance difference, they may come to a decision. This is also
                                    targeted towards Steam employees who need to greenlight games. If they are
                                    deciding between two different types of games to greenlight, this can
                                    help them come to a resolution as to what will sell better or have a more
                                    positive reception.")
                                ),
                                sidebarLayout(mainPanel(
                                  div(style = "background-color: #0081a7; margin-left:15px;",
                                    withSpinner(plotOutput(ns("plot1")), type = 5)
                                  ),
                                  p(""),
                                  p(""),
                                  useShinyjs(),
                                  actionButton(ns("test1"),label = "What does this mean?",
                                               style = 'margin-left: 20px; background-color:#f07167; color:#fdfcdc;'),
                                                div(id=ns("div_c"),
                                                    style = 'background-color:#fed9b7; padding-left:30px;',
                                                    box(background = NULL,
                                                     status = "primary",
                                                     solidHeader = TRUE,
                                                     width = 20,
                                                     textOutput(ns("ANOVA_TEST1"))))
                                ),
                                sidebarPanel(style = 'margin-right: 15px;',
                                            sliderInput(ns("Year"),
                                                         "Select a range of year:",
                                                         value = c(2010,2020),
                                                         min = min(steam$Year), 
                                                         max = max(steam$Year),
                                                         sep = ""),
                                                        
                                             selectInput(ns("genretp1"),"What genres game1 has?", genretype, multiple = TRUE),
                                             selectInput(ns("genretp2"),"What genres game2 has?", genretype, multiple = TRUE),
                                             radioButtons(ns("ylabel"),"Choose a Y variable to compare:",
                                                          choiceNames = list("Popularity",
                                                                             "Player Count",
                                                                             "Price"),
                                                          choiceValues = list("Popular",
                                                                              "Active",
                                                                              "Price"
                                                          )
                                             ),
                                             actionButton(ns("run1"),"Run!",icon = icon('play'),class = "btn-primary")
                                ),
                                
                                )
                       ),
                       tabPanel("For Investors",
                                value = 'comparison_investor_tab' , 
                                icon = icon("comment-dollar"),
                                style = 'background-color: #ffffff; padding-bottom: 20px;',
                                fluidRow(
                                  style = 'padding-left: 30px; padding-right: 30px',
                                  h3("Compare the Performance of Game Developers",
                                     style = 'color: #0081a7;'),
                                  p("Do you wanna get into the game industry? Well this provides a way to objectively
                                    determine which game developer is doing better than the rest. ")
                                ),
                                sidebarLayout(mainPanel(
                                  div(style = "background-color: #0081a7; margin-left:15px;",
                                      withSpinner(plotOutput(ns("plot2")),type = 5)
                                  ),
                                  p(""),
                                  p(""),
                                  useShinyjs(),
                                  actionButton(ns("test2"),
                                               style = 'margin-left: 20px; background-color:#f07167; color:#fdfcdc;',
                                               label = "What does this mean?"),
                                  div(id=ns("div_b"),
                                      style = 'background-color:#fed9b7;',
                                      box(background = NULL,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 20,
                                          textOutput(ns("ANOVA_TEST2"))
                                      )
                                  )
                                ),
                                sidebarPanel(
                                  style = 'margin-right: 15px;',
                                  sliderInput(ns("Year2"),
                                              "Select a range of year:",
                                              value = c(2010,2020),
                                              min = min(steam$Year), 
                                              max = max(steam$Year),
                                              sep = ""
                                  ),
                                  selectInput(ns("dev1"),
                                              "Which developers you want choose to compare:",
                                              inv$by, 
                                              multiple = TRUE
                                  ),
                                  radioButtons(ns("y"),
                                               "Choose a Y variable to compare:",
                                               choiceNames = list("Revenue",
                                                                  "Player Count"
                                               ),
                                               choiceValues = list("Revenue",
                                                                   "Active"
                                               )
                                  ),
                                  actionButton(ns("run2"),"Run!",icon = icon('play'),class = "btn-primary")
                                )
                                )
                       )
           )
           
  )
}


comparisonServer <- function(id, input, output, session) {
  moduleServer(id,
               function(input, output, session) {
                 
                  observe({
                    if(input$comparison_tabset == 'comparison_developer_tab'){
                      hide(id = 'div_b')
                    }
                    if(input$comparison_tabset == 'comparison_investor_tab'){
                      hide(id = "div_c")
                    }
                  })
                  
                  
                  ## For Developers
                  l1<-reactive({
                    input$run1
                    l1<-c()
                    for (j in 1:nrow(steam)){
                      l1<-append(l1,j)
                      for (i in input$genretp1){
                        if(!(i%in%steam$genre[[j]])){
                          l1<-l1[-length(l1)]
                          break
                        }
                      }}
                    return(l1)
                  })
                  l2<-reactive({
                    input$run1
                    l2<-c()
                    for (j in 1:nrow(steam)){
                      l2<-append(l2,j)
                      for (i in input$genretp2){
                        if(!(i%in%steam$genre[[j]])){
                          l2<-l2[-length(l2)]
                          break
                        }
                      }}
                    return(l2)
                  })
                  POP<-reactive({
                    input$run1
                    dat1<-steam[l1(),]%>%
                      select("price","popular","average_2weeks","Year")
                    
                    
                    dat2<-steam[l2(),]%>%
                      select("price","popular","average_2weeks","Year")
                    ## create df for plot
                    Genre<-c(rep("Game1",nrow(dat1)),rep("Game2",nrow(dat2)))
                    Popular<-c(dat1$popular,dat2$popular)
                    Active<-c(dat1$average_2weeks,dat2$average_2weeks)
                    Year<-c(dat1$Year,dat2$Year)
                    Price<-c(dat1$price,dat2$price)
                    
                    return(data.frame(Genre,Popular,Active,Year,Price))
                  })
                  
                  p1<-reactive({
                    input$run1
                    if (input$ylabel=="Popular"){
                      res.aov<-aov( Popular ~ Genre, data=POP())
                      stat<-summary(res.aov)
                      return(stat[[1]][5][1,])}
                    else{
                      if (input$ylabel=="Active Users"){
                        res.aov<-aov( Active ~ Genre, data=POP())
                        stat<-summary(res.aov)
                        return(stat[[1]][5][1,])}
                      else{
                        res.aov<-aov(Price ~ Genre, data=POP())
                        stat<-summary(res.aov)
                        return(stat[[1]][5][1,])
                      }}
                    
                  })
                  
                  
                  
                  ## anovaplot
                  observeEvent(
                    input$run1,{
                      output$plot1<-renderPlot({
                        if (input$comparison_tabset != 'comparison_developer_tab'){
                          return()
                        }
                        Sys.sleep(1)
                        input$run1
                        ggbetweenstats(
                          POP()[!!input$Year%in%POP()$Year==1,],
                          x = Genre,
                          y = !!input$ylabel,
                          type = "np",
                          messages = FALSE)
                      }) 
                    })
                  
                  ############################################## Investor tab
                  
                  INV<-reactive({
                    input$run2
                    Developer<-c()
                    Active<-c()
                    Revenue<-c()
                    Year<-c()
                    for (i in input$dev1){
                      dat<-invest[invest$developer==i,]%>%
                        select("revenue","average_2weeks","Year")
                      Developer<-append(Developer,rep(i,nrow(dat)))
                      Active<-append(Active,dat$average_2weeks)
                      Revenue<-append(Revenue,dat$revenue)
                      Year<-append(Year,dat$Year)
                    }
                    return(data.frame(Developer,Active,Revenue,Year))
                  })
                  
                  observeEvent(
                    input$run2,{
                      output$plot2<-renderPlot({
                        if (input$comparison_tabset != 'comparison_investor_tab') return()
                        Sys.sleep(1)
                        input$run2
                        ggbetweenstats(
                          INV()[!!input$Year%in%INV()$Year==1,],
                          x = Developer,
                          y = !!input$y,
                          type = "np",
                          messages = FALSE)
                      })
                    })
                  
                  
                  
                  p2<-reactive({
                    input$run2
                    if (input$y=="Game Active"){
                      res.aov<-aov( Active ~ Developer, data=INV())
                      summary(res.aov)
                      stat<-summary(res.aov)
                      return(stat[[1]][5][1,])}
                    else{
                      res.aov<-aov( Revenue ~ Developer, data=INV())
                      stat<-summary(res.aov)
                      return(stat[[1]][5][1,])
                    }
                    
                  })
                  
                  observeEvent(input$test2,{
                    toggle(id = "div_b",anim = T)
                    
                    output$ANOVA_TEST2<-renderText({
                      
                      if(p2()>=0.05){
                        paste0("The p-value of ANOVA test is",round(p2(),3),
                               ". Because the p-value is less than 0.05, we have enough confidence to believe 
                       that mean values of",input$y,"between genres of games are different from each other.")
                      }
                      else{
                        paste0("The p-value of ANOVA test is ",round(p2(),3),
                               ". Because the p-value is more than 0.05, we don't have enough confidence to reject the H0:The 
                         mean values of ",input$y," between genres of games are different from each other. Therefore, 
                         there is 95% confidence that the mean values of target variable are the same.")
                      }
                      
                    })
                    
                  })
                  hide(id = "div_c")
                  observeEvent(input$test1,{
                    toggle(id = "div_c",anim = T)
                    
                    output$ANOVA_TEST1<-renderText({
                      
                      if(p1()>=0.05){
                        paste0("The p-value of ANOVA test is ",round(p1(),3),
                               ". Because the p-value is less than 0.05, we have enough confidence to believe 
                       that mean values of ",input$ylabel," between genres of games are different from each other.")
                      }
                      else{
                        paste0("The p-value of ANOVA test is ",round(p1(),3),
                               ". Because the p-value is more than 0.05, we don't have enough confidence to reject the H0:The 
                         mean values of ",input$ylabel," between genres of games are different from each other. Therefore, 
                         there is 95% confidence that the mean values of target variable are the same.")
                      }
                      
                    })
                    
                  })
                  
                }
  )
}