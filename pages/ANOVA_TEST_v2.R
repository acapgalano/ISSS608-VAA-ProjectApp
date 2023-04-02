pacman::p_load(shiny, shinydashboard, tidyr, tidyverse, rlist, plotly,gganimate, ggthemes, gifski, 
               gapminder, reshape2, pracma, ggstatsplot, bslib, ggpubr,ggvis,shinyjs)


## Develop_tab
steam<-read_csv("../data/steam_join.csv")
steam<-steam
## Genre types
genretype<-list()
for (i in 1:length(steam$genre)){
  for (j in steam$genre[[i]]){
    if (!(j %in% genretype)){
      genretype<-list.append(genretype,j)
      
    }
  }
} 

##For investors
invest<-read_csv("../data/invest.csv")
inv<-steam%>%
  group_by(by=developer)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  filter(n>=25)



ui <- dashboardPage(
  dashboardHeader(title = "Part2. Analysis dashboard"),
  
  ## Side bar
  dashboardSidebar(
    sidebarMenu(
      menuItem("ANOVA", tabName = "ANOVA", icon = icon("dashboard"))
      )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "ANOVA",
              style = "background-color:#DBCEBA;",
        fluidPage(
          fluidRow(
            column(12,
                   p(""),
                   useShinyjs(),
                   actionButton(inputId = "info",label = "Why ANOVA test?",
                                style = "background-color:#B2C9D4;"),
                   div(id="div_a",box(paste("ANOVA, which stands for Analysis of Variance, 
                                    is a statistical test used to analyze the difference 
                                    between the means of more than two groups.","In our App, 
                                    you may want to explore from both a developer's view and 
                                    an investor's view about how a game or a developer performances. 
                                    As for a developer, you may want to explore if the genre of a game 
                                    will put an impact on the popularity, user activity and price; on 
                                    the other hand, as an investor, you may want to explore the revenue 
                                    and user activity of games from a developer, before you make a decision.",
                                            sep = "\n"),
                                      width = 20)),
                   p(""),
            tabsetPanel(
                tabPanel("For Developers",
                         icon = icon("business-time"),
                     sidebarLayout(
                       mainPanel(
                         plotOutput("plot1"),
                         p(""),
                         p(""),
                         useShinyjs(),
                         actionButton(inputId = "test1",label = "ANOVA Test Result"),
                         div(id="div_c",box(background = NULL,
                                            status = "primary",
                                            solidHeader = TRUE,
                                            width = 20,
                                            textOutput("ANOVA_TEST1")))
                         ),
                       sidebarPanel(
                         sliderInput("Year","Select a range of year:",value = c(2010,2020),min = min(steam$Year), max = max(steam$Year)),
                         selectInput("genretp1","What genres game1 has?",genretype, multiple = TRUE),
                         selectInput("genretp2","What genres game2 has?",genretype, multiple = TRUE),
                         radioButtons("ylabel","Choose a Y variable to compare:",
                                      choiceNames = list("Popular","Active Users","Price"),
                                      choiceValues = list(
                                        "Popular",
                                        "Active",
                                        "Price"
                                      )),
                         actionButton("run1","Run!",icon = icon("cocktail"),class = "btn-primary")
                       ))
                       ),
                tabPanel("For Investors",
                         icon = icon("comment-dollar"),
                     sidebarLayout(
                       mainPanel(
                         plotOutput("plot2"),
                         p(""),
                         p(""),
                         useShinyjs(),
                         actionButton(inputId = "test2",label = "ANOVA Test Result"),
                       div(id="div_b",box(background = NULL,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          width = 20,
                                          textOutput("ANOVA_TEST2")))
                         ),
                       sidebarPanel(
                         sliderInput("Year2","Select a range of year:",value = c(2010,2020),min = min(steam$Year), max = max(steam$Year)),
                         selectInput("dev1","Which developers you want choose to compare:",inv$by, multiple = TRUE),
                         radioButtons("y","Choose a Y variable to compare:",
                                      choiceNames = list(
                                        "Revenue",
                                        "Game Active"
                                      ),
                                      choiceValues = list(
                                        "Revenue",
                                        "Active"
                                      )),
                         actionButton("run2","Run!",icon = icon('play'),class = "btn-primary")
                       )
                     )
                     )
                )
              
            )),
          fluidRow()
            )
          ))))
  
  


server <- function(input, output,session) {
  hide(id = "div_a")
  observeEvent(input$info,{
    toggle(id = "div_a",anim = T)
  })
  hide(id = "div_b")
  observeEvent(input$test2,{
    toggle(id = "div_b",anim = T)
  })
  hide(id = "div_c")
  observeEvent(input$test1,{
    toggle(id = "div_c",anim = T)
  })
  
  
  ## for developers
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
  
  
  ## anovaplot
  observeEvent(
    input$run1,{
      output$plot1<-renderPlot({
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
}

shinyApp(ui, server)
