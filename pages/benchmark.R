

benchmarkTab <- function(id){
  
  ns <- NS(id)
  
  tabPanel("Benchmark", 
           tabName = "benchmark", 
           icon = icon("trophy"), 
           value = "benchmark",
           fluidPage(
             # Enter game infortmation 
             div(style = 'background-color: #fed9b7; padding: 15px;',
              fluidRow(
                align = 'center',
                style = 'padding: 0 10px 0 10px;',
                h2('Welcome to the Game Benchmark Calculator!',
                   style = 'color: #f07167; align: center;'),
                p("You can input the details of the game you wish to develop and we'll generate benchmark
                  values based on games that have similar characteristics to yours!")
              ),
               fluidRow(
                 column(4,
                   align = 'center',
                   textInput(ns('benchmark_title'),
                             "What's your game's name?")
                 ),
                 column(4,
                        align = 'center',
                        selectInput(ns('benchmark_type'),
                                    "Type", 
                                    type_list,
                                    multiple = TRUE),
                 ),
                 column(4,
                        align = 'center',
                        selectInput(ns('benchmark_genre'),
                                    "Game genre/s", 
                                    genre_list,
                                    multiple = TRUE),
                 )
               ),
               fluidRow(
                 column(2,
                        align = 'center',
                        radioButtons(ns('benchmark_multiplayer'),
                                     "Is it Multiplayer?",
                                     choices = c("Yes" = "Yes",
                                                 "No" = "No"),
                                     inline = TRUE),
                 ),
                 column(3,
                        align = 'center',
                        radioButtons(ns('benchmark_perspective'), 
                                     "First person or Third person?",
                                     choiceNames = list("First-person",
                                                        "Third-person"),
                                     choiceValues = list("First-Person",
                                                         "Third-Person"),
                                     inline = TRUE)
                   
                 ),
                 column(2,
                        align = 'center',
                        radioButtons(ns('benchmark_ftp'),
                                     "Is it Free To Play?",
                                     choices = c("Yes" = "Yes",
                                                 "No" = "No"),
                                     inline = TRUE),
                 ),
                 column(5,
                        align = 'center',
                        sliderInput(ns('benchmark_price'),
                                    "Possible price range:",
                                    value = c(0,499),
                                    min = 0, 
                                    max = 4999)
                 )
               ),
               fluidRow(
                 column(12, style = 'display: grid;',
                        actionButton(ns('benchmark_run'),
                                     " See Benchmarks",
                                     align = 'center',
                                     style = 'background-color: #f07167; color: #fdfcdc; padding:20px; font-size:120%',
                                     icon = icon('bullseye')
                                     )
                        )
                 
               )
             ),
             # Generate benchmark graphs 

             hr(style = "border-top: 3px solid #fed9b7;"),
              
             fluidRow(align = "center",
                textOutput(ns('benchmark_header')) 
             ),
             # FOR TYPE
             fluidRow(
               column(8,
                      plotlyOutput(ns('benchmark_type_plot')),
                      useShinyjs()
               ),
               column(4,
                      valueBoxOutput(ns('benchmark_type_rating')),
                      valueBoxOutput(ns('benchmark_type_users'))
             )
           ),
           hr(style = "border-top: 1px solid #fed9b7;"),
           # FOR GENRE
           fluidRow(
             column(8,
                    plotlyOutput(ns('benchmark_genre_plot')),
                    useShinyjs()
             ),
             column(4,
                    valueBoxOutput(ns('benchmark_genre_rating')),
                    valueBoxOutput(ns('benchmark_genre_users'))
             )
           ),
           hr(style = "border-top: 1px solid #fed9b7;"),
           # FOR MULTIPLAYER
           fluidRow(
             column(8,
                    plotlyOutput(ns('benchmark_multiplayer_plot')),
                    useShinyjs()
             ),
             column(4,
                    valueBoxOutput(ns('benchmark_multiplayer_rating')),
                    valueBoxOutput(ns('benchmark_multiplayer_users'))
             )
           ),
           
           hr(style = "border-top: 1px solid #fed9b7;"),
           # FOR PERSPECTIVE
           fluidRow(
             column(8,
                    plotlyOutput(ns('benchmark_perspective_plot')),
                    useShinyjs()
             ),
             column(4,
                    valueBoxOutput(ns('benchmark_perspective_rating')),
                    valueBoxOutput(ns('benchmark_perspective_users'))
             )
           ),
           
           hr(style = "border-top: 1px solid #fed9b7;"),
           
           # FOR PRICE 
           fluidRow(
             column(8,
                    plotlyOutput(ns('benchmark_price_plot')),
                    useShinyjs()
             ),
             column(4,
                    valueBoxOutput(ns('benchmark_price_rating')),
                    valueBoxOutput(ns('benchmark_price_users'))
             )
           ),
           
       )           
  )
}

benchmarkServer <- function(id, input, output, session) {
  moduleServer(id,
               function(input, output, session) {
                 
             observe(
               {
                 if(!is.na(input$benchmark_perspective) && !is.na(input$benchmark_multiplayer) &&!is.na(input$benchmark_ftp))
                 {
                   enable('benchmark_run')
                 }
                 else
                 {
                   disable('benchmark_run')
                 }
               })
                 
               # IF FTP, CHANGE PRICE RANGE
               observe({
                 if(input$benchmark_ftp == 'Yes'){
                   updateSliderInput(session, 'benchmark_price', value = c(0,0))
                 }
               })
                
               # Filter TYPE Dataframe 
                
                benchmark_df_type <- reactive({
                  benchmark_table %>% 
                    filter(grepl(paste(input$benchmark_type, 
                                       collapse = '|'), tags)) %>%
                    group_by(year) %>%
                    summarize(across(c(Agg_Score, average_forever), mean, na.rm=TRUE))
  
                })
                
                # Filter GENRE Dataframe
                
                benchmark_df_genre <- reactive({
                  benchmark_table %>% 
                    filter(grepl(paste(input$benchmark_genre, 
                                       collapse = '|'), tags)) %>%
                    group_by(year) %>%
                    summarize(across(c(Agg_Score, average_forever), mean, na.rm=TRUE))
                  
                })
                
                # Filter MULTIPLAYER Dataframe
                benchmark_df_multiplayer <- reactive({
                  if(input$benchmark_multiplayer == TRUE){
                    benchmark_table %>% 
                    filter(multiplayer == TRUE) %>%
                    group_by(year) %>%
                    summarize(across(c(Agg_Score, average_forever), mean, na.rm=TRUE))
                  } else {
                    benchmark_table %>% 
                    filter(multiplayer == FALSE) %>%
                    group_by(year) %>%
                    summarize(across(c(Agg_Score, average_forever), mean, na.rm=TRUE))
                  }
                })
                
                # Filter PERSPECTIVE Dataframe
                benchmark_df_perspective <- reactive({
                  if(input$benchmark_perspective == 'First-Person'){
                    benchmark_table %>% 
                      filter(perspective == 'First-Person') %>%
                      group_by(year) %>%
                      summarize(across(c(Agg_Score, average_forever), mean, na.rm=TRUE))
                  } else {
                    benchmark_table %>% 
                      filter(perspective == 'Third-Person') %>%
                      group_by(year) %>%
                      summarize(across(c(Agg_Score, average_forever), mean, na.rm=TRUE))
                  }
                })
                
                # Filter PRICE Dataframe
                benchmark_df_price <- reactive({
                  benchmark_table %>%
                    filter(price >= input$benchmark_price[1] & price <= input$benchmark_price[2]) %>%
                    group_by(year) %>%
                    summarize(across(c(Agg_Score, average_forever), mean, na.rm=TRUE))
                })
                
                # GENERATING GRAPHS AND BENCHMARKS 
                
                observeEvent(
                  input$benchmark_run, {
                   output$benchmark_type_plot <-renderPlotly({
                     ## FOR TYPE 
                     #  Generate Plot 
                     plot_ly(data = benchmark_df_type(),
                             x = ~year) %>% 
                             add_trace(y = ~average_forever, 
                                       name = 'Average Players',
                                       mode = 'lines',
                                       line = list(color = '#0081a7', 
                                                   width = 2)) %>% 
                            add_trace(y = ~average_forever, 
                                 name = 'Average Players',
                                 mode = 'markers',
                                 marker = list(color = '#0081a7'),
                                 showlegend = FALSE) %>% 
                             add_trace(y = ~Agg_Score, 
                                       name = 'Average Rating',
                                       mode = 'lines', 
                                       yaxis = "y2",
                                       line = list(color = '#f07167', 
                                                   width = 2)) %>% 
                             add_trace(y = ~Agg_Score, 
                                       name = 'Average Rating',
                                       mode = 'markers',
                                       yaxis = "y2",
                                       marker = list(color = '#f07167'),
                                       showlegend = FALSE) %>%
                             layout(title = paste("Average Metrics for Type/s:",paste(input$benchmark_type,
                                                                                     collapse = ", ")), 
                                   yaxis2 =list(overlaying = "y",
                                                side = "right",
                                                title = "Average Rating",
                                                color = '#f07167'),
                                   yaxis = list(title = "Average Weekly Players",
                                                color = '#0081a7'),
                                   xaxis = list(title = "Year")
                     ) 
                                 
                   })
                   
                   # Generate Value Box 
                   output$benchmark_type_rating <- renderValueBox({
                     valueBox(
                       format(round(mean(benchmark_df_type()$Agg_Score), 2), nsmall = 2),
                       subtitle = 'Average Rating',
                       icon = icon('star'),
                       color = 'teal')
                   })
                   output$benchmark_type_users <- renderValueBox({
                     valueBox(
                       round(mean(benchmark_df_type()$average_forever), 0),
                       subtitle = 'Average Weekly Players',
                       icon = icon('person'),
                       color = 'teal')
                   })
                   
                   ## FOR GENRE 
                   #  Generate Plot 
                   output$benchmark_genre_plot <-renderPlotly({
                     plot_ly(data = benchmark_df_genre(),
                             x = ~year) %>% 
                       add_trace(y = ~average_forever, 
                                 name = 'Average Players',
                                 mode = 'lines',
                                 line = list(color = '#0081a7', 
                                             width = 2)) %>% 
                       add_trace(y = ~average_forever, 
                                 name = 'Average Players',
                                 mode = 'markers',
                                 marker = list(color = '#0081a7'),
                                 showlegend = FALSE) %>% 
                       add_trace(y = ~Agg_Score, 
                                 name = 'Average Rating',
                                 mode = 'lines', 
                                 yaxis = "y2",
                                 line = list(color = '#f07167', 
                                             width = 2)) %>% 
                       add_trace(y = ~Agg_Score, 
                                 name = 'Average Rating',
                                 mode = 'markers',
                                 yaxis = "y2",
                                 marker = list(color = '#f07167'),
                                 showlegend = FALSE) %>%
                       layout(title = paste("Average Metrics for Genre/s:",paste(input$benchmark_genre,
                                                                               collapse = ", ")), 
                              yaxis2 =list(overlaying = "y",
                                           side = "right",
                                           title = "Average Rating",
                                           color = '#f07167'),
                              yaxis = list(title = "Average Weekly Players",
                                           color = '#0081a7'),
                              xaxis = list(title = "Year")
                       ) 
                     
                  })
                
                # Generate Value Box 
                output$benchmark_genre_rating <- renderValueBox({
                  
                  valueBox(
                    format(round(mean(benchmark_df_genre()$Agg_Score), 2), nsmall = 2),
                    subtitle = 'Average Rating',
                    icon = icon('star'),
                    color = 'teal')
                })
                output$benchmark_genre_users <- renderValueBox({
                  valueBox(
                    round(mean(benchmark_df_genre()$average_forever), 0),
                    subtitle = 'Average Weekly Players',
                    icon = icon('person'),
                    color = 'teal')
                })
                
                ## FOR MULTIPLAYER 
                #  Generate Plot 
                output$benchmark_multiplayer_plot <-renderPlotly({
                  plot_ly(data = benchmark_df_multiplayer(),
                          x = ~year) %>% 
                    add_trace(y = ~average_forever, 
                              name = 'Average Players',
                              mode = 'lines',
                              line = list(color = '#0081a7', 
                                          width = 2)) %>% 
                    add_trace(y = ~average_forever, 
                              name = 'Average Players',
                              mode = 'markers',
                              marker = list(color = '#0081a7'),
                              showlegend = FALSE) %>% 
                    add_trace(y = ~Agg_Score, 
                              name = 'Average Rating',
                              mode = 'lines', 
                              yaxis = "y2",
                              line = list(color = '#f07167', 
                                          width = 2)) %>% 
                    add_trace(y = ~Agg_Score, 
                              name = 'Average Rating',
                              mode = 'markers',
                              yaxis = "y2",
                              marker = list(color = '#f07167'),
                              showlegend = FALSE) %>%
                    layout(title = {if(input$benchmark_multiplayer == "Yes"){
                                      "Average Metrics for Multiplayer Games"
                                    } else {
                                      "Average Metrics for Singleplayer Games"
                                    }}, 
                           yaxis2 =list(overlaying = "y",
                                        side = "right",
                                        title = "Average Rating",
                                        color = '#f07167'),
                           yaxis = list(title = "Average Weekly Players",
                                        color = '#0081a7'),
                           xaxis = list(title = "Year")
                    ) 
                  
                })
                
                # Generate Value Box 
                output$benchmark_multiplayer_rating <- renderValueBox({
                  
                  valueBox(
                    format(round(mean(benchmark_df_multiplayer()$Agg_Score), 2), nsmall = 2),
                    subtitle = 'Average Rating',
                    icon = icon('star'),
                    color = 'teal')
                })
                output$benchmark_multiplayer_users <- renderValueBox({
                  valueBox(
                    round(mean(benchmark_df_multiplayer()$average_forever), 0),
                    subtitle = 'Average Weekly Players',
                    icon = icon('person'),
                    color = 'teal')
                })
                
                ## FOR PERSPECTIVE 
                #  Generate Plot 
                output$benchmark_perspective_plot <-renderPlotly({
                  plot_ly(data = benchmark_df_perspective(),
                          x = ~year) %>% 
                    add_trace(y = ~average_forever, 
                              name = 'Average Players',
                              mode = 'lines',
                              line = list(color = '#0081a7', 
                                          width = 2)) %>% 
                    add_trace(y = ~average_forever, 
                              name = 'Average Players',
                              mode = 'markers',
                              marker = list(color = '#0081a7'),
                              showlegend = FALSE) %>% 
                    add_trace(y = ~Agg_Score, 
                              name = 'Average Rating',
                              mode = 'lines', 
                              yaxis = "y2",
                              line = list(color = '#f07167', 
                                          width = 2)) %>% 
                    add_trace(y = ~Agg_Score, 
                              name = 'Average Rating',
                              mode = 'markers',
                              yaxis = "y2",
                              marker = list(color = '#f07167'),
                              showlegend = FALSE) %>%
                    layout(title = {if(input$benchmark_perspective == "First-Person"){
                      "Average Metrics for First-Person Games"
                    } else {
                      "Average Metrics for Third-Person Games"
                    }}, 
                    yaxis2 =list(overlaying = "y",
                                 side = "right",
                                 title = "Average Rating",
                                 color = '#f07167'),
                    yaxis = list(title = "Average Weekly Players",
                                 color = '#0081a7'),
                    xaxis = list(title = "Year")
                    ) 
                  
                })
                
                # Generate Value Box 
                output$benchmark_perspective_rating <- renderValueBox({
                  
                  valueBox(
                    format(round(mean(benchmark_df_perspective()$Agg_Score), 2), nsmall = 2),
                    subtitle = 'Average Rating',
                    icon = icon('star'),
                    color = 'teal')
                })
                output$benchmark_perspective_users <- renderValueBox({
                  valueBox(
                    round(mean(benchmark_df_perspective()$average_forever), 0),
                    subtitle = 'Average Weekly Players',
                    icon = icon('person'),
                    color = 'teal')
                })
                   
                ## FOR PRICE 
                #  Generate Plot 
                output$benchmark_price_plot <-renderPlotly({
                  plot_ly(data = benchmark_df_price(),
                          x = ~year) %>% 
                    add_trace(y = ~average_forever, 
                              name = 'Average Players',
                              mode = 'lines',
                              line = list(color = '#0081a7', 
                                          width = 2)) %>% 
                    add_trace(y = ~average_forever, 
                              name = 'Average Players',
                              mode = 'markers',
                              marker = list(color = '#0081a7'),
                              showlegend = FALSE) %>% 
                    add_trace(y = ~Agg_Score, 
                              name = 'Average Rating',
                              mode = 'lines', 
                              yaxis = "y2",
                              line = list(color = '#f07167', 
                                          width = 2)) %>% 
                    add_trace(y = ~Agg_Score, 
                              name = 'Average Rating',
                              mode = 'markers',
                              yaxis = "y2",
                              marker = list(color = '#f07167'),
                              showlegend = FALSE) %>%
                    layout(title = {if(input$benchmark_ftp == 'Yes'){
                                     "Average Metrics for Free-to-Plays"
                                   } else {
                                     paste("Average Metrics for Price Range:", 
                                           input$benchmark_price[1], 
                                           "-", 
                                           input$benchmark_price[2])
                                   }}, 
                           yaxis2 =list(overlaying = "y",
                                        side = "right",
                                        title = "Average Rating",
                                        color = '#f07167'),
                           yaxis = list(title = "Average Weekly Players",
                                        color = '#0081a7'),
                           xaxis = list(title = "Year")
                    ) 
                  
                })
                
                # Generate Value Box 
                output$benchmark_price_rating <- renderValueBox({
                  
                  valueBox(
                    format(round(mean(benchmark_df_price()$Agg_Score), 2), nsmall = 2),
                    subtitle = 'Average Rating',
                    icon = icon('star'),
                    color = 'teal')
                })
                output$benchmark_price_users <- renderValueBox({
                  valueBox(
                    round(mean(benchmark_df_price()$average_forever), 0),
                    subtitle = 'Average Weekly Players',
                    icon = icon('person'),
                    color = 'teal')
                })
                   
                output$benchmark_header <- renderText({ 
                  paste("Benchmarks for: ", input$benchmark_title) 
                })
                   
                # end of observe event    
                })
  
               }
  )
}