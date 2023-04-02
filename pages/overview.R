
overviewTab <- function(id){
  
   ns <- NS(id)
  
   tabPanel("Overview",
   tabName = "overview", 
   icon = icon("globe"),
   value = "overview",
   fluidPage(
     # TODO: Think if need to add something here before tabset
     tabsetPanel(type = 'tabs',
                  id = "overview_tabs",
                  # First Tab
                        tabPanel("Top Games",
                                style = "background-color: #ffffff;",
                                value = 'overview_topgames_tab', 
                                icon = icon('gamepad'),
                                fluidRow(style = 'padding: 10px 5px 5px 30px;',
                                  h2("Looking at the Best of the Best", style = 'color: #0081a7;'),
                                  p("In this tab, you can take a look at the top N games by rating or popularity. Make sure to hover to find out more about the games.")
                                ),
                                fluidRow(style = 'padding: 10px 15px 10px 30px;',
                                        sidebarLayout(sidebarPanel(p("Use the following inputs:"),
                                                                  div(style = 'padding:5px 5px 5px 5px;',
                                                                  sliderInput(ns('overview_topgames_topn'),
                                                                              "Select top N games:",
                                                                              value = 5, 
                                                                              min = min(5), 
                                                                              max = max(30), 
                                                                              step = 5),
                                                                  
                                                                  radioButtons(ns('overview_topgames_sortby'),"Sort by:",
                                                                               choices = c(
                                                                                 "Popularity" = "Popularity",
                                                                                 "Rating" = "Rating")
                                                                               ),
                                                                  
                                                                  selectInput(ns("overview_topgames_genre"),
                                                                              "Filter by:", 
                                                                              c('All', genretype), 
                                                                              multiple = FALSE)#,
                                          
                                                                   ) #end div
                                                                  
                                          
                                                    ),
                                                    mainPanel(
                                                      div(style = 'padding:20px 20px 20px 20px;',
                                                          plotlyOutput(ns('overview_topgames_plot')),
                                                          useShinyjs()
                                                      )
                                                    )
                                          )
                                  ),
                                  hr(style = "border-top: 10px solid #fdfcdc;"),
          
                                  fluidRow(style = 'padding: 10px;',
                                           #div(style = 'padding-left: 20px;',
                                          #     h2("Table of Game Data")),
                                           column(12,
                                           div(style = "overflow-y: auto; height: 500px;",
                                               dataTableOutput(ns('overview_topgames_table')))
                                    )
                                  )
                        ),
                        # For Developers 
                        tabPanel("AAA Developers",
                                 style = "background-color: #ffffff;",
                                 value = 'overview_a3devs_tab', 
                                 icon = icon('star'),
                                 fluidRow(
                                   style = 'padding: 10px 5px 5px 30px;',
                                   h2("The Kings of the Video Game Industry",
                                      style = 'color: #0081a7;'),
                                   p("Triple A developers are big companies with lots of manpower who are well established in the video game industry. They create the most popular and highest quality (/supposedly/) games.")
                                 ),
                                 fluidRow(
                                   sidebarLayout(sidebarPanel(style = 'margin-left:35px;',
                                                        div(style = 'padding: 5px;',
                                                        sliderInput(ns('overview_a3devs_year'),
                                                                    "Select a range of year:",
                                                                    value = c(year(as.Date("1997-01-01", "%Y-%m-%d")),year(as.Date("2018-01-01", "%Y-%m-%d"))),
                                                                    min = year(as.Date("1997-01-01", "%Y-%m-%d")), 
                                                                    max = year(as.Date("2018-01-01", "%Y-%m-%d")),
                                                                    sep = ""),
                                                        radioButtons(ns('overview_a3devs_sortby'),"Sort by:",
                                                                     choiceNames = list("Rating",
                                                                                        "Popularity"),
                                                                     choiceValues = list("Rating",
                                                                                         "Popularity"))
                                                        )
                                   ),
                                     mainPanel(
                                       div(style = 'padding:5px 5px 5px 5px; margin-right: 20px;',
                                         plotlyOutput(ns('overview_a3devs_plot')),
                                         useShinyjs()
                                       )
                                     )
                                   )
                                 ),
                                 hr(style = "border-top: 10px solid #fdfcdc;"),
                                 
                                 #fluidRow(style = 'padding: 10px;',
                                          #div(style = 'padding-left: 20px;',
                                          #     h2("Table of Game Data")),
                                  #        column(12,
                                  #               div(style = "overflow-y: auto; height: 500px;",
                                  #                   dataTableOutput(ns('overview_a3devs_table')))
                                  #        )
                                 #)
                                 # AAA Devs Info
                                 fluidRow(
                                   column(1),
                                   column(2),
                                   column(2),
                                   column(2),
                                   column(2),
                                   column(2),
                                   column(1)
                                 )
                        ),
                 # For Indie Developers
                 tabPanel("Indie Developers",
                          style = "background-color: #ffffff;",
                          value = 'overview_indie_tab', 
                          icon = icon('snowflake'),
                          fluidRow(
                            style = 'padding: 10px 5px 5px 30px;',
                            h2("Appreciating Hidden Gems",
                               style = 'color: #0081a7;'),
                            p("Passion goes a long way. You don't need a team of 100 people or the best tools to create a masterpiece. These games, despite their humble beginnings, found their way into our hard drives and our hearts.")
                          ),
                          fluidRow(
                            sidebarLayout(sidebarPanel(
                              style = 'margin-left: 30px;',
                              sliderInput(ns('overview_indie_topn'),
                                          "Select top N games:",
                                          value = 5, 
                                          min = min(5), 
                                          max = max(30), 
                                          step = 5),
                              
                              #radioButtons(ns('overview_indie_sortby'),"Sort by:",
                              #             choiceNames = list("Rating",
                              #                                "Popularity"),
                              #             choiceValues = list("Rating",
                              #                                 "Popularity"))
                              ),
                              mainPanel(
                                div(
                                  style = 'margin-right: 20px; padding-right: 10px',
                                  plotlyOutput(ns('overview_indie_plot')),
                                  useShinyjs()
                                )
                              )
                            )
                          ),
                          fluidRow(
                            column(1),
                            column(10,
                                   dataTableOutput(ns('overview_indie_table'))
                            ),
                            column(1)
                          )
                   
                 )
                        
                      )
            )
      )
} 

overviewServer <- function(id, input, output, session){
  moduleServer(id,
               function(input, output, session){
                 
                 # Generate Dataframe of Top N Games
                 top_games <- reactive({
                   if(input$overview_topgames_genre != "All"){
                      x <- merge_steam %>% 
                      filter(genre %in% input$overview_topgames_genre)
                   } else {
                      x <- merge_steam
                   }
                   
                   if(input$overview_topgames_sortby == 'Rating'){
                   x %>% arrange(desc(Agg_Score)) %>%
                     filter(total_reviews >= 10000) %>%
                     head(input$overview_topgames_topn)
                   } else {
                   x %>% arrange(desc(average_forever)) %>%
                     filter(total_reviews >= 10000) %>%
                     head(input$overview_topgames_topn)
                   }
                   
                 })
                   
                 
                 # Generate Top Games Bar Graph

                 output$overview_topgames_plot <- renderPlotly({
                   if(input$overview_topgames_sortby == 'Popularity'){
                     plot_ly(data = top_games(), 
                             x = ~reorder(name.x, -average_forever), 
                             y = ~average_forever, 
                             type = 'bar',
                             name = 'Average Player Count',
                             template = 'ggplot2',
                             hovertemplate = ~paste('<b>Game Name</b>: %{x}', 
                                                   '<br><b>Average Player Count</b>: %{y}',
                                                   '<br><b>Description</b>: ', gsub('(.{1,45})(\\s|$)', '\\1\n', short_description)),
                             marker = list(color = '#00afb9',
                                           line = list(color = '#0081a7',
                                                       width = 3))) %>%
                       
                             layout(xaxis = list(title = 'Games', 
                                                 showticklabels = {if(input$overview_topgames_topn == 5 ){
                                                                      TRUE
                                                                  } else {
                                                                      FALSE
                                                                  }}),
                                    yaxis = list(title = 'Number of Players'),
                                    title = list(text = paste("Top ", input$overview_topgames_topn, " Games by Popularity"),
                                                 font = list(size = 20)), 
                                    margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
                                    hoverlabel = list(bgcolor = '#0081a7', 
                                                      font = list(size = 12)),
                                    paper_bgcolor = '#00afb9')
                   }else{
                     plot_ly(data = top_games(), 
                             x = ~reorder(name.x, -Agg_Score), 
                             y = ~Agg_Score, 
                             type = 'scatter',
                             name = 'Average Positive Rating',
                             template = 'ggplot2',
                             hovertemplate = ~paste('<b>Game Name</b>: %{x}', 
                                                   '<br><b>Average Positive Rating</b>:', percent(Agg_Score),
                                                   '<br><b>Average Player Count</b>: %{marker.size}',
                                                   '<br><b>Description</b>: ', 
                                                   gsub('(.{1,45})(\\s|$)', '\\1\n', short_description)),
                             marker = list(size = ~average_forever, 
                                           sizemode = 'area',
                                           sizeref = 10,
                                           color = '#fed9b7',
                                           line = list(
                                             color = '#f07167',
                                             width = 3
                                           )
                                           )) %>%
                       
                       layout(xaxis = list(title = 'Games', 
                                           showticklabels = {if(input$overview_topgames_topn == 5 ){
                                             TRUE
                                           } else {
                                             FALSE
                                           }}),
                              yaxis = list(title = 'Average Positive Rating'),
                              title = list (text = paste("Top ", input$overview_topgames_topn, " Games by Rating"),
                                            font = list(size = 20)), 
                              margin = list(l = 50, r = 50, b = 50, t = 50, pad = 5),
                              hoverlabel = list(bgcolor = '#f07167', 
                                                font = list(size = 14,
                                                            color = '#fed9b7')),
                              paper_bgcolor = '#fed9b7')
                   }
                   
                 })
                 
                 # Generate Top Games Table 
                 
                 output$overview_topgames_table <- renderDataTable({subset(top_games(), 
                                                                           select = c('appid', 
                                                                                      'name.x', 
                                                                                      'developer', 
                                                                                      'publisher', 
                                                                                      'Agg_Score', 
                                                                                      'average_forever', 
                                                                                      'price', 
                                                                                      'genre', 
                                                                                      'website', 
                                                                                      'release_date')) %>% 
                                                                           rename('App ID' = appid,
                                                                                 'Name' = name.x,
                                                                                 'Developer' = developer,
                                                                                 'Publisher' = publisher,
                                                                                 'Average Rating' = Agg_Score,
                                                                                 'Average Weekly Players' = average_forever,
                                                                                 'Price' = price,
                                                                                 'Tags' = genre,
                                                                                 'Website' = website,
                                                                                 'Release Date' = release_date)%>% 
                                                                          mutate('Release Date' = format(`Release Date`, "%m-%d-%Y"))
                                                                   }, rownames = FALSE,
                                                                   options = list(scrollX = TRUE,
                                                                                  scroller = TRUE,
                                                                                  autoWidth = TRUE, 
                                                                                  pageLength = 30, 
                                                                                  lengthChange = FALSE,
                                                                                  dom = 't')
                 )
                 
                
                 
                 # Generate Top Devs Dataframe
                 top_publish_rating <- reactive({
                     top_publish_table %>% mutate(year = year(as.Date(release_date))) %>%
                     filter(year >= input$overview_a3devs_year[1] & year <= input$overview_a3devs_year[2]) %>% 
                     group_by(developer, year) %>%
                     summarize('Ave_Score' = mean(Agg_Score)) %>%
                     ungroup() %>%
                     arrange(desc(year)) %>% 
                     na.omit()
    
                 })
                 
                top_publish_users <- reactive({
                  top_publish_table %>% mutate(year = year(as.Date(release_date))) %>%
                    filter(year >= input$overview_a3devs_year[1] & year <= input$overview_a3devs_year[2]) %>%
                    group_by(developer, year) %>%
                    summarize('Sum_User' = sum(average_forever)) %>%
                    ungroup() %>%
                    arrange(desc(year)) %>% 
                    na.omit()
                })
                 
                 
                 
                 
                 
                 # Generate Top Devs Graph 
                 
                 output$overview_a3devs_plot <- renderPlotly({
                   if(input$overview_a3devs_sortby != 'Rating'){
                     plot_ly(top_publish_users(), x = ~developer, y = ~Sum_User, color = ~factor(year), colors = 'Spectral',
                             hovertemplate = ~paste("Game Developer: ", developer, "<br>Average Player Count: ", Sum_User, "<br>Year: ", year),
                             type = "bar") %>%
                       layout(xaxis = list(title = "Game Developer"),
                              yaxis = list(title = "Average Positive Rating"),
                              title = "Average Popularity of Games by AAA Steam Developers",
                              legend = list(title = "Year"),
                              hovermode = "closest",
                              showlegend = TRUE,
                              barmode = "stack",
                              margin = list(l = 40, r = 20, t = 50, b = 30),
                              plot_bgcolor = '#dcf8fa',
                              paper_bgcolor = '#00afb9',
                              font = list(color = '#ffffff'))
                   } else {
                       plot_ly(top_publish_rating(), x = ~developer, y = ~Ave_Score, color = ~factor(year), colors = 'Spectral',
                                 hovertemplate = ~paste("Game Developer: ", developer, "<br> Average Positive Rating: ", percent(Ave_Score), "<br>Year: ", year),
                                 type = "bar") %>%
                       layout(xaxis = list(title = "Game Developer"),
                              yaxis = list(title = "Average Player Count"),
                              title = "Average Rating of Games by AAA Steam Developers",
                              legend = list(title = "Year"),
                              hovermode = "closest",
                              showlegend = TRUE,
                              barmode = "stack",
                              margin = list(l = 40, r = 20, t = 50, b = 30),
                              plot_bgcolor = '#fcf1e6',
                              paper_bgcolor = '#fed9b7')
                   }
                   
                 })
                 
                 
                 
                 
                 
                 # Generate Dataframe of Top N Indie Games
                 
                 top_indie_games <- reactive({
                     indie_table %>%
                     arrange(desc(average_forever)) %>%
                     head(input$overview_indie_topn)
                 })
                 
                 
                 # Generate Indie Games Bar Graph
                 
                 output$overview_indie_plot <- renderPlotly({
                   plot_ly(data = top_indie_games(), 
                           x = ~reorder(name.x, -average_forever), 
                           y = ~average_forever, type = 'bar', 
                           marker = list(color = '#00afb9', 
                                         line = list(color = 'white', width = 0.5),
                                         hovertemplate = ~paste('<b>Game Name</b>:', name.x, 
                                                                '<br><b>Average Positive Rating</b>:', percent(Agg_Score),
                                                                '<br><b>Average Player Count</b>:', average_forever,
                                                                '<br><b>Description</b>: ', 
                                                                gsub('(.{1,45})(\\s|$)', '\\1\n', short_description))),
                           name = 'Average Player Count') %>% 
                           add_trace(x = ~reorder(name.x, -average_forever), 
                                     y = ~Agg_Score, 
                                     type = 'scatter', 
                                     mode = 'lines', 
                                     line = list(color = '#f07167', 
                                                 width = 2), 
                                     yaxis = 'y2', 
                                     name = 'Aggregate Score (%)',
                                     hovertemplate = ~paste('<b>Game Name</b>:', name.x, 
                                                            '<br><b>Average Positive Rating</b>:', percent(Agg_Score),
                                                            '<br><b>Average Player Count</b>:', average_forever,
                                                            '<br><b>Description</b>: ', 
                                                            gsub('(.{1,45})(\\s|$)', '\\1\n', short_description))) %>% 
                                    layout(title = paste("Top ", input$overview_indie_topn ," Played Indie Games on Steam"), 
                                           xaxis = list(title = 'Games', 
                                                        showticklabels = FALSE),
                                           yaxis = list(title = 'No. of Users'), 
                                           yaxis2 = list(title = 'Aggregate Score (%)', 
                                                         overlaying = 'y', 
                                                         side = 'right', 
                                                         tickformat = '1%'),
                                           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
                                           legend = list(font = list(size = 10),
                                                         orientation = "h",  
                                                         xanchor = "center", 
                                                         x = 0.75,
                                                         y = -0.25))   
                                          
                  
                   
                 })
                 
                 # Generate Indie Games Table 
                 
                 
                 
                 
                 
                 
                 
                 
               })
}
