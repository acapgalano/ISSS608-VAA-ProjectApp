
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
                                 style = "background-color: #ffffff; padding: 5px;",
                                value = 'overview_topgames_tab', 
                                icon = icon('gamepad'),
                                fluidRow(style = 'padding: 10px 5px 5px 30px;',
                                  h2("Top Games"),
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
                                  fluidRow(column(12,
                                           div(style = "overflow-y: auto; height: 300px",
                                               dataTableOutput(ns('overview_topgames_table')))
                                    )
                                  )
                        ),
                        # For Developers 
                        tabPanel("AAA Developers",
                                 value = 'overview_a3devs_tab', 
                                 icon = icon('star'),
                                 fluidRow(
                                   sidebarLayout(sidebarPanel(sliderInput(ns('overview_a3devs_year'),
                                                                          "Select a range of year:",
                                                                          value = c(year(as.Date("1997-01-01", "%Y-%m-%d")),year(as.Date("2018-01-01", "%Y-%m-%d"))),
                                                                          min = year(as.Date("1997-01-01", "%Y-%m-%d")), 
                                                                          max = year(as.Date("2018-01-01", "%Y-%m-%d"))),
                                                              radioButtons(ns('overview_a3devs_sortby'),"Sort by:",
                                                                           choiceNames = list("Rating",
                                                                                              "Popularity"),
                                                                           choiceValues = list("Rating",
                                                                                               "Popularity")),
                                                        
                                                        
                                   ),
                                     mainPanel(
                                       div(style = 'padding:5px 5px 5px 5px;',
                                         plotlyOutput(ns('overview_a3devs_plot')),
                                         useShinyjs()
                                       )
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   column(1, p("")),
                                   column(10,
                                          dataTableOutput(ns('overview_a3devs_table'))
                                   ),
                                   column(1, p(""))
                                 )
                        ),
                 # For Indie Developers
                 tabPanel("Indie Developers",
                          value = 'overview_indie_tab', 
                          icon = icon('snowflake'),
                          fluidRow(
                            sidebarLayout(sidebarPanel(
                              sliderInput(ns('overview_indie_topn'),
                                          "Select top N games:",
                                          value = 5, 
                                          min = min(5), 
                                          max = max(30), 
                                          step = 5),
                              
                              radioButtons(ns('overview_indie_sortby'),"Sort by:",
                                           choiceNames = list("Rating",
                                                              "Popularity"),
                                           choiceValues = list("Rating",
                                                               "Popularity"))
                              ),
                              mainPanel(
                                plotlyOutput(ns('overview_indie_plot')),
                                useShinyjs()
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
                             hovertemplate = paste('<b>Game Name</b>: %{x}', 
                                                   '<br><b>Average Player Count</b>: %{y}'),
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
                                    title =  paste("Top ", input$overview_topgames_topn, " Games by Popularity"), 
                                    margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
                                    hoverlabel = list(bgcolor = '#0081a7', 
                                                      font = list(size = 12)))
                   }else{
                     plot_ly(data = top_games(), 
                             x = ~reorder(name.x, -Agg_Score), 
                             y = ~Agg_Score, 
                             type = 'scatter',
                             name = 'Average Positive Rating',
                             hovertemplate = paste('<b>Game Name</b>: %{x}', 
                                                   '<br><b>Average Positive Rating</b>: %{y}',
                                                   '<br><b>Average Player Count</b>: %{marker.size}'),
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
                              title =  paste("Top ", input$overview_topgames_topn, " Games by Rating"), 
                              margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
                              hoverlabel = list(bgcolor = '#f07167', 
                                                font = list(size = 14,
                                                            color = '#fed9b7')))
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
                                                                   }, rownames = FALSE)
                 
                
                 
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
                     g <- ggplot(top_publish_users(), aes(x = developer, y = Sum_User, fill = factor(year), tooltip = paste("Developer = ", developer, "<br>", "Sum_User = ", Sum_User, "<br>", "Year = ", year))) + 
                       geom_bar(stat = "identity") +
                       scale_color_viridis_d() +
                       labs(x = "Developer", 
                            y = "Number of User",
                            title = "Top Steam Developers") +
                       theme(legend.position = "top") +
                       theme_bw() +
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank())
                     
                     ggplotly(g)
                   } else {
                     g <- ggplot(top_publish_rating(), aes(x = developer, y = Ave_Score, fill = factor(year), tooltip = paste("Developer = ", developer, "<br>", "New User = ", Ave_Score, "<br>", "Year = ", year))) + 
                       geom_bar(stat = "identity") +
                       scale_color_viridis_d() +
                       labs(x = "Developer", 
                            y = "Average Rating",
                            title = "Top Steam Developers") +
                       theme(legend.position = "top") +
                       theme_bw() +
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank())
                     
                     ggplotly(g)
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
                           marker = list(color = '#006000', 
                                         line = list(color = 'white', width = 0.5)),
                           name = 'Average Users') %>% 
                           add_trace(x = ~reorder(name.x, -average_forever), 
                                     y = ~Agg_Score, 
                                     type = 'scatter', 
                                     mode = 'lines', 
                                     line = list(color = 'red', 
                                                 width = 2), 
                                     yaxis = 'y2', 
                                     name = 'Aggregate Score (%)') %>% 
                                    layout(title = 'Top 10 Indie Games', 
                                           xaxis = list(title = 'Games', 
                                                        showticklabels = FALSE),
                                           yaxis = list(title = 'No. of Users'), 
                                           yaxis2 = list(title = 'Aggregate Score (%)', 
                                                         overlaying = 'y', 
                                                         side = 'right', 
                                                         tickformat = '%'),
                                           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))   
                                          
                  
                   
                 })
                 
                 # Generate Indie Games Table 
                 
                 
                 
                 
                 
                 
                 
                 
               })
}
