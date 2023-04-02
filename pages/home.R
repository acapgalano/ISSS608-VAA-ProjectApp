
#
#
#


homeTab <- tabPanel("Home", 
                    tabName = "home", 
                    value = "home", 
                    icon = icon("gamepad"),
                    fluidPage(
                      fluidRow(
                        tags$div(class = "jumbotron text-center", 
                                 style = "margin-bottom:0px;margin-top:0px;",
                                 tags$h2(class = 'jumbotron-heading', 
                                         style = 'margin-bottom:0px; margin-top:0px;color:#ffffff;', 
                                         "It's time to get your game on."),
                                 p("Press START to continue sgdfgdgdgdgdf"),
                        )
                      ), 
                      fluidRow(
                        column(6, align = 'center',
                               img(id = "steam",
                                   src = 'img/steam.jpg',
                                   style = 'width: 100%')
                        ),
                               
                        column(6, 
                               img(id = "steam",
                                   src = 'img/steam.jpg',
                                   style = 'width: 100%')
                        )
                      ),
                      fluidRow(
                        column(4),
                        column(4, align = 'center',
                               img(
                                 id = "start_button",
                                 src= "img/start.png",
                                 style="cursor:pointer;"
                               )
                        ),
                        column(4)
                      )
                      
                    )
              
)


