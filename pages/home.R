
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
                                         "IT'S TIME TO GET YOUR GAME ON!"),
                                 p("PRESS START TO CONTINUE",
                                   style = 'color: #FFFFFF; font-size: 14px; margin-top: 5px;'),
                                 img(
                                   id = "start_button",
                                   src= "img/start.png",
                                   style="cursor:pointer;"
                                 )
                        )
                      ), 
                      fluidRow(style = 'padding: 20px;',
                        column(6,
                               img(id = "steam",
                                   src = 'img/steam.jpg',
                                   style = 'width: 100%'),
                               hr(style = "border-top: 1px solid #0081a7;"),
                               p("In 2021, the video game market was estimated to be 
                               $195.65 billion and is expected to expand at a compound 
                               annual growth rate (CAGR) of 12.9% from 2022 to 2030. 
                               Steam is a major digital game distribution platform run 
                               by the Valve Corporation. The platform accounts for 75% of 
                               the global PC games market. In 2020, Steam had 120 million 
                               active users per month, averaging 62.6 million people use 
                               Steam every day. This growth resulted in the number of hours 
                               spent playing games on Steam climbing by 50.7% year over year,
                               and game sales increased by nearly 21.4%.")
                               
                        ),
                               
                        column(6, 
                               img(id = "steam",
                                   src = 'img/shiny.png',
                                   style = 'width: 100%'),hr(style = "border-top: 1px solid #0081a7;"),
                                   p("Shiny is an R package that enables building interactive web applications 
                                     that can execute R code on the backend. With Shiny, you can host standalone 
                                     applications on a webpage, embed interactive charts in R Markdown documents, 
                                     or build dashboards. You can also extend your Shiny applications with CSS themes, 
                                     HTML widgets, and JavaScript actions.")
                        )
                      ),
        
                    )
              
)


