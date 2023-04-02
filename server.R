######################### PATHS TO SERVER ELEMENTS ##############################
source(file.path('pages/home.R'),  local = TRUE)
source(file.path('pages/overview.R'),  local = TRUE)
source(file.path('pages/comparison.R'),  local = TRUE)
source(file.path('pages/benchmark.R'),  local = TRUE)
source(file.path('pages/about.R'), local = TRUE)


server <- function(input, output, session) {

  observe({
    onclick('start_button', 
            updateTabItems(session, inputId = 'main_page', selected = 'overview'))
  })
  
  comparisonServer('comparison', input, output, session)
  overviewServer('overview', input, output, session)
  benchmarkServer('benchmark', input, output, session)
}

shinyServer(server)