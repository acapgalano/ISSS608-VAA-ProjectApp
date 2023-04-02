
######################### PATHS TO UI ELEMENTS ##############################
source(file.path('pages/home.R'),  local = TRUE)
source(file.path('pages/overview.R'),  local = TRUE)
source(file.path('pages/comparison.R'),  local = TRUE)
source(file.path('pages/benchmark.R'),  local = TRUE)
source(file.path('pages/about.R'), local = TRUE)


ui <- 
  #list(tags$head(HTML('<link rel="icon", href="img/logo.png", 
  #                                 type="image/png" />'))),
  #div(style="padding: 1px 0px; width: '100%'",
  #    titlePanel(
  #      title="", windowTitle="My Window Title"
  #    )
  #),
  navbarPage(
    id = 'main_page',
    theme = 'styles.css',
    windowTitle="My Window Title",
    title = div(img(src='img/logo.png'),
                height = "50px",
                width = "100px",
                style = "position: relative; margin:-15px 0px; display:right-align;"),
    setBackgroundColor( color = "#fdfcdc",
                        gradient = c("linear", "radial"),
                        direction = c("bottom", "top", "right", "left"),
                        shinydashboard = FALSE
    ),
    homeTab,
    overviewTab('overview'),
    comparisonTab('comparison'), 
    benchmarkTab('benchmark'),
    aboutTab
  )

shinyUI(ui)