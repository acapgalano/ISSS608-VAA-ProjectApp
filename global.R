# Package to Load
pacman::p_load(
               # Shiny Related Packages
               shiny, 
               shinydashboard,
               shinyjs,
               shinycssloaders,
               shinyWidgets,
               htmlwidgets,
               bslib,
               DT,
               
               # Tidyverse/Analytics
               tidyr,
               knitr,
               tidyverse, 
               readxl,
               lubridate,
               tidytext,
               rlist, 
               plotly,
               gganimate, 
               ggthemes, 
               gifski, 
               gapminder, 
               reshape2, 
               pracma, 
               ggstatsplot, 
               bslib,
               ggiraph,
               ggpubr,
               ggvis,
               cowplot)

############################## PATHS TO DATA #################################

steam <- read_csv("data/steam_join.csv") %>% mutate(genre = str_split(genre, ", "))
invest <- read_csv("data/invest.csv")
steamspy <- read_xlsx("data/steamspy_data.xlsx")
steamapp <- read_xlsx("data/steamapp_data.xlsx")
AAA_dev <- read_xlsx("data/triple_a_devs.xlsx")

################################# OVERVIEW ##################################

merge_steam <- merge(steamspy, 
                     steamapp, by.x = "appid", 
                     by.y = "steam_appid", 
                     all.x = TRUE)  %>% 
               mutate(total_reviews = positive + negative) %>% 
               mutate(genre = str_split(genre, ", "))

AAA_list <- AAA_dev$Developer

top_publish_table <- subset(merge_steam, select = c('appid', 'name.x', 'developer', 'publisher', 'Agg_Score', 'average_forever', 'median_forever', 'price', 'genre', 'website', 'metacritic_score', 'recommendations', 'achievements', 'short_description', 'release_date' ))

top_publish_table <- top_publish_table[top_publish_table$developer %in% AAA_list,] 



indie_table <- merge_steam %>% filter(map_lgl(genre, ~'Indie' %in% .))

############################## COMPARISON ###################################

genretype <- list()
for (i in 1:length(steam$genre)){
  for (j in steam$genre[[i]]){
    if (!(j %in% genretype)){
      genretype<-list.append(genretype,j)
      
    }
  }
} 

inv <- steam %>%
       group_by(by = developer) %>%
       summarise(n = n()) %>%
       arrange(desc(n)) %>%
       filter(n >= 25)

################################# BENCHMARK ##################################

type_list <- c('Action','RPG','Strategy','Simulation','RTS', 'Platformer',
               'Shooter','Fighting','Survival','Battle Royale','Visual Novel',
               'Puzzle','Point & Click', 'Roguelike')

type_list <- sort(type_list)


genre_list <- c('Sci-Fi', 'Fantasy', 'Horror','Casual', 'Mystery','Comedy',
               'Anime','Sports', 'Lovecraftian')
genre_list <- sort(genre_list)

benchmark_table <- merge_steam %>% 
                   filter(total_reviews > 50) %>%
                   mutate(year = year(as.Date(release_date))) %>% 
                   mutate(multiplayer = ifelse(grepl('Multiplayer|Massively', tags),
                                                              TRUE, 
                                                              FALSE)) %>%
                   mutate(perspective = ifelse(grepl('Third|Top-Down', tags),
                                              'Third-Person', 
                                              'First-Person'))

