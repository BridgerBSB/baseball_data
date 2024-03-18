#install newest version of baseballr
#devtools::install_github("BillPetti/baseballr")

#load libraries
library(baseballr)
library(tidyverse)
library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(shiny)
library(ggpubr)



#NCAA Scraper -> 5:33
#load D1 Teams
d1 <- baseballr::load_ncaa_baseball_teams() %>% dplyr::filter(year == 2024, division == 1)


#error handling
safescrape <- purrr::safely(ncaa_scrape)

#custom function to acquire baseball stats
ncaa_scraper <- function(team_id,team_name,type) {
  if (type == "batting") {
    message(paste("Getting Batting Stats for", team_name))
    
    stats <- safescrape(team_id = team_id, year = 2024, type = "batting")
  } else {
    message(paste("Getting Pitching Stats for", team_name))
    
    stats <- safescrape(team_id = team_id, year = 2024, type = "pitching")
  }
  
  
  Sys.sleep(sample(c(.000001, .000005, .004, .01, .003, .007, .00000696969),1))
  
  return(stats)
}


#map hitting and pitching stats
batting_stats <- 1:nrow(d1) %>% purrr::map(function(x)
  ncaa_scraper(d1$team_id[x], d1$team_name[x], type = "batting"))

pitcher_stats <- 1:nrow(d1) %>% purrr::map(function(x)
  ncaa_scraper(d1$team_id[x], d1$team_name[x], type = "pitching"))



# convert list to a data set
d1_batting_stats <- batting_stats %>% map("result") %>% bind_rows()
d1_pitching_stats <- pitcher_stats %>% map("result") %>% bind_rows()



#remove totals and opponent totals
d1_batting_stats <- d1_batting_stats %>% dplyr::filter(str_detect(player_name,"(Totals)")==FALSE,
                                                       str_detect(player_name,"(Opponent Totals)")==FALSE)

d1_pitching_stats <- d1_pitching_stats %>% dplyr::filter(str_detect(player_name,"(Totals)")==FALSE,
                                                       str_detect(player_name,"(Opponent Totals)")==FALSE)




#PART 2 5:34->9:44
#Westmont College Stats
ncaa_school_id_lu(team_name = "Westmont")

westmont_batting <- ncaa_team_player_stats(30260, year = 2024, type = "batting")
westmont_pitching <- ncaa_team_player_stats(30260, year = 2024, type = "pitching")

#bind westmont with D1 data
d1_westmont_batting <- rbind(westmont_batting, d1_batting_stats)
d1_westmont_pitching <- rbind(westmont_pitching, d1_pitching_stats)


#gather hitters and pitchers
chosen_batters <- c("Silva, Anthony", "Patterson, Daniel", "Belyeu, Max", "Flores, Jalin")
chosen_pitchers <- c("Yates, Zach", "Youngerman, Sean", "Koch, Mason", "Cranz, Robert")

#filter sters hitters and pitchers
ster_batters_23 <- d1_westmont_batting[d1_westmont_batting$player_name %in% chosen_batters,]
ster_pitchers_23 <- d1_westmont_pitching[d1_westmont_pitching$player_name %in% chosen_pitchers,]


#save.csv
write.csv(ster_batters_23, "SterHitters'23")
write.csv(ster_pitchers_23, "SterPitchers'23")

                                                                                                              