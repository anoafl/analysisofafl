library(data.table)
library(devtools)
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(stringr)
library(tidyr)

source_url("https://raw.githubusercontent.com/DataByJosh/AFL-Data/main/AFLM_Match_Chains/Scraper.R")
data <- get_match_chains(2021)

write.csv(data, "chains_2021.csv")


data%>%
  mutate(match_id=paste(roundNumber, homeTeam.teamName,awayTeam.teamName))%>%
  group_by(match_id, chain_number)%>%
  filter(any(playerName.givenName=="Jake" & playerName.surname=="Lever"))%>%
select(match_id, chain_number, initialState, finalState, 
       playerName.givenName, playerName.surname,x,y)%>%
  group_by(chain_number)%>%
  slice(n())%>%View()

data%>%
  mutate(match_id=paste(roundNumber, homeTeam.teamName,awayTeam.teamName))%>%
  group_by(match_id, chain_number)%>%
  filter(any(playerName.givenName=="Caleb" & playerName.surname=="Daniel"))%>%
select(match_id, chain_number, initialState, finalState, 
       playerName.givenName, playerName.surname,description,x,y)%>%
  group_by(chain_number)%>%
    filter(match_id=="19 Melbourne Western Bulldogs")%>%
  ungroup()%>%
  select(initialState, finalState, chain_number)%>%
  distinct()%>%
  group_by(finalState)%>%
  summarise(count=n())
  
  ggplot(aes(x=initialState, y=finalState))+geom_point()

  
   data%>%
  mutate(match_id=paste(roundNumber, homeTeam.teamName,awayTeam.teamName))%>%
    select(match_id, chain_number)%>%distinct()%>%
    group_by(match_id)%>%
    slice(n())%>%
    ggplot(aes(x=chain_number))+geom_histogram(binwidth = 10)
   
   ## bulldogs played  dees in round 19 and 11 (11 dees win)
   
      data%>%
      mutate(match_id=paste(roundNumber, homeTeam.teamName,awayTeam.teamName))%>%
      filter(match_id %in% c("19 Melbourne Western Bulldogs",
                           "11 Western Bulldogs Melbourne"))%>%    
      select(match_id, chain_number, team.teamName, 
             playerName.givenName,playerName.surname, description)%>%
       
       data%>%
      mutate(match_id=paste(roundNumber, homeTeam.teamName,awayTeam.teamName))%>%
      filter(match_id %in% c("19 Melbourne Western Bulldogs",
                           "11 Western Bulldogs Melbourne"))%>%    
      select(match_id, chain_number, team.teamName, 
             playerName.givenName,playerName.surname, description,finalState)%>%
    filter(lag(playerName.surname=="Bontempelli"& description=="Kick" ))%>%
       group_by(playerName.surname, description,match_id)%>%
         summarise(count=n())

   data%>%
      mutate(match_id=paste(roundNumber, homeTeam.teamName,awayTeam.teamName))%>%
      filter(match_id %in% c("19 Melbourne Western Bulldogs",
                           "11 Western Bulldogs Melbourne"))%>%    
      select(match_id, chain_number, team.teamName, 
             playerName.givenName,playerName.surname, description)
 
      names(data) <- snakecase::to_snake_case(names(data))
     
     chain_clean <- data %>% 
    rename(
        playing_for = team_team_name,
        home_team = home_team_team_name,
        away_team = away_team_team_name,
        home_team_score = home_team_score_total_score,
        away_team_score = away_team_score_total_score,
        home_team_direction = home_team_direction_qtr_1
    ) %>% 
    mutate(
        h_a = if_else(playing_for == home_team, 'H', 'A')
    )

library(ggplot2)
library(ggforce)
     chain_clean <- data %>% 
    rename(
        playing_for = team_team_name,
        home_team = home_team_team_name,
        away_team = away_team_team_name,
        home_team_score = home_team_score_total_score,
        away_team_score = away_team_score_total_score,
        home_team_direction = home_team_direction_qtr_1
    ) %>% 
    mutate(
        h_a = if_else(playing_for == home_team, 'H', 'A')
    )     
     
 chain_clean%>%
      mutate(match_id=paste(round_number, home_team,away_team))%>%
      filter(match_id %in% c("19 Melbourne Western Bulldogs",
                           "11 Western Bulldogs Melbourne"))%>%    
      select(match_id, chain_number, playing_for, 
             player_name_given_name,player_name_surname, 
             description,final_state, x, y)%>%
    filter(lag(player_name_surname=="Bontempelli"& description=="Kick" ))%>%
     ggplot(aes(x,y, group = chain_number)) +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 75, b = 65, angle = 0), fill = '#398c0a', alpha = 0.1) +
    geom_point() +
    geom_path() +
    geom_segment(aes(x = 50, y = 60, xend = 75, yend = 60), arrow = arrow()) +
    ggtitle('Every PA rd 1 2021') +
    coord_fixed() + ylab('') + xlab()
 
 chain_clean%>%
    mutate(match_id=paste(round_number, home_team,away_team))%>%
   filter(match_id=="19 Melbourne Western Bulldogs"& chain_number==266)%>%View()
     
     
ggplot(chain_clean %>% 
        filter(playing_for == 'Western Bulldogs'), aes(x,y, group = chain_number)) +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 75, b = 65, angle = 0), fill = '#398c0a', alpha = 0.1) +
    geom_point() +
    geom_path() +
    geom_segment(aes(x = 50, y = 60, xend = 75, yend = 60), arrow = arrow()) +
    ggtitle('Every PA rd 1 2021') +
    coord_fixed() + ylab('') + xlab('')   

 chain_clean%>%
      mutate(match_id=paste(round_number, home_team,away_team))%>%
      filter(match_id %in% c("19 Melbourne Western Bulldogs",
                           "11 Western Bulldogs Melbourne"))%>%    
      select(match_id, chain_number, playing_for, 
             player_name_given_name,player_name_surname, 
             description,final_state, x, y)%>%
    filter(lag(player_name_surname=="Bontempelli"& description=="Kick" ))%>%
     ggplot(aes(x,y, group = chain_number)) +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 75, b = 65, angle = 0),
                 fill = '#398c0a', alpha = 0.1) +
    geom_point() +
    geom_path(aes(col=description)) +facet_wrap(~match_id)
   