library(rvest)
library(tidyverse)
library(stringr)

url<-"https://www.footywire.com/afl/footy/ft_players"


link<-read_html(url)%>%
  html_nodes("br+ a , .lnormtop a:nth-child(1)")%>%
  html_attr("href")

url_players<-str_c("https://www.footywire.com/afl/footy/",link)

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

player_info <- function(x){
  # page <- read_html(x)
  page<-read_html(x)  
  player<-       page%>%
    html_nodes(".ldrow .hltitle")%>%
    html_text() %>% as.tibble()
  playing.for<-  page%>%
    html_nodes(".ldrow a b")%>%
    html_text() %>% as.tibble()
  number<-     page%>%
    html_nodes(".ldrow > b")%>%
    html_text() %>% as.tibble()
  
  weight<-page%>%
    html_nodes("form tr:nth-child(4) .ldrow")%>%
    html_text()%>%
    str_replace_all("[\r\n]" , "")%>%
    str_squish()%>%
    str_extract(pattern =("(?<=Weight:).*(?=Position:)"))%>%as.tibble()
  
  height<-page%>%
    html_nodes("form tr:nth-child(4) .ldrow")%>%
    html_text()%>%
    str_replace_all("[\r\n]" , "")%>%
    str_squish()%>%
    str_extract(pattern =("(?<=Height:).*(?=Weight:)"))%>%as.tibble()
  
  draft_position <- page%>%
    html_nodes("tr:nth-child(5) .ldrow")%>%
    html_text()%>%
    str_replace_all("[\r\n]" , "")%>%
    str_squish()%>%
    str_extract(pattern =("(?<=Drafted: ).*(?=by)"))%>%as.tibble()
  
  club_drafted <- page%>%
    html_nodes("tr:nth-child(5) .ldrow")%>%
    html_text()%>%str_replace_all("[\r\n]" , "")%>%
    str_squish()%>%
    str_remove(".*by") %>% as.tibble()
  position <-     page%>%
    html_nodes("form tr:nth-child(4) .ldrow")%>%
    html_text()%>%
    str_replace_all("[\r\n]" , "")%>%
    str_remove(".*Position: ")%>%
    str_squish() %>% as.tibble()
  
  
  
  
  #combine, name, and make it a tibble
  player_information <- cbind.fill(player, playing.for, number, weight, height,draft_position, club_drafted, position)
  
  player_information <- as.tibble(player_information)
  
  # print(x)
  # return(x)
  return(player_information)
}
footywire <- purrr::map_df(url_players, player_info)

names(footywire) <- c("player", "club", "number","weight","height",  "draft_position", "club_drafted", "position")
head(footywire)

# need to change the club names so it maches the fitzRoy dataset

footywire <- footywire%>%
  mutate(club=replace(club, club=="Richmond Tigers", "Richmond") )%>%
  mutate(club=replace(club, club=="Geelong Cats", "Geelong"))%>%
  mutate(club=replace(club, club== "St Kilda Saints", "St Kilda"))%>%
  mutate(club=replace(club, club=="Brisbane Lions", "Brisbane"))%>%
  mutate(club=replace(club, club=="Collingwood Magpies", "Collingwood"))%>%
  mutate(club=replace(club, club=="West Coast Eagles", "West Coast"))%>%
  mutate(club=replace(club, club=="Gold Coast Suns", "Gold Coast"))%>%
  mutate(club=replace(club, club=="North Melbourne Kangaroos", "North Melbourne"))%>%
  mutate(club=replace(club, club=="Sydney Swans", "Sydney"))%>%
  mutate(club=replace(club, club=="Essendon Bombers", "Essendon"))%>%
  mutate(club=replace(club, club=="Port Adelaide Power", "Port Adelaide"))%>%
  mutate(club=replace(club, club=="Adelaide Crows", "Adelaide"))%>%
  mutate(club=replace(club, club=="Melbourne Demons", "Melbourne"))%>%
  mutate(club=replace(club, club=="Fremantle Dockers", "Fremantle"))%>%
  mutate(club=replace(club, club=="Hawthorn Hawks", "Hawthorn"))%>%
  mutate(club=replace(club, club=="GWS Giants", "GWS"))%>%
  mutate(club=replace(club, club=="Carlton Blues", "Carlton"))%>%as.data.frame()
  
  
  df<-fitzRoy::player_stats%>%
  filter(Season==2018)


dataset<-left_join(df, footywire, by=c("Player"="player","Team"="club"))
head(dataset)

head(dataset)

View(dataset)

new_DF <- dataset[is.na(dataset$position),]

NA_Players<-unique(new_DF$Player)


## take just the players who have changed clubs

dataset<-left_join(df, footywire, by=c("Player"="player"))

new_DF <- dataset[is.na(dataset$position),]

dataset <- dataset %>%
  mutate(position = replace(position, which(is.na(position) & 
                                          Player == "Sam P-Seton"), "Midfield, Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                          Player=="Brendon Goddard"), "Defender, Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                          Player=="Cameron E-Yolmen"), "Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                          Player=="Curtly Hampton"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Koby Stevens"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Dom Barry"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Sam P-Pepper"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Danyle Pearce"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Matthew Rosa"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Billy Hartung"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Ed V-Willis"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jarrad Waite"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Luke D-Uniacke"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Cyril Rioli"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player == "Will H-Elliott"), "Midfield, Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Shane Biggs"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Mitch Honeychurch"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Bernie Vince"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Cameron Pedersen"),"Forward, Ruck"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Alex N-Bullen"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Cory Gregson"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Mark Lecras"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Dean Towers"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Cameron O'Shea"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player == "Rohan Bewick"), "Midfield, Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Nathan Wright"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jack Redpath"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Ryan Griffen"),"Midfield, Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Nicholas Graham"),"Midfield, Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Sam Gilbert"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Harrison Marsh"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player==" Sam Kerridge"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Sam Rowe"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jake Neade"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Lindsay Thomas"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="George H-Smith"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jesse Lonergan"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jackson Merrett"),"Defender, Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Daniel Robinson"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Michael Barlow"),"Midfield, Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Max Spencer"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Matthew Leuenberger"),"Ruck"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jarryd Blair"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Stewart Crameri"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Brad Scheer"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Brendan Whitecross"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Sam Gibson"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Tim Mohr"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Aaron Black"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Michael Apeness"),"Forward, Ruck"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Kyle Cheney"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jake Barrett"),"Forward"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jonathan O'Rourke"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Matt Shaw"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Jay K-Harris"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Alex Morgan"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Nathan Freeman"),"Midfield"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Alex Johnson"),"Defender"))%>%
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Brady Grey"),"Forward"))%>%
  
  mutate(position = replace(position, which(is.na(position) & 
                                              Player=="Adam Oxley"),"Defender"))
  
  
  
  
  
  
teamstats<-dataset%>%
  
  filter(position %in% c("Midfield, Forward", "Midfield","Defender, Midfield", "Ruck", "Forward, Ruck"))%>%
  group_by(Team, Round)%>%
  summarise(sumCP=sum(CP))%>%
  group_by(Team)%>%
  summarise(meanCPTeam=mean(sumCP))
 
gamesfyfenealeplayedtogether<-fitzRoy::player_stats%>%
  filter(Season==2015)%>%
  group_by(Match_id)%>%
  filter(Player %in% c("Nathan Fyfe", "Lachie Neale"))%>%
  

   

select(Match_id)%>%
  distinct()
# filter(Player %in% c("Nathan Fyfe", "Lachie Neale"))%>%
  # ggplot(aes(x=Player, y=CPPM))+geom_boxplot()



playerstats<-dataset%>%
  filter(position %in% c("Midfield, Forward", "Midfield","Defender, Midfield", "Ruck", "Forward, Ruck"))%>%
  group_by(Team, Round, Player)%>%
  summarise(sumCP=sum(CP))%>%
  group_by(Team, Player)%>%
  summarise(meanCPlayer=mean(sumCP))

df<-left_join(playerstats,teamstats , by=c("Team","Team"))%>%
  mutate(pertcontribution=meanCPlayer/meanCPTeam)
library(reldist)
  df%>%
    group_by(Team)%>%
    summarise(mygini=gini(pertcontribution))%>%
    arrange(desc(mygini))

df%>%filter(Team=="Fremantle")%>%arrange(desc(pertcontribution))%>%View()

df%>%ggplot(aes(x=meanCPTeam, y=pertcontribution))+geom_point(aes(colour=Team))




teamstats<-dataset%>%
  
  filter(position %in% c("Midfield, Forward", "Midfield","Defender, Midfield", "Ruck", "Forward, Ruck"))%>%
  library(tidyverse)

  teamstats<-fitzRoy::player_stats%>%filter(Season==2015)%>%
  
   group_by(Team, Round)%>%
  summarise(sumCP=sum(CP))%>%
  group_by(Team)%>%
  summarise(meanCPTeam=mean(sumCP))


playerstats<-fitzRoy::player_stats%>%filter(Season==2015)%>%
  group_by(Team, Round, Player)%>%
  summarise(sumCP=sum(CP))%>%
  group_by(Team, Player)%>%
  summarise(meanCPlayer=mean(sumCP))

df<-left_join(playerstats,teamstats , by=c("Team","Team"))%>%
  mutate(pertcontribution=meanCPlayer/meanCPTeam)

df%>%
  filter(Team=="Fremantle")%>%
  arrange(desc(pertcontribution)) %>%View("dockers 2015")

df%>%ggplot(aes(x=meanSClayer, y=pertcontribution))+geom_point(aes(colour=Team))



teamstats<-dataset%>%
  
  filter(position %in% c("Midfield, Forward", "Midfield","Defender, Midfield", "Ruck", "Forward, Ruck"))%>%
  group_by(Team, Round)%>%
  summarise(sumSI=sum(SI))%>%
  group_by(Team)%>%
  summarise(meanSITeam=mean(sumSI))


playerstats<-dataset%>%
  filter(position %in% c("Midfield, Forward", "Midfield","Defender, Midfield", "Ruck", "Forward, Ruck"))%>%
  group_by(Team, Round, Player)%>%
  summarise(sumSI=sum(SI))%>%
  group_by(Team, Player)%>%
  summarise(meanSIlayer=mean(sumSI))

df<-left_join(playerstats,teamstats , by=c("Team","Team"))%>%
  mutate(pertcontribution=meanSIlayer/meanSITeam)

df%>%filter(Team=="Fremantle")%>%arrange(desc(pertcontribution))%>%View()
df%>%ggplot(aes(x=meanSIlayer, y=pertcontribution))+geom_point(aes(colour=Team))
