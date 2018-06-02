########-------######

library(tidyverse)

df<-fitzRoy::get_footywire_stats(9514:9602)
df1<-fitzRoy::player_stats
df3<-rbind(df1, df) 
# head(df3)
# df3$Date
df3%>%
  select(Player, AF, Date, Status)%>%
  filter(Player %in% c("Connor Blakely"))%>%
  ggplot(aes(x=Date, y=AF, colour=Status))+
  geom_point(aes(),size=3)+
 
  geom_segment(aes(x=Date, xend=Date,y=0, yend=AF ))+
  geom_hline(yintercept =109) +ggtitle("Conor Blakely AF scores")+
  ylab("AF Score")



 

df3%>%
  select(Player, AF, Date, Status)%>%
  filter(Player %in% c("Connor Blakely", "Jack Steele"))%>%
  ggplot(aes(x=Date, y=AF, colour=Status))+
  geom_point(aes(),size=3)+
  geom_segment(aes(x=Date, xend=Date,y=0, yend=AF ))+
  geom_hline(yintercept =109) +   
  facet_wrap(~Player) +
  ggtitle("Catchy Title Here")+
  ylab("AF Score") 

  
  


df3%>%
     select(Player, AF)%>%
     filter(Player %in% c("Connor Blakely"))%>%tally(AF>109)
df3%>%
  select(Player, AF)%>%
  filter(Player %in% c("Connor Blakely"))%>%tally() 


ggplot(aes(x=AF ,y=..scaled..))+geom_density() +
   geom_vline(xintercept = 110) 

 fitzRoy::afldata%>%
   select(First.name, Surname, Goals)%>%
   filter(Goals>6)%>%
   distinct(First.name, Surname)
 
 
 library(fitzRoy)
 tips <- get_squiggle_data("tips")
 head(tips)
library(tidyverse) 
 tips%>%
   filter(year>2017)%>%
   group_by(round, source)%>%
   summarise(MAE_by_round=mean(err))%>%
   ggplot(aes(x=round, y=MAE_by_round))+ 
   geom_point(aes(colour=source)) +
   geom_line(aes(group=source, colour=source)) +facet_wrap(~source)
 
 
 df2%>%
   select(Player, AF, Date, Status, Opposition)%>%
   filter(Player %in% c("Connor Blakely"))%>%
   ggplot(aes(x=Date, y=AF, colour=Status))+
   geom_point()+
   geom_segment(aes(x=Date, xend=Date,y=0, yend=AF ))+
   geom_hline(yintercept =109) +ggtitle("Conor Blakely AF scores")+
   ylab("AF Score") +ylim(0,150) +geom_text(aes(label=Opposition),vjust=-1)
 
 #########---##########
#####How to visualise betting data################
 library(gsheet) 
library(tidyverse)
  url<-"https://docs.google.com/spreadsheets/d/1H3od0gyEkWj3icnTn55ywQ3YEQ--6xnhFl2AHI2QFdc/edit?usp=sharing"
 afl_bookies<-read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)

 head(afl_bookies)

 afl_bookies%>%select(Date, Kick.Off..local., Home.Team, 
                      Away.Team, Venue, Home.Score, Away.Score,
                      Home.Line.Close, Away.Line.Close)%>%
   mutate(home.margin=Home.Score-Away.Score)%>%
   mutate(home.cover=home.margin+Home.Line.Close)%>%
   mutate(away.margin=Away.Score - Home.Score)%>%
   mutate(away.cover=away.margin+Away.Line.Close)

    
df<- afl_bookies%>%select(Date, Kick.Off..local., Home.Team, 
                          Away.Team, Venue, Home.Score, Away.Score,
                          Home.Line.Close, Away.Line.Close, Total.Score.Close)%>%
  mutate(home.margin=Home.Score-Away.Score)%>%
  mutate(home.cover=home.margin+Home.Line.Close)%>%
  mutate(away.margin=Away.Score - Home.Score)%>%
  mutate(away.cover=away.margin+Away.Line.Close)%>%
  mutate(total_over=Home.Score + Away.Score - Total.Score.Close)

df1<-select(df, Date,Kick.Off..local., Home.Team, 
            Home.Line.Close,home.margin,home.cover,total_over)
df1$h_a<-"home"

df2<-select(df, Date,Kick.Off..local., Away.Team, 
            Away.Line.Close,away.margin,away.cover,total_over)
df2$h_a<-"away"

colnames(df1)[3] <- "Team"
colnames(df1)[4] <- "Line.Close"
colnames(df1)[5] <- "margin"
colnames(df1)[6] <- "cover"

colnames(df2)[3] <- "Team"
colnames(df2)[4] <- "Line.Close"
colnames(df2)[5] <- "margin"
colnames(df2)[6] <- "cover"

df3<-rbind(df1,df2)
df3$Date<-dmy(df3$Date)
str(df3$Date)
df3%>%arrange(Date)%>%
  filter(Date>"2016-01-09")%>%
  filter(Team=="Sydney")%>%
mutate(games_number=row_number())%>%
  ggplot(aes(y=total_over, x=Date,fill=h_a))+geom_col() +
  ggtitle("Swans totals") +geom_text(aes(label=cover),vjust=-.5, size=2.5)+ 
  theme_tufte()
 

df3%>%arrange(Date)%>%
  mutate(games_number=row_number())%>%
  ggplot(aes(y=cover, x=games_number,fill=h_a))+geom_col() 


#############



library(fitzRoy)
library(lubridate)
library(tidyverse)
library(ggthemes)
tips <- get_squiggle_data("tips")

df<-tips%>%mutate(home.margin=ifelse(hteam==tip, margin,-margin))%>%
  mutate(away.margin=ifelse(ateam==tip, margin,-margin)) %>%
  select(source,date,correct,  hconfidence,hteam,
         ateam,home.margin,away.margin,err ,tip,round, year, venue)%>%
  mutate(covered=ifelse(correct==1, err,-err))

df1<-select(df,source, date, correct, hconfidence,hteam, home.margin, err, tip, round, year, covered ,venue)
df1$H_A<-"HOME"
df2<-select(df, source, date, correct, hconfidence, ateam, away.margin, err, tip, round, year, covered, venue)
df2$H_A<-"AWAY"
colnames(df1)[5]<-"TEAM"

colnames(df1)[6] <- "margin"

colnames(df2)[5]<-"TEAM"

colnames(df2)[6]<-"margin"

df3<-rbind(df1,df2)
str(df3$date)

df3$date<-ymd_hms(df3$date)


df3%>%arrange(date)%>%
  filter(date>"2018-01-09")%>%
  filter(round<11)%>%
  filter(TEAM %in% c("Essendon"))%>%
  ggplot(aes(y=covered, x=date,fill=venue))+geom_col() +
  ggtitle("Essendon")   +
  theme_economist_white() +
  theme(plot.title  = element_text(size =12),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 12))+
  facet_wrap(~source)



df3%>%arrange(date)%>%
  filter(date>"2018-01-09")%>%
  filter(round<11)%>%
  filter(TEAM=="Essendon")%>%
  ggplot(aes(y=margin, x=date,fill=H_A))+geom_col() +
  ggtitle("Essendon")   +
  theme_economist_white() +
  theme(plot.title  = element_text(size =12),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 12))+
  facet_wrap(~source)

df<-fitzRoy::get_match_results()
df %>% 
  group_by(Round, Season) %>% 
  filter(which.min(Margin)==row_number())  %>%
  arrange(desc(Margin))
View(
     df %>% 
       group_by(Round, Season) %>% 
       summarise(average_margin=mean(abs(Margin))%>%
       filter(row_number() == 1) %>%
       arrange((average_margin)))
)


df<-fitzRoy::afldata

df%>%
  select(Attendance,Date ,Venue, Away.team)%>%
  filter(Away.team %in% c ("West Coast", "Adelaide"))%>%
  filter(Venue=="M.C.G.") %>% distinct()%>%
  ggplot(aes(x=Date, y=Attendance))+geom_line(aes(colour=Away.team))+
  facet_wrap(~Away.team)
  

df%>%
  select(Attendance,Season ,Venue, Away.team)%>%
  filter(Away.team %in% c ("West Coast", "Adelaide"))%>%
  filter(Venue=="M.C.G.") %>% distinct()%>%
  group_by(Season, Away.team)%>%summarise(average=mean(Attendance))%>%
  ggplot(aes(x=Season, y=average))+geom_col(aes(colour=Away.team)) + facet_wrap(~Away.team)


library(fitzRoy)
library(tidyverse)

df<-fitzRoy::player_stats

df1<-fitzRoy::get_footywire_stats(9514:9593)
df<-df%>%filter(Season != 2018)
df2<-rbind(df, df1)
df2%>%
  select(Season, ITC, CM, Player)%>%
  group_by(Player, Season)%>%
  filter(Season>2015)%>%
  summarise(ITC_T=sum(ITC), CM_T=sum(CM))%>%
  mutate(flag=ifelse(Player %in% c("Jeremy McGovern", "Alex Rance"), T,F))%>%
  ggplot(aes(x=ITC_T, y=CM_T))+ geom_point(aes(colour=flag), show.legend = FALSE)+ geom_text(aes(label=Player)
  # geom_text(aes(label=ifelse(Player %in% c("Jeremy McGovern", "Alex Rance"), 
                             # Player, ""),vjust=-1))+facet_wrap(~Season)
) +facet_wrap(~Season)


library(fitzRoy)
library(tidyverse)

df<-fitzRoy::player_stats

df1<-fitzRoy::get_footywire_stats(9514:9594)

df<-df%>%filter(Season != 2018)
df2<-rbind(df, df1)

  df2%>%
    select(Season, Round, K,HB, Team, Opposition, Date ) %>%
    group_by(Season, Round, Team, Opposition, Date)%>%
    summarise(tk=sum(K), thb=sum(HB)) %>%
    mutate(hb2k=thb/tk) %>%
    filter(Season==2018)%>%
    filter(Opposition=="West Coast")%>%
    ggplot(aes(x=Date, y=hb2k)) +geom_point() +
     geom_text(aes(label=Team), size=2) + 
    geom_hline(yintercept=0.753)
    
  
  facet_wrap(~Opposition)


    
    df3<-df2%>%
      select(Season, Round, K,HB, Team, Opposition, Date ) %>%
      group_by(Season, Round, Team, Opposition, Date)%>%
      summarise(tk=sum(K), thb=sum(HB)) %>%
      mutate(hb2k=thb/tk) %>%
      filter(Season==2018)%>%
      filter(Opposition == "West Coast")       
    
    
        
    df2%>%
      select(Season, Round, K,HB, Team, Opposition, Date ) %>%
      group_by(Season, Round, Team, Opposition, Date)%>%
      summarise(tk=sum(K), thb=sum(HB)) %>%
      mutate(hb2k=thb/tk) %>%
      filter(Season==2018)%>%
      filter(Opposition != "West Coast") %>%
      ggplot(aes(x=Round, y=hb2k))+geom_point(data=df3)+
      geom_point(aes(colour="red"))

df3<-df2%>%
    select(Season, Round, K,HB, Team, Opposition, Date ) %>%
    group_by(Season, Round, Team, Opposition, Date)%>%
    summarise(tk=sum(K), thb=sum(HB)) %>%
    mutate(hb2k=thb/tk) %>%
    filter(Season==2018)%>%
    filter(Opposition == "West Coast")       


#######################

library(fitzRoy)
library(tidyverse)

df<-fitzRoy::player_stats

df1<-fitzRoy::get_footywire_stats(9514:9594)

df<-df%>%filter(Season != 2018)
df<-df%>%
  filter(Season != 2018) #filters out the 2018 data (incomeplete that was downloaded when installing fitzRoy for first time) 
df2<-rbind(df, df1) #stacks the datasets on top of each other

df2%>%
  select(Season, Round, K,HB, Team, Opposition, Date ) %>%
  group_by(Season, Round, Team, Opposition, Date)%>%
  summarise(tk=sum(K), thb=sum(HB)) %>%
  mutate(hb2k=thb/tk) %>%
  group_by(Season, Opposition) %>%
  summarise(average_hb2k=mean(hb2k)) %>%
  filter(Season==2018)


df2%>%
  select(Season, Round, K,HB, Team, Opposition, Date ) %>%
  group_by(Season, Round, Team, Opposition, Date)%>%
  summarise(tk=sum(K), thb=sum(HB)) %>%
  mutate(hb2k=thb/tk) %>%
  filter(Season==2018)%>%
  filter(Team=="West Coast")%>%
  ggplot(aes(x=Date, y=hb2k)) +geom_point() +
  geom_text(aes(label=Opposition), size=2) + 
  geom_hline(yintercept=0.753)


df2%>%
  select(Season, Round, K,HB, Team, Opposition, Date ) %>%
  group_by(Season, Round, Team, Opposition, Date)%>%
  summarise(tk=sum(K), thb=sum(HB)) %>%
  mutate(hb2k=thb/tk) %>%
  group_by(Season, Team) %>%
  summarise(average_hb2k=mean(hb2k)) %>%
  filter(Season==2018)

df2%>%
  select(Season, Round, K,HB, Team, Opposition, Date ) %>%
  group_by(Season, Round, Team, Opposition, Date)%>%
  summarise(tk=sum(K), thb=sum(HB)) %>%
  mutate(hb2k=thb/tk) %>%
  filter(Season==2018)%>%
  filter(Team=="Collingwood")%>%
  ggplot(aes(x=Date, y=hb2k)) +geom_point() +
  geom_text(aes(label=hb2k), size=2) + 
  geom_hline(yintercept=0.829)


library(fitzRoy)
df<-fitzRoy::get_match_results()

 home<-select(df, Home.Team, Home.Points,Season)
 away<-select(df, Away.Team, Away.Points,Season)
 
 colnames(home)[colnames(home) == 'Home.Points'] <- 'Total'
  colnames(home)[colnames(home) == 'Home.Team'] <- 'Team'
  colnames(away)[colnames(away) == 'Away.Team'] <- 'Team'
  colnames(away)[colnames(away) == 'Away.Team'] <- 'Team'
  colnames(away)[colnames(away) == 'Away.Points'] <- 'Total'
df1<-rbind(home,away)
df1%>%
  filter(Season>2015)%>%
  group_by(Team, Season)%>%
summarise(team.average=mean(Total))

df2<-df1%>%
  filter(Season>1999)%>%
  group_by(Team,Season)%>%
  summarise(team.average=mean(Total))


var(scale(df2$team.average))
mean(scale(df2$team.average))
df3<-cbind(df2$Team,df2$Season, scale(df2$team.average))
View(df3)
