########-------######

library(tidyverse)

df<-fitzRoy::get_footywire_stats(9514:9585)
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
 url<-"https://docs.google.com/spreadsheets/d/1H3od0gyEkWj3icnTn55ywQ3YEQ--6xnhFl2AHI2QFdc/edit?usp=sharing"
 afl_bookies<-read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)
View(afl_bookies) 
 
 