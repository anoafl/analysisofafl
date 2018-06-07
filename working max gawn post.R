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
  filter(Season==2018)%>%
  group_by(Team)%>%
  summarise(team.average=mean(Total))

df2<-df1%>%
  filter(Season==2018)%>%
  group_by(Team)%>%
  summarise(team.average=mean(Total))


var(scale(df2$team.average))
mean(scale(df2$team.average))
cbind(df2$Team, scale(df2$team.average))
library(tidyverse)

###comparing Max Gawn via dumbell/cleveland plots
df<-df%>%
  filter(Season != 2018) #filters out the 2018 data (incomeplete that was downloaded when installing fitzRoy for first time) 
df1<-fitzRoy::get_footywire_stats(9514:9611) #(end round 11)


df2<-rbind(df, df1) #stacks the datasets on top of each other

###Step one, Get out Max Gawn for 2016 and Max Gawn for 2018

df3<-df2%>%filter(Season %in% c(2016,2018))
df3<-df2%>%
  filter(Season %in% c(2016,2018))%>%
  filter(Player =="Max Gawn") %>%
  group_by(Season)%>%
  summarise(ave.ho=mean(HO),
            ave.CM=mean(CM),
            ave.SC=mean(SC),
            ave.MG=mean(MG),
            ave.ITC=mean(ITC), 
            ave.AF=mean(AF),
            ave.SC=mean(SC),
            ave.Mi5=mean(MI5))





names(df3)
df4<-gather(df3,variables, values, -Season)   
names(df4)
df4%>%   
  ggplot(aes(x=values, y=variables)) +
  geom_point(aes(colour=as.factor(df4$Season)))


df1%>%filter(Opposition=="Western Bulldogs")%>%
  filter(HO>0)%>%ggplot(aes(x=Date, y=SC))+
  geom_point()+
  geom_text(aes(label=paste(Player,"" ,HO)))


df1%>%filter(Player %in% c("Brodie Grundy","Max Gawn"))%>%
  ggplot(aes(x=Opposition, y=SC,colour=Player))+
  geom_point()+
  geom_text(aes(label=paste(Player," ",HO)))
