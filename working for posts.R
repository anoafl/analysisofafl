
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

#what this table gives us is for each team, 
#when they are the opposition what was their opponents kick to handball ratio vs them



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


df1%>%filter(Opposition=="Western Bulldogs")%>%
  filter(HO>0)%>%ggplot(aes(x=Date, y=SC))+
  geom_point()+
  geom_text(aes(label=paste(Player,"" ,HO)))
