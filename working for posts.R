
library(fitzRoy)
library(tidyverse)

df<-fitzRoy::player_stats

df1<-fitzRoy::get_footywire_stats(9514:9611)

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




library(fitzRoy)
library(tidyverse)

df<-fitzRoy::player_stats

df1<-fitzRoy::get_footywire_stats(9514:9611)

df<-df%>%filter(Season != 2018)
df<-df%>%
  filter(Season != 2018) #filters out the 2018 data (incomeplete that was downloaded when installing fitzRoy for first time) 
df2<-rbind(df, df1) #stacks the datasets on top of each other

df2%>%
  filter(Player=="Max Gawn")%>%
  filter(Season %in% c(2016,2018))%>%
  group_by(Season)%>%
  summarise(aveHO=mean(HO),
            aveCP=mean(CP),
            aveCM=mean(CM),
            aveSI=mean(SI),
            aveITC=mean(ITC),
            aveM=mean(M)
            ) %>%
  ggplot(aes(x=aveHO,y=Season))+geom_dotplot()

df46 <-  swiss %>% rownames_to_column("Province")

library(tidyverse)
df3<-df2%>%
  filter(Season %in% c(2017,2018))%>%
  filter(Player =="Dustin Martin") %>%
group_by(Season)%>%
  summarise(ave.ho=mean(HO),
            ave.CM=mean(CM),
            ave.SC=mean(SC),
            ave.MG=mean(MG),
            ave.ITC=mean(ITC), 
            ave.AF=mean(AF),
            ave.CP=mean(CP),
            ave.Mi5=mean(MI5))
df4<-gather(df3,variables, values, -Season)   
df4%>%   
  ggplot(aes(x=as.factor(df4$Season), y=values)) +
  geom_point()+
  facet_wrap(~variables,scales = "free")
