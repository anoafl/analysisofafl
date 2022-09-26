library(fitzRoy)

df<-fitzRoy::get_fryzigg_stats(start=2010, end=2020)
library(tidyverse)
table(df$match_id, df$brownlow_votes)
unique(df$match_round)

df<-df%>%
  filter(!match_round%in% c( "Qualifying Final",  "Elimination Final",
                            "Semi Final" ,"Preliminary Final",
                            "Grand Final","Finals Week 1","Semi Finals") )


  ## explore when variables get added
  df%>%
  dplyr::select(match_id,match_date,player_id, player_first_name, player_last_name,player_team,
                match_round, kicks:spoils)%>%
  dplyr::mutate(year=lubridate::year(match_date))%>%
    ggplot(aes(x=year, y=pressure_acts))+geom_point()
  
 ## get team stats
 df_team<-  df%>%
     dplyr::select(match_id,match_date,player_id, player_first_name, player_last_name,player_team,
                match_round, kicks:spoils)%>%
    
  dplyr::mutate(year=lubridate::year(match_date))%>%
    dplyr::filter(year>2011)%>%
  dplyr::select(year, everything())%>%
    dplyr:: group_by(match_date, year, match_round, player_team)%>%
  summarise_if(is.numeric,funs(sum=c(sum(.))))
 
 ## player_stats
 
 df_player<-df%>%
  filter(!match_round%in% c( "Qualifying Final",  "Elimination Final",
                            "Semi Final" ,"Preliminary Final",
                            "Grand Final","Finals Week 1","Semi Finals") )%>%
    
  dplyr::mutate(year=lubridate::year(match_date))%>%
   dplyr::filter(year>2011)



    complete_df<-left_join(df_player,df_team, 
                           by=c("match_date"="match_date", "year"="year",  
                                "match_round"="match_round",
                                "player_team"="player_team"))
    
    naniar::vis_miss(complete_df,warn_large_data=FALSE)
    
    ## create the ratio variables
  # complete_df_ratio<-
    
complete_df_ratio<-complete_df%>%
  dplyr::mutate(kicks_ratio=kicks/kicks_sum,                                
              handballs_ratio=handballs/handballs_sum,
              disposals_ratio=disposals/disposals_sum,
              marks_ratio=marks/marks_sum,
              effective_disposals_ratio=effective_disposals/effective_disposals_sum,
              disposal_efficiency_percentage_ratio=disposal_efficiency_percentage/disposal_efficiency_percentage_sum,
              goals_ratio=goals/goals_sum,
              behinds_ratio=behinds/behinds_sum,
              hitouts_ratio=hitouts/hitouts_sum,
              tackles_ratio=tackles/tackles_sum,
              rebounds_ratio=rebounds/rebounds_sum,
              inside_fifties_ratio=inside_fifties/inside_fifties_sum,
              clearances_ratio=clearances/clearances_sum,
              clangers_ratio=clangers/clangers_sum,
              free_kicks_for_ratio=free_kicks_for/free_kicks_for_sum,
              free_kicks_against_ratio=free_kicks_against/free_kicks_against_sum,
              contested_possessions_ratio=contested_possessions/contested_possessions_sum,
              uncontested_possessions_ratio=uncontested_possessions/uncontested_possessions_sum,
              contested_marks_ratio=contested_marks/contested_marks_sum,
              marks_inside_fifty_ratio=marks_inside_fifty/marks_inside_fifty_sum,
              one_percenters_ratio=one_percenters/one_percenters_sum,
              bounces_ratio=bounces/bounces_sum,
              goal_assists_ratio=goal_assists/goal_assists_sum,
              time_on_ground_percentage_ratio=time_on_ground_percentage/time_on_ground_percentage_sum,
              afl_fantasy_score_ratio=afl_fantasy_score/afl_fantasy_score_sum,
              supercoach_score_ratio=supercoach_score/supercoach_score_sum,
              centre_clearances_ratio=centre_clearances/centre_clearances_sum,
              stoppage_clearances_ratio=stoppage_clearances/stoppage_clearances_sum,
              score_involvements_ratio=score_involvements/score_involvements_sum,
              metres_gained_ratio=metres_gained/metres_gained_sum,
              turnovers_ratio=turnovers/turnovers_sum,
              intercepts_ratio=intercepts/intercepts_sum,
              tackles_inside_fifty_ratio=tackles_inside_fifty/tackles_inside_fifty_sum,
              contest_def_losses_ratio=contest_def_losses/contest_def_losses_sum,
              contest_def_one_on_ones_ratio=contest_def_one_on_ones/contest_def_one_on_ones_sum,
              contest_off_one_on_ones_ratio=contest_off_one_on_ones/contest_off_one_on_ones_sum,
              contest_off_wins_ratio=contest_off_wins/contest_off_wins_sum,
              def_half_pressure_acts_ratio=def_half_pressure_acts/def_half_pressure_acts_sum,
              effective_kicks_ratio=effective_kicks/effective_kicks_sum,
              f50_ground_ball_gets_ratio=f50_ground_ball_gets/f50_ground_ball_gets_sum,
              ground_ball_gets_ratio=ground_ball_gets/ground_ball_gets_sum,
              hitouts_to_advantage_ratio=hitouts_to_advantage/hitouts_to_advantage_sum,
              hitout_win_percentage_ratio=hitout_win_percentage/hitout_win_percentage_sum,
              intercept_marks_ratio=intercept_marks/intercept_marks_sum,
              marks_on_lead_ratio=marks_on_lead/marks_on_lead_sum,
              pressure_acts_ratio=pressure_acts/pressure_acts_sum,
              rating_points_ratio=rating_points/rating_points_sum,
              ruck_contests_ratio=ruck_contests/ruck_contests_sum,
              score_launches_ratio=score_launches/score_launches_sum,
              shots_at_goal_ratio=shots_at_goal/shots_at_goal_sum,
              spoils_ratio=spoils/spoils_sum)     

complete_df_ratio[is.na(complete_df_ratio)] <- 0
complete_df_ratio$brownlow_votes<-as.factor(complete_df_ratio$brownlow_votes)
df_in.sample

df_working<-complete_df_ratio%>%
  dplyr::select(match_id,player_id, player_team, year, match_round, match_date, brownlow_votes, kicks_ratio:spoils_ratio)
df_in_sample<-df_working%>%
  dplyr::filter(year<2020)
names(df_working)
library(ordinal)

# can play around with stepwise here. 
fm1<-clm(brownlow_votes~ kicks_ratio+
                        # handballs_ratio+
                        disposals_ratio+
                        marks_ratio+
                        effective_disposals_ratio+
                        # disposal_efficiency_percentage_ratio+
                        goals_ratio+
                        # behinds_ratio+
                        # hitouts_ratio+
                        tackles_ratio+
                        rebounds_ratio+
                        inside_fifties_ratio+
                        # clearances_ratio+
                        # clangers_ratio+
                        # free_kicks_for_ratio+
                        # free_kicks_against_ratio+
                        contested_possessions_ratio+
                        uncontested_possessions_ratio+
                        contested_marks_ratio+
                        marks_inside_fifty_ratio+
                        # one_percenters_ratio+
                        # bounces_ratio+
                        goal_assists_ratio+
                        # time_on_ground_percentage_ratio+
                        # afl_fantasy_score_ratio+
                        supercoach_score_ratio+
                        centre_clearances_ratio+
                        stoppage_clearances_ratio+
                        score_involvements_ratio+
                        metres_gained_ratio+
                        # turnovers_ratio+
                        intercepts_ratio+
                        tackles_inside_fifty_ratio+
                        # contest_def_losses_ratio+
                        # contest_def_one_on_ones_ratio+
                        # contest_off_one_on_ones_ratio+
                        contest_off_wins_ratio+
                        def_half_pressure_acts_ratio+
                        effective_kicks_ratio+
                        f50_ground_ball_gets_ratio+
                        ground_ball_gets_ratio+
                        hitouts_to_advantage_ratio+
                        # hitout_win_percentage_ratio+
                        intercept_marks_ratio+
                        marks_on_lead_ratio+
                        pressure_acts_ratio+
                        # rating_points_ratio+
                        ruck_contests_ratio+
                        score_launches_ratio+
                        shots_at_goal_ratio+
                        spoils_ratio, 
                        data = df_in_sample)

fm2<- MASS::stepAIC(fm1, direction='backward',type=AIC)

## or just select nuff variables here

df_in_sample<-df_in_sample%>%filter(year %in% c(2017,2018,2019))
fm3<-clm(brownlow_votes~ kicks_ratio+
                        # handballs_ratio+
                        disposals_ratio+
                        # marks_ratio+
                        effective_disposals_ratio+
                        # disposal_efficiency_percentage_ratio+
                        goals_ratio+
                        # behinds_ratio+
                        # hitouts_ratio+
                        tackles_ratio+
                        rebounds_ratio+
                        inside_fifties_ratio+
                        # clearances_ratio+
                        # clangers_ratio+
                        # free_kicks_for_ratio+
                        # free_kicks_against_ratio+
                        contested_possessions_ratio+
                        uncontested_possessions_ratio+
                        contested_marks_ratio+
                        marks_inside_fifty_ratio+
                        # one_percenters_ratio+
                        # bounces_ratio+
                        goal_assists_ratio+
                        # time_on_ground_percentage_ratio+
                        # afl_fantasy_score_ratio+
                        # supercoach_score_ratio+
                        centre_clearances_ratio+
                        stoppage_clearances_ratio+
                        score_involvements_ratio+
                        # metres_gained_ratio+
                        # turnovers_ratio+
                        intercepts_ratio+
                        tackles_inside_fifty_ratio+
                        # contest_def_losses_ratio+
                        # contest_def_one_on_ones_ratio+
                        # contest_off_one_on_ones_ratio+
                        contest_off_wins_ratio+
                        # def_half_pressure_acts_ratio+
                        # effective_kicks_ratio+
                        f50_ground_ball_gets_ratio+
                        ground_ball_gets_ratio+
                        hitouts_to_advantage_ratio+
                        # hitout_win_percentage_ratio+
                        intercept_marks_ratio+
                        marks_on_lead_ratio+
                        pressure_acts_ratio+
                        # rating_points_ratio+
                        # ruck_contests_ratio+
                        # score_launches_ratio+
                        shots_at_goal_ratio+
                        spoils_ratio, 
                        data = df_in_sample)


df_out_sample<-df_working%>%
  dplyr::filter(year==2020)
df_out_sample<-df_out_sample%>%
  dplyr::select(-brownlow_votes)%>%
  dplyr::mutate(brownlow_votes=0)

newdata   <- df_out_sample[ , -ncol(df_out_sample)]



pre.dict    <- predict(fm2,newdata=newdata, type='prob')
pre.dict.m  <- data.frame(matrix(unlist(pre.dict), nrow= nrow(newdata)))
colnames(pre.dict.m) <- c("vote.0", "vote.1", "vote.2", "vote.3")
newdata.pred  <- cbind.data.frame(newdata, pre.dict.m)

#### Step 1: Get expected value on Votes
newdata.pred$expected.votes <- newdata.pred$vote.1 + 2*newdata.pred$vote.2 + 3*newdata.pred$vote.3


sum1 <- aggregate(vote.1~match_id, data = newdata.pred, FUN = sum ); names(sum1) <- c("match_id", "sum.vote.1");
sum2 <- aggregate(vote.2~match_id, data = newdata.pred, FUN = sum ); names(sum2) <- c("match_id", "sum.vote.2");
sum3 <- aggregate(vote.3~match_id, data = newdata.pred, FUN = sum ); names(sum3) <- c("match_id", "sum.vote.3");

#### Step 3: Add sum of each vote by matchId to big table
newdata.pred <- merge(newdata.pred, sum1, by = "match_id")
newdata.pred <- merge(newdata.pred, sum2, by = "match_id")
newdata.pred <- merge(newdata.pred, sum3, by = "match_id")

#### Step 4: Add std1/2/3
newdata.pred$std.1  <- (newdata.pred$sum.vote.1/newdata.pred$vote.1)^-1
newdata.pred$std.2  <- (newdata.pred$sum.vote.2/newdata.pred$vote.2)^-1
newdata.pred$std.3  <- (newdata.pred$sum.vote.3/newdata.pred$vote.3)^-1


#### Step 5: Expected standard game vote
newdata.pred$exp_std_game_vote <- newdata.pred$std.1 + 2*newdata.pred$std.2 + 3*newdata.pred$std.3  


#### Step 6: List of winners

newdata.pred$player_idd<-paste(newdata.pred$player_id," ",newdata.pred$player_team)
winners.stdgame   <- aggregate(exp_std_game_vote~player_idd, data = newdata.pred, FUN = sum );
winners.stdgame   <- winners.stdgame[order(-winners.stdgame$exp_std_game_vote), ]
winners.stdgame[1:100, ]
df%>%
  filter(player_id==11923)%>%
  select(player_first_name,player_last_name, player_id)%>%
  distinct()
  