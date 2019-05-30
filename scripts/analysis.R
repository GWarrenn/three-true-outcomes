library(tidyverse)
library(reshape2)

#####################################################
##
## Load in Inning Level Data
##
#####################################################

inning_data <- read.csv("C:/Users/august.warren/Downloads/retrosheet/agg-inning-results.csv")

## filtering to regulation length games 

game_length <- inning_data %>%
  group_by(game_id) %>%
  summarise(max_inning = max(inning))

inning_data <- merge(inning_data,game_length,by = "game_id")

inning_data <- inning_data %>% filter(max_inning == 9)

## add in average inning data for reference

avg_inning <- inning_data %>%
  group_by(inning) %>%
  summarise(true_outcome_result = mean(true_outcome_result))

avg_inning$game_id <- "Average Innings"

#####################################################
##
## Average Inning-Level Results by Team (& Overall)
##
## For example, how much is each bet making per inning
## across the season?
##
#####################################################

inning_data_team <- inning_data %>%
  select(inning,true_outcome_result,game_id) %>%
  filter(grepl('DET',game_id))

avg_data_team <- inning_data %>%
  select(value,inning,true_outcome_result,game_id) %>%
  group_by(value,inning) %>%
  summarise(true_outcome_result = mean(true_outcome_result))

ggplot() +
  geom_point(data=avg_data_team,aes(x=inning,y=true_outcome_result,group=value,color=value),alpha=1,size=3) +
  geom_line(data=avg_inning,aes(x=inning,y=true_outcome_result),alpha=1,size=3,color="red") +
  geom_hline(yintercept = 0,size = 2) +
  labs(x="Inning",
       y="Three True Outcomes Winnings",
       title='Average Winnings by Inning')

#####################################################
##
## Average Cumulative Winnings by Inning
##
## This tracks the average outcome of the betting process
## for all games on average.
##
#####################################################

cumulative_data <- inning_data %>%
  select(game_id,inning,true_outcome_result,value,var)

cumulative_data_1 <- dcast(cumulative_data,game_id + inning ~ var, value.var = "value")

cumulative_data_2 <- dcast(cumulative_data,game_id + inning ~ var, value.var = "true_outcome_result") %>%
  rename(home_total_outcome = hometeam,
         visit_total_outcome = visteam)

cumulative_data <- merge(cumulative_data_1,cumulative_data_2,by=c("game_id","inning"))

cumulative_data$home_total_outcome <- ifelse(is.na(cumulative_data$home_total_outcome),0,cumulative_data$home_total_outcome)
cumulative_data$visit_total_outcome <- ifelse(is.na(cumulative_data$visit_total_outcome),0,cumulative_data$visit_total_outcome)

cumulative_data$true_outcome_result <- cumulative_data$home_total_outcome + cumulative_data$visit_total_outcome

cumulative_data <- cumulative_data %>%
    group_by(game_id) %>%
    arrange(game_id,inning) %>%
    mutate(cumulative_outcome = cumsum(true_outcome_result))

cumulative_inning <- cumulative_data %>%
  group_by(inning) %>%
  summarise(avg_winnings = mean(cumulative_outcome))

cumulative_inning_all <- cumulative_data %>%
  group_by(game_id,inning) %>%
  summarise(avg_winnings = mean(cumulative_outcome))

ggplot() +
  geom_line(data=cumulative_inning,aes(x=inning,y=avg_winnings),alpha=1,size=3,color="red") +
  geom_hline(yintercept = 0,size =2) +
  labs(x="Inning",
       y="Three True Outcomes Winnings",
       title='Seventh Inning Rebound',
       subtitle ='Average Cumulative Winnings by Inning')

#####################################################
##
## Compare two teams on Average -- Controlling for Home Field Advantage
##
#####################################################

teams <- c("WAS","PHI")

comparison_1 <- cumulative_data %>%
  filter(hometeam == teams[1] & visteam == teams[2]) %>%
  group_by(inning) %>%
  summarise(avg_winnings = mean(cumulative_outcome))

comparison_1$label <- paste("Home Team:",teams[1])

comparison_2 <- cumulative_data %>%
  filter(hometeam == teams[2] & visteam == teams[1]) %>%
  group_by(inning) %>%
  summarise(avg_winnings = mean(cumulative_outcome))

comparison_2$label <- paste("Home Team:",teams[2])

comparison <- rbind(comparison_1,comparison_2)

ann_text <- data.frame(inning = 1,avg_winnings = 5,lab = "True Outcome Wins Money",
                       label = "Home Team: PHI")

ann_text_2 <- data.frame(inning = 1,avg_winnings =-5,lab = "Anything Else Wins Money",
                       label = "Home Team: PHI")

plot <- ggplot() +
  geom_line(data=comparison,aes(x=inning,y=avg_winnings),alpha=1,size=3,color="red") +
  facet_wrap(~label) +
  geom_text(data = ann_text,aes(x=inning,y=avg_winnings,label = lab),angle = -90) +
  geom_text(data = ann_text_2,aes(x=inning,y=avg_winnings,label = lab),angle = -90) +
  geom_hline(yintercept = 0,size =2) +
  labs(x="Inning",
       y="Three True Outcomes Winnings",
       title=paste(teams[1],"vs.",teams[2]),
       subtitle ='Average Cumulative Winnings by Home Field Advantage & Inning')

ggsave(plot = plot, "nats_phillies.png", w = 10.67, h = 8,type = "cairo-png")

#####################################################
##
## Average Winnings by Days into Season
##
#####################################################

totals <- read.csv("C:/Users/august.warren/Downloads/retrosheet/agg-results.csv")

by_days <- totals %>%
  group_by(days_since_season_start) %>%
  summarise(avg_winnings = mean(true_outcome_result))

by_days$winner <- ifelse(by_days$avg_winnings > 0,"True Outcome",ifelse(by_days$avg_winnings < 0,"Anything Else","Tie"))

by_days$days_since_season_start <- as.numeric(gsub(pattern = "days.*",replacement = "",x = by_days$days_since_season_start))

ggplot() +
  geom_point(data=by_days,aes(x=days_since_season_start,y=avg_winnings,color=winner),alpha=1,size=3) +
  geom_smooth(data=by_days,aes(x=days_since_season_start,y=avg_winnings),size=2) +
  geom_hline(yintercept = 0,size =2) +
  labs(x="Days Since Start of Season",
       y="Three True Outcomes Winnings",
       title='Average Winnings by Days since Start of Season')


#####################################################
##
## Running Count of Wins by Bet Choices
##
#####################################################

totals$winner <- ifelse(totals$true_outcome_result == 0,"Tie",ifelse(totals$true_outcome_result > 0,"Three True Outcomes","Anything Else"))

winners <- totals %>%
  group_by(winner,fmt_date) %>%
  summarise(count = n()) %>%
  mutate(cumsum = cumsum(count)) %>%
  group_by(fmt_date) %>%
  mutate(tot = sum(cumsum),
         percent = cumsum / tot)
  
winner_totals <- winners %>%  
  group_by(fmt_date) %>%
  summarise(tot = sum(cumsum))

ggplot() +
  geom_line(data=winners,aes(x=as.Date(fmt_date),y=cumsum,group=winner,color=winner),alpha=1,size=3)

ggplot() +
  geom_line(data=winners,aes(x=as.Date(fmt_date),y=percent,group=winner,color=winner),alpha=1,size=3)

#####################################################
##
## match-ups: which teams netted the most money for each outcome
##
#####################################################

parity_data <- cumulative_data %>%
  group_by(game_id,hometeam,visteam) %>%
  summarise(winnings = sum(true_outcome_result)) %>%
  group_by(game_id) %>%
  mutate(hometeam_adj = lag(hometeam))

parity_data$hometeam <- ifelse(is.na(parity_data$hometeam),parity_data$hometeam_adj,parity_data$hometeam)

parity_data <- parity_data %>%
  group_by(game_id,hometeam,visteam) %>%
  summarise(winnings = sum(winnings))

parity_data$match_up <- paste(parity_data$hometeam,"-",parity_data$visteam)

match_ups <- parity_data %>%
  group_by(hometeam,visteam,match_up) %>%
  summarise(avg_winnings = mean(winnings))

#####################################################
##
## Impact of Team Parity on Outcomes 
##
#####################################################

info <- read.csv("C:/Users/august.warren/Documents/GitLab/three-true-outcomes/data/info.csv")

info <- info %>%
  filter(var %in% c("hometeam","visteam","date","hometeam_score","awayteam_score"))

info_w <- dcast(data = info,game_id ~ var, value.var = "value")

info_w$home_win <- ifelse(info_w$hometeam_score > info_w$awayteam_score,1,0)
info_w$away_win <- ifelse(info_w$hometeam_score < info_w$awayteam_score,1,0)

parity_w_win_loss <- merge(parity_data,info_w,by="game_id")

parity_w_win_loss <- parity_w_win_loss %>%
  group_by(hometeam.x) %>%
  mutate(wl_record_home = reduce(map(0:30,~lag(home_win,.,0)),`+`),
         wl_record_away = reduce(map(0:30,~lag(away_win,.,0)),`+`)) %>%
  ungroup()

parity_w_win_loss$home_win_pct <- parity_w_win_loss$wl_record_home / 30
parity_w_win_loss$away_win_pct <- parity_w_win_loss$wl_record_away / 30

parity_w_win_loss$parity <- parity_w_win_loss$home_win_pct - parity_w_win_loss$away_win_pct

parity_w_win_loss$winner <- ifelse(parity_w_win_loss$winnings > 0,"True Outcome",ifelse(parity_w_win_loss$winnings < 0,"Anything Else","Tie"))

ggplot() +
  geom_point(data=parity_w_win_loss,aes(x=parity,y=winnings,color=winner),size=4,alpha=.6) +
  geom_smooth(data=parity_w_win_loss,aes(x=parity,y=winnings),size=2,method = "glm") +
  geom_hline(yintercept = 0,size =2) +
  geom_vline(xintercept = 0,size =2) +
  labs(x="Team Parity",
       y="Three True Outcomes Winnings",
       title='Average Winnings by Team Parity')

#####################################################
##
## Players
##
#####################################################

## MVPitchers

## MVBatters

batters <- read.csv("C:/Users/august.warren/Documents/GitLab/three-true-outcomes/data/batting.csv")

batters$outcome <- ifelse(batters$stat=="K" | batters$stat=="HR" | batters$stat=="W" | batters$stat=="HBP",2,-1)

## bring in batter position on field

positions <- read.csv("C:/Users/august.warren/Documents/GitLab/three-true-outcomes/data/rosters.csv")

batters <- merge(batters,positions,by="player_id")

## fixing typos in data

batters$position <- ifelse(as.character(batters$position) == "LF","OF",as.character(batters$position))
batters$position <- ifelse(as.character(batters$position) == "C ","C",as.character(batters$position))
batters$position <- ifelse(as.character(batters$position) == "P ","P",as.character(batters$position))

batter_sum <- batters %>%
  group_by(game_id,player_id,position) %>%
  summarise(winnings = sum(outcome)) %>%
  group_by(player_id,position) %>%
  summarise(mean_per_game = mean(winnings))

ggplot() +
  geom_density(data=batter_sum,aes(x=mean_per_game))

batter_sum_avgs <- batter_sum %>%
  group_by(position) %>%
  summarise(mean_per_position = mean(mean_per_game))

ggplot() +
  geom_density(data=batter_sum,aes(x=mean_per_game)) +
  geom_vline(data=batter_sum %>%
               group_by(position) %>%
               summarise(mean_per_position = mean(mean_per_game)),aes(xintercept=mean_per_position),size=1) +
  facet_wrap(~position) +
  labs(x="Three True Outcomes Winnings",
       y="Density",
       title='Average Winnings per Game by Batter Positions')

batters$strikeouts <- ifelse(batters$stat == "K",1,0)
batters$homeruns_walks <- ifelse(batters$stat == "HR" | batters$stat == "HBP" | batters$stat == "W",1,0)

hrs_ks <- batters %>%
  group_by(position) %>%
  summarise(hrs = sum(homeruns_walks),
            strikeouts = sum(strikeouts),
            num_games_played = n())

hrs_ks$adj_hrs <- (hrs_ks$hrs / hrs_ks$num_games_played) * 10
hrs_ks$adj_ks <- (hrs_ks$strikeouts / hrs_ks$num_games_played) * 10

ggplot() +
  geom_text(data=hrs_ks,aes(x=adj_ks,y=adj_hrs,label=position)) +
  geom_abline(slope = 1,intercept = 0) + 
  scale_y_continuous(limits = c(0,6)) + 
  scale_x_continuous(limits = c(0,6))
