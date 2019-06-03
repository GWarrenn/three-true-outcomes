#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(reshape2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  inning_data <- read.csv("https://raw.githubusercontent.com/GWarrenn/three-true-outcomes/master/data/agg-inning-results.csv")
  team_data <- read.csv("https://raw.githubusercontent.com/GWarrenn/three-true-outcomes/master/data/teams.csv")
  
  game_length <- inning_data %>%
    group_by(game_id) %>%
    summarise(max_inning = max(inning))
  
  inning_data <- merge(inning_data,game_length,by = "game_id")
  
  inning_data <- inning_data %>% filter(max_inning == 9)
  
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
  
  cumulative_data <- cumulative_data %>%
    group_by(game_id) %>%
    mutate(hometeam_adj = lag(hometeam))
  
  cumulative_data$hometeam <- ifelse(is.na(cumulative_data$hometeam),cumulative_data$hometeam_adj,cumulative_data$hometeam)
  
  match_ups <- cumulative_data %>%
    ungroup() %>%
    distinct(hometeam,visteam) %>%
    filter(!is.na(hometeam))

  home_teams <- reactive({
    home_teams <- cumulative_data %>%
      ungroup() %>%
      distinct(hometeam) %>%
      select(hometeam)  %>%
      filter(!is.na(hometeam)) 
  })
  
  observe({
    updateSelectInput(session = session, inputId = "home_team_list", choices = home_teams()$hometeam)
  })  
  
  away_teams <- reactive({
    away_teams <- cumulative_data %>%
      ungroup() %>%
      distinct(visteam) %>%
      select(visteam) %>%
      filter(!is.na(visteam)) 
  })
  
  observe({
    updateSelectInput(session = session, inputId = "away_team_list", choices = away_teams()$visteam)
  })    
  
  output$distPlot <- renderPlot({
  
    teams <- c(input$home_team_list,input$away_team_list)
    
    comparison_1 <- cumulative_data %>%
      filter(hometeam == teams[1] & visteam == teams[2]) %>%
      group_by(inning) %>%
      summarise(avg_winnings = mean(cumulative_outcome))
    
    comparison_2 <- cumulative_data %>%
      filter(hometeam == teams[2] & visteam == teams[1]) %>%
      group_by(inning) %>%
      summarise(avg_winnings = mean(cumulative_outcome))
    
    if (nrow(comparison_1) > 0 & nrow(comparison_2) > 0){
    
      comparison_1$label <- paste("Home Team:",teams[1])
      comparison_2$label <- paste("Home Team:",teams[2])
      
      filter_1 <- comparison_1 %>% filter(inning == 9)
      filter_2 <- comparison_2 %>% filter(inning == 9)
      
      comparison <- rbind(comparison_1,comparison_2)

      output$results <- renderText({
        winner_1 <- ifelse(filter_1$avg_winnings > 0,"Three True Outcomes wins","Anything Else Wins")
        winner_1_amt <- filter_1$avg_winnings
        
        winner_2 <- ifelse(filter_2$avg_winnings > 0,"Three True Outcomes wins","Anything Else Wins")
        winner_2_amt <- filter_2$avg_winnings
        
        modifier <- ifelse((filter_1$avg_winnings > 0 & filter_2$avg_winnings < 0) | 
                             (filter_1$avg_winnings < 0 & filter_2$avg_winnings > 0),"However, when ","And when ")
        
        HTML(paste0("When ",teams[1]," plays ",teams[2]," at home, <b>",winner_1,"</b> wins $",round(abs(winner_1_amt),2),". ", modifier,
             teams[2]," plays ",teams[1]," at home, <b>",winner_2,"</b> wins $",round(abs(winner_2_amt),2)))
      })
      
      comparison$fill_guide = ifelse(comparison$avg_winnings == 0,"Tie",
                                     ifelse(comparison$avg_winnings > 0,"Three True Outcomes Winning","Anything Else Winning"))
    
      pal <- c(
        "Tie" = "white",
        "Three True Outcomes Winning" = "green", 
        "Anything Else Winning" = "red"
      )
      
      ggplot() +
        geom_line(data=comparison,aes(x=inning,y=avg_winnings,color=fill_guide,group=label),alpha=1,size=3) +
        facet_wrap(~label) +
        #geom_text(data = ann_text,aes(x=inning,y=avg_winnings,label = lab),angle = -90) +
        #geom_text(data = ann_text_2,aes(x=inning,y=avg_winnings,label = lab),angle = -90) +
        geom_hline(yintercept = 0,size =2) +
        labs(x="Inning",
             y="Three True Outcomes Winnings",
             title=paste(teams[1],"vs.",teams[2]),
             subtitle ='Average Cumulative Winnings by Home Field Advantage & Inning') +
        scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9)) +
        scale_color_manual(
          values = pal,
          limits = names(pal)
        )
    }
    else{
      output$results <- renderText({
        HTML(paste0("No 2017 game data available for ", teams[1]," and ",teams[2]))
      })
    }
  })
})
