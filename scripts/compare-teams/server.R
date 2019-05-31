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
  
  inning_data <- read.csv("C:/Users/august.warren/Downloads/retrosheet/agg-inning-results.csv")
  
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

  home_teams <- reactive({
    home_teams <- cumulative_data %>%
      ungroup() %>%
      distinct(hometeam) %>%
      select(hometeam) %>%
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
    
    ggplot() +
      geom_line(data=comparison,aes(x=inning,y=avg_winnings),alpha=1,size=3,color="red") +
      facet_wrap(~label) +
      geom_text(data = ann_text,aes(x=inning,y=avg_winnings,label = lab),angle = -90) +
      geom_text(data = ann_text_2,aes(x=inning,y=avg_winnings,label = lab),angle = -90) +
      geom_hline(yintercept = 0,size =2) +
      labs(x="Inning",
           y="Three True Outcomes Winnings",
           title=paste(teams[1],"vs.",teams[2]),
           subtitle ='Average Cumulative Winnings by Home Field Advantage & Inning')
    
  })
  
})
