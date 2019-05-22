# -*- coding: utf-8 -*-
"""
Created on Mon May 20 15:45:54 2019

@author: August.Warren
"""

import pandas as pd
import numpy as np

#######################################
##
## load at-bat level data
##
#######################################

plays = "C:/Users/august.warren/Downloads/retrosheet/plays.csv"

plays_df = pd.read_csv(plays)

## classify outcomes

recode = {'W' : 2, 'K': 2}

plays_df['true_outcome'] = plays_df['play_str'].map(recode)
plays_df['true_outcome'] = np.where(plays_df['play_str'].str.contains("^H|^IW|^W.|^HR"),2,plays_df['true_outcome'])

plays_df['true_outcome'] = plays_df['true_outcome'].fillna(0)

plays_df['anything_else'] = np.where(plays_df['true_outcome'] == 2,0,1)

## export coded data to check classifications -- the string values are a little strange

plays_df[['game_id','inning','team','true_outcome','anything_else','play_str']].to_csv("C:/Users/august.warren/Downloads/retrosheet/checks.csv")

## collapse up to the inning level

agg_by_inning = plays_df[['game_id','inning','team','true_outcome','anything_else']].groupby(['game_id','inning','team']).sum().reset_index()

## determine inning-level net winners 

agg_by_inning['true_outcome_result'] = agg_by_inning['true_outcome'] - agg_by_inning['anything_else']

#######################################
##
## load game-level info to get home/away team names
##
#######################################

games = "C:/Users/august.warren/Downloads/retrosheet/info.csv"

games_df = pd.read_csv(games)

values = ["visteam","hometeam"]

games_df = games_df[games_df['var'].isin(values)]

games_df['team'] = np.where(games_df['var'] == "hometeam",1,0)

agg_by_inning = agg_by_inning.merge(games_df,how="inner",on=["game_id","team"])

agg_by_game_team = agg_by_inning[['game_id','true_outcome_result',"value"]].groupby(['game_id','value']).sum().reset_index()

agg_by_game_team.pivot(['game_id','true_outcome_result'], columns='true_outcome_result', values='value')

#######################################
##
## Export results
##
#######################################

agg_by_inning.to_csv("C:/Users/august.warren/Downloads/retrosheet/agg-inning-results.csv")
agg_by_game.to_csv("C:/Users/august.warren/Downloads/retrosheet/agg-results.csv")