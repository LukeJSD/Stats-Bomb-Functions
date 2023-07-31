# Stats-Bomb-Functions
## Summary
Created some functions that provide useful statistics, visuals, and easier to understand tables for Stats Bomb's free event data in R.
## Libraries
- StatsBombR
- ggplot2
- grid
- tidyverse
## Functions
- get.competitionEvents(id.c, id.s)
  - id.c: Competition id (Women's World Cup = 72, EPL = 2, La Liga = 11, etc.)
  - id.s: Season id (2015/16 = 27, 2003/04 = 4, etc.)
  - Use the StatsBombR function FreeCompetitions() to get all of the freely available competition details
  - The StatsBombR functions for getting all the matches in one call was not working, so made this function for getting lots of matches in 1 data.frame.
  - Some notes on the calls on this function in the main.R setup:
    - The 15/16 EPL season, 2019 World Cup, and 2022 Euros include every match from every team
    - The 03/04 EPL season only has Arsenal matches (Invincibles)
    - La Liga has pretty much every Barcelona match during the Messi era, but is missing most everything else. Have found only one season with all the Real Madrid games (15/16)
- get.minutesplayed.custom(df, match.id=NULL)
  - df: data.frame of all the events in a match
  - match.id: currently not implemented
  - The builtin StatsBombR function for getting minutes played was not working, so made this.
  - Right now only works when df is only 1 match
- plot.teamDefense(df, leag_yr, doPlot=T)
  - df: all the events from a competition
  - leag_yr: year of the season being plotted
  - doPlot: Plot the ggplot if true, only return the plot is false
  - Plots a defensive heatmap of every team a competition for a season
  - Taken directly from the StatsBomb's guide to working with their R library
    - https://statsbomb.com/wp-content/uploads/2021/11/Working-with-R.pdf
- 
