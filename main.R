setwd("C:/users/luke/Personal_Projects/Soccer/stats_bomb/")

source("C:/users/luke/Personal_Projects/Soccer/stats_bomb/StatsBombFun.R")

# WWC19: 72, 30
events.wwc <- allclean(get.competitionEvents(72,30))
# quarters.wwc = c(
#   "United States Women's","England Women's","France Women's","Germany Women's",
#   "Netherlands Women's","Sweden Women's","Italy Women's","Norway Women's"
# )
# WEuros: 53, 106
events.weuro <- allclean(get.competitionEvents(53,106))
events.ll.16 <- allclean(get.competitionEvents(11, 27))
events.epl.16 <- allclean(get.competitionEvents(2, 27))
events.epl.04 <- allclean(get.competitionEvents(2, 44)) %>% filter(team.name=="Arsenal")

events.wc.22 <- allclean(get.competitionEvents(43,106))
events.wc.18 <- allclean(get.competitionEvents(43,3))
events.wc.74 <- allclean(get.competitionEvents(43,51))
