png(paste(getwd(), "wwc_teamdefheatmap.png", sep="/"), width=1500, height=600)
plot.teamDefense(events.wwc, "Women's World Cup 2019")
dev.off()

png(paste(getwd(), "completedBoxPasses_Horan.png", sep="/"), width=653, height=380)
plot.Passes_CompletedBox(4999, "Lindsey Michelle Horan")
dev.off()

png(paste(getwd(), "weuro_teamdefheatmap.png", sep="/"), width=1500, height=600)
plot.teamDefense(events.weuro, "Women's Euro 2022")
dev.off()

events <- events.weuro.22
ppda.all <- c()
for (tm.id in unique(events$team.id)) {ppda.all<-c(ppda.all,get.PPDA(events,tm.id))}
ppda.teams.euro <- merge(
  unique(events[,c("team.name","team.id")]),
  data.frame(team.id=unique(events$team.id), ppda=ppda.all),
  by="team.id"
) %>% arrange(ppda)
gplt <- ggplot(ppda.teams.euro, aes(y=ppda, x=1:length(ppda.all), label=team.name)) +
  geom_col() +
  geom_text(angle=90, hjust="left", y=0.1, color="white")
  
plot(gplt)

events <- events.epl.16
events.br <- events %>% filter(type.id==2 & is.na(ball_recovery.recovery_failure))
events.br["unit"] <- rep(1, each=nrow(events.br))
br.plr.count <- events.br %>% group_by(player.id, player.name, team.name) %>% 
  summarise(total_br=sum(unit)) %>% 
  arrange(desc(total_br))

plot.ball_recoveries(events.wc.18, 3961)

events.epl.04.ars.prog <- get.progressivePasses(events.epl.04.ars) %>% filter(team.name=="Arsenal" & !is.na(pass.progressive)) %>% select(player.name, player.id, location, pass.end_location, pass.progressive)
events.epl.04.ars.prog["unit"] <- rep(1, each=nrow(events.epl.04.ars.prog))
View(events.epl.04.ars.prog %>% group_by(player.id) %>% summarise(player.name=player.name, pP=sum(pass.progressive)))

events.wwc.prog <-  get.progressivePasses(events.wwc) %>% filter(!is.na(pass.progressive))
events.wwc.prog["unit"] <- rep(1, each=nrow(events.wwc.prog))
events.wwc.prog.count <- events.wwc.prog %>% group_by(player.name, player.id, team.name) %>% summarise(pP=sum(unit)) %>% arrange(desc(pP))
plot.progressivePasses(get.progressivePasses(events.wwc) %>% filter(player.id==4999))

plot.passingNetwork.team_comp(events.wwc, 1214)


drib <-  events.wwc %>% filter(type.id==14)
carry <- events.wwc %>% filter(type.id==43)
#carry <- carry[4,]
allRelated <- c()
for (i in 1:length(carry$related_events)) {
  allRelated <- c(allRelated, carry$related_events[[i]])
}
related <- events.wwc %>% filter(id %in% allRelated)

#carry.wwc <- get.progressive.carry(events.wwc)
test.carry <- get.progressive(events.wwc,"Carry")

rl.c <- events.wwc %>% 
  filter(player.id==4949 & type.name=="Carry") %>% 
  select(location.x,location.y,carry.end_location.x,carry.end_location.y,related_events)
dist <- c()
dist.e <- c()
for (i in 1:nrow(rl.c)) {
  d <- dist_to_gl(
    rl.c[i,"location.x"],
    rl.c[i,"location.y"]
  )
  d.e <- dist_to_gl(
    rl.c[i,"carry.end_location.x"],
    rl.c[i,"carry.end_location.y"]
  )
  dist <- c(dist, d)
  dist.e <- c(dist.e, d.e)
}
rl.c["distToGoal"] <- unlist(dist)
rl.c["distToGoal.end"] <- unlist(dist.e)

test.carry["pC.unit"] <- ifelse(is.na(test.carry$carry.progressive),0,1)
test.carry.grp <- test.carry %>% group_by(player.id,player.name,team.name) %>%
  summarise(pC=sum(pC.unit))



carry.wwc <- get.progressive(events.wwc,"Carry")
pPass.wwc <- get.progressive(events.wwc,"Pass")
# carry.wwc["pC.unit"] <- ifelse(is.na(carry.wwc$carry.progressive),0,1)
# pPass.wwc["pP.unit"] <- ifelse(is.na(pPass.wwc$pass.progressive),0,1)
# wwc.group <- merge(
#   carry.wwc %>% group_by(player.id,player.name,team.name) %>%
#     summarise(pC=sum(pC.unit),pC.dist=sum(carry.progressive.distance)),
#   pPass.wwc %>% group_by(player.id,player.name,team.name) %>%
#     summarise(pP=sum(pP.unit),pP.dist=sum(pass.progressive.distance)),
#   by=c("player.id","player.name","team.name")
# )
wwc.group <- merge(
  carry.wwc %>% group_by(player.id,player.name,team.name) %>%
    summarise(pC=count.createdEvent.col(carry.progressive),pC.dist=sum(carry.progressive.distance)),
  pPass.wwc %>% group_by(player.id,player.name,team.name) %>%
    summarise(pP=count.createdEvent.col(pass.progressive),pP.dist=sum(pass.progressive.distance)),
  by=c("player.id","player.name","team.name")
)

wc.74 <- get.match_info(events.wc.74)
wc.18 <- get.match_info(events.wc.18)
wc.22 <- get.match_info(events.wc.22)

carry.wc.18 <- get.progressive(events.wc.18,"Carry")
pPass.wc.18 <- get.progressive(events.wc.18,"Pass")

carry.wc.22 <- get.progressive(events.wc.22,"Carry")
pPass.wc.22 <- get.progressive(events.wc.22,"Pass")

wc.18.ind <- merge(
  carry.wc.18 %>% group_by(player.id,team.id,player.name,team.name) %>%
    summarise(pC=count.createdEvent.col(carry.progressive),pC.distance=sum(carry.progressive.distance)),
  pPass.wc.18 %>% group_by(player.id,team.id,player.name,team.name) %>%
    summarise(pP=count.createdEvent.col(pass.progressive),pP.distance=sum(pass.progressive.distance)),
  by=c("player.id","team.id","player.name","team.name")
)
wc.22.ind <- merge(
  carry.wc.22 %>% group_by(player.id,team.id,player.name,team.name) %>%
    summarise(pC=count.createdEvent.col(carry.progressive),pC.distance=sum(carry.progressive.distance)),
  pPass.wc.22 %>% group_by(player.id,team.id,player.name,team.name) %>%
    summarise(pP=count.createdEvent.col(pass.progressive),pP.distance=sum(pass.progressive.distance)),
  by=c("player.id","team.id","player.name","team.name")
)

minutes.wc.18 <- get.minutesplayed.total(events.wc.18)
wc.18.ind <- merge(
  minutes.wc.18,
  wc.18.ind,
  by=c("player.id","player.name")
)
minutes.wc.22 <- get.minutesplayed.total(events.wc.22)
wc.22.ind <- merge(
  minutes.wc.22,
  wc.22.ind,
  by=c("player.id","player.name")
)
wc.22.ind <- merge(
  wc.22.ind,
  get.player.primaryPosition(events.wc.22),
  by="player.id"
) %>% filter(position.primary != 1)

wc.22.ind <- compile.individualStats(events.wc.22,carry.wc.22,pPass.wc.22)
wc.18.ind <- compile.individualStats(events.wc.18,carry.wc.18,pPass.wc.18)

wc.18.ind.90 <- to.per90(wc.18.ind)
wc.22.ind.90 <- to.per90(wc.22.ind)

wc.22.ind.90["pos_group.name"] <- ifelse(
  wc.22.ind.90$position.primary %in% get.defense.id(),
  "DF",
  ifelse(
    wc.22.ind.90$position.primary %in% get.midfield.id(),
    "MF",
    ifelse(
      wc.22.ind.90$position.primary %in% get.attack.id(),
      "FW",
      NA
    )
  )
)

wc.22.ind.90 <- wc.22.ind.90 %>% filter(position.primary!=1)
# labBool <- ((wc.22.ind.90 %>% arrange(desc(pP)))$pP[50] <= wc.22.ind.90$pP &
#               (wc.22.ind.90 %>% arrange(desc(pC)))$pC[50] <= wc.22.ind.90$pC) |
#   ((wc.22.ind.90 %>% arrange(desc(pP)))$pP[25] <= wc.22.ind.90$pP) |
#   (wc.22.ind.90 %>% arrange(desc(pC)))$pC[25] <= wc.22.ind.90$pC |
library(ggplot2)
library(ggrepel)
labBool <- (wc.22.ind.90 %>% arrange(desc(pP)))$pP[50] <= wc.22.ind.90$pP |
  (wc.22.ind.90 %>% arrange(desc(pC)))$pC[50] <= wc.22.ind.90$pC
gplt <- ggplot(wc.22.ind.90, 
               aes(x=pC,
                   y=pP,
                   # label=ifelse(
                   #   labBool,
                   #   paste(player.name,team.name,sep='\n'),
                   #   NA
                   #  ), 
                   label=paste(player.name,team.name,sep='\n'),
                   shape=factor(pos_group.name),
                   color=factor(team.name)
                  )
              ) +
  geom_point() +
  geom_text_repel(size=2) +
  labs(title = "2022 World Cup") +
  xlab("Progressive-Carries per 90") +
  ylab("Progressive-Passes per 90") +
  scale_colour_discrete(name="Nation") + 
  scale_shape_discrete(name="Postion") +
  coord_fixed(max(wc.22.ind.90$pC)/max(wc.22.ind.90$pP))
  #+ geom_smooth(mapping=factor(pos_group.name), method=lm)
  #guides(color=guide_legend(title=c("Nation","Position"))) #
plot(gplt)


plot.directness.competition(events.wc.22, title = "WC 2022")

write.csv(apply(events.wc.18,2,as.character), "Data/Events/wc22.csv")
write.csv(apply(events.wc.22,2,as.character), "Data/Events/wc18.csv")
write.csv(apply(events.wwc.19,2,as.character), "Data/Events/wwc19.csv")
write.csv(apply(events.weuro.22,2,as.character), "Data/Events/weuro22.csv")

write.csv(apply(carry.wc.18,2,as.character),"Data/Prog/wc18_carry")
write.csv(apply(pPass.wc.18,2,as.character),"Data/Prog/wc18_pPass")

write.csv(apply(carry.wc.22,2,as.character),"Data/Prog/wc22_carry")
write.csv(apply(pPass.wc.22,2,as.character),"Data/Prog/wc22_pPass")

write.csv(apply(carry.wwc.19,2,as.character),"Data/Prog/wwc19_carry")
write.csv(apply(pPass.wwc.19,2,as.character),"Data/Prog/wwc19_pPass")

write.csv(apply(carry.weuro.22,2,as.character),"Data/Prog/weuro22_carry")
write.csv(apply(pPass.weuro.22,2,as.character),"Data/Prog/weuro22_pPass")



pp <- pPass.wc.22 %>% filter(!is.na(pass.progressive))
min(pp$pass.angle)

