png(paste(getwd(), "wwc_teamdefheatmap.png", sep="/"), width=1500, height=600)
plot.teamDefense(events.wwc, "Women's World Cup 2019")
dev.off()

png(paste(getwd(), "completedBoxPasses_Horan.png", sep="/"), width=653, height=380)
plot.Passes_CompletedBox(4999, "Lindsey Michelle Horan")
dev.off()

png(paste(getwd(), "weuro_teamdefheatmap.png", sep="/"), width=1500, height=600)
plot.teamDefense(events.weuro, "Women's Euro 2022")
dev.off()

events <- events.weuro
ppda.all <- c()
for (tm.id in unique(events$team.id)) {ppda.all<-c(ppda.all,get.PPDA(events,tm.id))}
ppda.teams.euro <- merge(
  unique(events[,c("team.name","team.id")]),
  data.frame(team.id=unique(events$team.id), ppda=ppda.all),
  by="team.id"
) %>% arrange(ppda)
gplt <- ggplot(ppda.teams.euro) +
  geom_col(aes(y=ppda, x=1:length(ppda.all)))
plot(gplt)

events <- events.epl.16
events.br <- events %>% filter(type.id==2 & is.na(ball_recovery.recovery_failure))
events.br["unit"] <- rep(1, each=nrow(events.br))
br.plr.count <- events.br %>% group_by(player.id, player.name, team.name) %>% 
  summarise(total_br=sum(unit)) %>% 
  arrange(desc(total_br))

plot.ball_recoveries(events, 3961)

events.epl.04.ars.prog <- get.progressivePasses(events.epl.04.ars) %>% filter(team.name=="Arsenal" & !is.na(pass.progressive)) %>% select(player.name, player.id, location, pass.end_location, pass.progressive)
events.epl.04.ars.prog["unit"] <- rep(1, each=nrow(events.epl.04.ars.prog))
View(events.epl.04.ars.prog %>% group_by(player.id) %>% summarise(player.name=player.name, pP=sum(pass.progressive)))

events.wwc.prog <-  get.progressivePasses(events.wwc) %>% filter(!is.na(pass.progressive))
events.wwc.prog["unit"] <- rep(1, each=nrow(events.wwc.prog))
events.wwc.prog.count <- events.wwc.prog %>% group_by(player.name, player.id, team.name) %>% summarise(pP=sum(unit)) %>% arrange(desc(pP))
plot.progressivePasses(get.progressivePasses(events.wwc) %>% filter(player.id==4999))

plot.passingNetwork.team_comp(events.wwc, 1214)
