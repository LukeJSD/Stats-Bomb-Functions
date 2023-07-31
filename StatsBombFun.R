library(StatsBombR)
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)

get.competitionEvents <- function(id.c, id.s){
  comp <- FreeCompetitions()
  matches <- FreeMatches(
    comp[comp$competition_id==id.c & comp$season_id==id.s,]
  )
  
  events <- get.matchFree(matches[1,])
  for (m in 2: nrow(matches)) {
    me <- get.matchFree(matches[m,])
    cols <- names(events)[(names(events) %in% names(me))]
    events <- rbind(events[,cols], me[,cols])
  }
  return(events)
}

get.minutesplayed.custom <- function(df, match.id=NULL) {
  # if (unique(df$match_id)>1) {
  #   match <- df %>% filter(match_id==match.id)
  # } else {
  #   match <- df
  # }
  start.11.all <- match[match$type.id==35 & match$possession==1,"tactics.lineup"]
  players.df <- data.frame(
    player.name=c(start.11.all[[1]][[1]]$player.name, start.11.all[[1]][[2]]$player.name),
    player.id=c(start.11.all[[1]][[1]]$player.id, start.11.all[[1]][[2]]$player.id),
    minute.on=rep(0, each=22),
    minute.off=rep(90, each=22)
  )
  subs.all <- match[match$type.id==19,]
  for (sub in 1:nrow(subs.all)) {
    sub <-  subs.all[sub,]
    player.new <- data.frame(
      player.name=c(sub$substitution.replacement.name),
      player.id=c(sub$substitution.replacement.id),
      minute.on=c(sub$minute),
      minute.off=c(90)
    )
    players.df <- rbind(players.df, player.new)
    players.df[players.df$player.id==sub$player.id,"minute.off"] <- sub$minute
  }
  players.df["minutes_played"] <- players.df$minute.off - players.df$minute.on
  return(players.df[,c("player.name", "player.id", "minutes_played")])
}


plot.teamDefense <- function(df, leag_yr, doPlot=T) {
  defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
                               "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                               "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                               "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd",
                               "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", "#798590",
                               "#697785", "#526173", "#435367", "#3a4b60", "#2e4257", "#1d3048",
                               "#11263e", "#11273e", "#0d233a", "#020c16") 
  
  
  heatmap = df %>%mutate(location.x = ifelse(location.x>120, 120, location.x),
                             location.y = ifelse(location.y>80, 80, location.y),
                             location.x = ifelse(location.x<0, 0, location.x),
                             location.y = ifelse(location.y<0, 0, location.y))
  heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE )
  heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE)
  
  heatmap = heatmap%>%
    filter(type.name=="Pressure" | duel.type.name=="Tackle" |
             type.name=="Foul Committed" | type.name=="Interception" |
             type.name=="Block" ) %>%
    group_by(team.name) %>%
    mutate(total_DA = n()) %>%
    group_by(team.name, xbin, ybin) %>%
    summarise(total_DA = max(total_DA),
              bin_DA = n(),
              bin_pct = bin_DA/total_DA,
              location.x = median(location.x),
              location.y = median(location.y)) %>%
    group_by(xbin, ybin) %>%
    mutate(league_ave = mean(bin_pct)) %>%
    group_by(team.name, xbin, ybin) %>%
    mutate(diff_vs_ave = bin_pct - league_ave)
  
  gp <- ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) +
    geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) + #2
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(),
          line = element_blank()) +
    annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
    annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
    annotate("path", colour = "white", size = 0.6,
             x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+
    annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
    annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
    theme(axis.text.x=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
          plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
          axis.text.y=element_blank(),
          legend.title = element_blank(),
          legend.text=element_text(size=22,family="Source Sans Pro"),
          legend.key.size = unit(1.5, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5,
                                    family="Source Sans Pro", colour = "black", hjust = 0.5),
          legend.direction = "vertical",
          axis.ticks=element_blank(),
          plot.background = element_rect(fill = "white"),
          strip.text.x = element_text(size=13,family="Source Sans Pro")) + #4
    scale_y_reverse() + #5
    scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels =
                           scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + #6
    labs(title = "Where Do Teams Defend vs League Average?", subtitle = leag_yr) + #7
    coord_fixed(ratio = 95/100) + 
    annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                   length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                      xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9
    facet_wrap(~team.name)+ #10
    guides(fill = guide_legend(reverse = TRUE))
  
  if (doPlot) {plot(gp)}
  return(gp)
}

add_pitch <- function() {
  pitch <- annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(),
          line = element_blank()) +
    annotate("point", x = 12 , y = 40, colour = "black", size = 1.05) +
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6,
             x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black")
  return(pitch)
}

plot.Passes_CompletedBox <- function(player_id, player_name, doPlot=T){
  passes = events.wwc %>%
    filter(type.name=="Pass" & is.na(pass.outcome.name) &
             player.id==player_id) %>% #1
    filter(pass.end_location.x>=102 & pass.end_location.y<=62 &
             pass.end_location.y>=18) #2
  gp <- ggplot() +
    xlim(0, 120) +
    ylim(0, 80) +
    geom_segment(data = passes, aes(x = location.x, y = location.y,
                                    xend = pass.end_location.x, yend = pass.end_location.y),
                 lineend = "round", size = 0.5, colour = "#000000", arrow =
                   arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + #3
    labs(title = paste(player_name, "Completed Box Passes", sep=", ")) + #4
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(),
          line = element_blank()) +
    annotate("point", x = 12 , y = 40, colour = "black", size = 1.05) +
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6,
             x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    scale_y_reverse() + #5
    coord_fixed(ratio = 105/100)
  if (doPlot) {plot(gp)}
  return(gp)
}

get.PPDA <- function(df, tm.id) {
  match.id.all <- unique(df[df$team.id==tm.id,]$match_id)
  events <- df %>% filter(match_id %in% match.id.all)
  pitch_subset.att60.defAct = events %>%
    filter((duel.type.name=="Tackle" | type.name=="Foul Committed" | 
             type.name=="Interception") & (location.x>=120*0.4 & possession_team.id!=tm.id)) %>% # | type.name=="Block") %>%
    filter()
  pitch_subset.att60.passes = events %>%
    filter(type.name=="Pass" & is.na(pass.outcome.name)) %>%
    filter(location.x>=120*0.4 & possession_team.id!=tm.id)
  return(nrow(pitch_subset.att60.passes) / nrow(pitch_subset.att60.defAct))
}

plot.ball_recoveries <- function(df, plr.id) {
  events <- df %>% 
    filter(type.id==2 & player.id==plr.id & is.na(ball_recovery.recovery_failure))
  plr.name <- unique(events$player.name)
  gplt <- ggplot(events) +
    xlim(c(0,120)) +
    ylim(c(0,80)) +
    annotate_pitchSB(colour = "grey", fill = "black") +
    geom_point(aes(x=location.x, y=location.y, col="indianred4")) +
    labs(title = paste(plr.name, paste(nrow(events), "Successful Ball Recoveries"), sep=", ")) + 
    coord_flip()
  
  
  position.df <- get.player.positions(events) %>% filter(player.id==plr.id)
  if (is.list(position.df$positions.id)) {
    pid.all <- position.df$positions.id[[1]]
  }
  else {pid.all <- c(position.df$positions.id)}
  for (pos.id in pid.all) {
    rect_coord <- get.positionRectangle(pos.id)
    gplt <- gplt + annotate("rect",xmin = rect_coord[1], xmax = rect_coord[2], ymin =rect_coord[3], ymax = rect_coord[4], fill = "dodgerblue", colour = "dodgerblue", alpha=0.3)
  }  
  plot(gplt)
}

get.player.positions <- function(df) {
  return(players <- df %>% 
    group_by(player.id, player.name, position.id, position.name) %>% summarise() %>%
    group_by(player.id, player.name) %>% summarise(positions.id=list(position.id), positions.name=list(position.name)))
}

get.player.primaryPosition <- function(df) {
  players <- df %>% group_by(player.id) %>% 
    summarise(
      position.primary=unique(position.id)[which.max(tabulate(match(position.id, unique(position.id))))]
    )
  return(players)
}

get.defense.id <- function() {return(2:8)}

get.midfield.id <- function() {return(c(9:16, 18:20))}

get.attack.id <- function() {return(c(17, 21:25))}

get.position.abbrev <- function(pos.id) {
  positions <- c(
    "GK",
    "RB", "RCB", "CB", "LCB", "LB", "RWB", "LWB",
    "RDM", "CDM", "LDM", "RM", "RCM", "CM", "LCM", "LM",
    "RW",
    "RAM", "CAM", "LAM",
    "LW", "RCF", "ST", "LCF", "SS"
  )
  return(positions[pos.id])
}

get.positionRectangle <- function(pos.id) {
  # c(xmin, xmax, ymin, max)
  if (pos.id==1) {return(c(0, 18, 18, 62))}   # GK
  else if (pos.id==2) {return(c(18, 34, 0, 18))}   # RB
  else if (pos.id==3) {return(c(18, 34, 18, 32))}   # RCB
  else if (pos.id==4) {return(c(18, 34, 32, 48))}   # CB
  else if (pos.id==5) {return(c(18, 34, 48, 62))}   # LCB
  else if (pos.id==6) {return(c(18, 34, 62, 80))}   # LB
  else if (pos.id==7) {return(c(34, 50, 0, 18))}   # RWB
  else if (pos.id==9) {return(c(34, 50, 18, 32))}   # RDM
  else if (pos.id==10) {return(c(34, 50, 32, 48))}   # CDM
  else if (pos.id==11) {return(c(34, 50, 48, 62))}   # LDM
  else if (pos.id==8) {return(c(34, 50, 62, 80))}   # LWB
  else if (pos.id==12) {return(c(50, 70, 0, 18))}   # RM
  else if (pos.id==13) {return(c(50, 70, 18, 32))}   # RCM
  else if (pos.id==14) {return(c(50, 70, 32, 48))}   # CM
  else if (pos.id==15) {return(c(50, 70, 48, 62))}   # LCM
  else if (pos.id==16) {return(c(50, 70, 62, 80))}   # LM
  else if (pos.id==17) {return(c(70, 86, 0, 18))}   # RW
  else if (pos.id==18) {return(c(70, 86, 18, 32))}   # RAM
  else if (pos.id==19) {return(c(70, 86, 32, 48))}   # CAM
  else if (pos.id==20) {return(c(70, 86, 48, 62))}   # LAM
  else if (pos.id==21) {return(c(70, 86, 62, 80))}   # LW
  else if (pos.id==22) {return(c(102, 120, 18, 32))}   # RCF
  else if (pos.id==23) {return(c(102, 120, 32, 48))}   # ST
  else if (pos.id==24) {return(c(102, 120, 48, 62))}   # LCF
  else if (pos.id==25) {return(c(86, 102, 32, 48))}   # SS
  else {return(NULL)}
}

dist_to_gl <- function(x, y) {
  touchline <- 120
  min_post <- 36
  max_post <- 50
  if (y >= min_post & y <= max_post) {
    return(120-x)
  } else if (y < min_post) {
    return(sqrt((120-x)**2 + (min_post-y)**2))
  } else if (y > max_post) {
    return(sqrt((120-x)**2 + (y-max_post)**2))
  }
}

is.boxEntry <- function(x0, y0, x1, y1) {
  if (x0>=102 & y0>=18 & y0<=62) {
    return(FALSE)
  } else {
    if (x1>=102 & y1>=18 & y1<=62) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

get.progressivePasses <- function(df) {
  events <- df %>% filter(type.id==30 & is.na(pass.outcome.id) & location.x>=120*0.4 & pass.length >= 10)
  d2g_0 <- c()
  d2g_1 <- c()
  bx.ent <- c()
  for (i in 1:nrow(events)) {
    d2g_0 <- c(d2g_0, dist_to_gl(events$location.x[i], events$location.y[i]))
    d2g_1 <- c(d2g_1, dist_to_gl(events$pass.end_location.x[i], events$pass.end_location.y[i]))
    bx.ent <- c(bx.ent, is.boxEntry(events$location.x[i], 
                events$location.y[i], 
                events$pass.end_location.x[i], 
                events$pass.end_location.y[i]))
  }
  events["location.distToGoal"] <- d2g_0
  events["pass.end_location.distToGoal"] <- d2g_1
  events["pass.box_entry"] <- ifelse(bx.ent, TRUE, NA)
  events["pass.progressive"] <- ifelse((events$pass.end_location.distToGoal / events$location.distToGoal) < 0.75 | !is.na(events$pass.box_entry), TRUE, NA)
  return(events)
  # progressive.df <- NULL
  # for (m.id in unique(events$match_id)) {
  #   events.match <- events %>% filter(match_id==m.id)
  #   for (poss in unique(events.match$possession)) {
  #     isProg <- c()
  #     events.possession <- events.match %>% filter(possession==poss)
  #     if (nrow(events.possession)<1) {next}
  #     for (pass.iter in 1:nrow(events.possession)) {
  #       if (is.boxEntry(events.possession$location.x[pass.iter], events.possession$location.y[pass.iter], events.possession$pass.end_location.x[pass.iter], events.possession$pass.end_location.y[pass.iter])) {
  #         isProg <- c(isProg, TRUE)
  #         next
  #       }
  #       if (pass.iter<7) {
  #         if (pass.iter==1) {
  #           d0 <- events.possession$location.distToGoal[pass.iter]
  #           d1 <- events.possession$pass.end_location.distToGoal[pass.iter]
  #         } else {
  #           d1 <- events.possession$pass.end_location.distToGoal[pass.iter]
  #           d0 <- min(events.possession[1:(pass.iter-1),"pass.end_location.distToGoal"])
  #           d0 <- min(events.possession[1:(pass.iter-1),"location.distToGoal"])
  #           d0 <- min(c(d0, events.possession$location.distToGoal[pass.iter]))
  #         }
  #       } else {
  #         d1 <- events.possession$pass.end_location.distToGoal[pass.iter]
  #         d0 <- min(events.possession[(pass.iter-6):(pass.iter-1),"pass.end_location.distToGoal"])
  #         d0 <- min(events.possession[1:(pass.iter-1),"location.distToGoal"])
  #         d0 <- min(c(d0, events.possession$location.distToGoal[pass.iter]))
  #       }
  #       if ((d1+10) <= d0) {
  #         isProg <- c(isProg, TRUE)
  #       } else {
  #         isProg <- c(isProg, NA)
  #       }
  #     }
  #     events.possession["pass.progressive"] <- isProg
  #     if (is.null(progressive.df)) {
  #       progressive.df <- events.possession
  #     } else {
  #       progressive.df <-  rbind(progressive.df, events.possession)
  #     }
  #   }
  # }
  # return(progressive.df)
}

plot.progressivePasses <- function(df) {
  events <- df %>% filter(type.id==30 & is.na(pass.outcome.id) & !is.na(pass.progressive) &
                            location.x>=120*0.4 & pass.length >= 10)
  gplt <- ggplot() +
    geom_segment() +
    xlim(c(120*0.4,120)) +
    ylim(c(0,80)) +
    geom_segment(data = events, aes(x = location.x, y = location.y,
                                    xend = pass.end_location.x, yend = pass.end_location.y),
                 lineend = "round", size = 0.5, colour = "#000000", arrow =
                   arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + 
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(),
          line = element_blank()) +
    annotate("point", x = 12 , y = 40, colour = "black", size = 1.05) +
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6,
             x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    scale_y_reverse() + coord_fixed(ratio = (120*.6)/80)
  plot(gplt)
}

plot.passingNetwork <- function(df, mt.id, tm) {
  events <- df %>% filter(match_id==mt.id & team.id==tm)
  subs <- (events %>% filter(type.id==19) %>% select(index, minute))
  firstSub.ind <- ifelse(nrow(subs)>0, subs$index[1], events$index[nrow(events)])
  firstSub.min <- ifelse(nrow(subs)>0, subs$minute[1], 90)
  passes <- events %>% filter(index<firstSub.ind & type.id==30 & is.na(pass.outcome.id))
  passes["unit"] <- rep(1, each=nrow(passes))
  passes.grouped <- passes %>% 
    group_by(player.id, pass.recipient.id, match_id) %>% 
    summarise(pass.total=sum(unit)*(90/firstSub.min)) %>%
    filter(pass.total>=5)
  player.position <- NULL
  players.locations <- passes %>% group_by(player.id, player.name, match_id) %>%
    summarise(pass.per90=sum(unit)*(90/firstSub.min), location.x.avg=mean(location.x), location.y.avg=mean(location.y))
  gplt <- ggplot() + xlim(0,120) + ylim(0,80)
  gplt <- gplt +
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "grey", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "grey", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "grey", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "grey", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "grey", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "grey", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "grey", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "grey", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "grey", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "grey", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "grey", size = 0.6)+
    theme(rect = element_blank(),
          panel.background = element_rect(fill="black"),
          line = element_blank()) +
    annotate("point", x = 12 , y = 40, colour = "grey", size = 1.05) +
    annotate("point", x = 108 , y = 40, colour = "grey", size = 1.05) +
    annotate("path", colour = "grey", size = 0.6,
             x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+
    annotate("point", x = 60 , y = 40, colour = "grey", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="grey") +
    annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="grey")
  
  pass.segments <- NULL
  for (pl.id in unique(passes.grouped$player.id)) {
    passer.loc <- players.locations %>% filter(player.id==pl.id) %>% select(location.x.avg, location.y.avg)
    passer.loc.x <- passer.loc$location.x.avg[1]
    passer.loc.y <- passer.loc$location.y.avg[1]
    passer.net <- passes.grouped %>% filter(player.id==pl.id)
    for (rec.id in passer.net$pass.recipient.id) {
      receiver.loc <- players.locations %>% filter(player.id==rec.id)
      receiver.loc.x <- receiver.loc$location.x.avg[1]
      receiver.loc.y <- receiver.loc$location.y.avg[1]
      passer.net.rec <- passer.net %>% filter(pass.recipient.id==rec.id)
      
      if (is.null(pass.segments)) {
        pass.segments <-  data.frame(
          x=c(passer.loc.x),
          y=c(passer.loc.y),
          xend=c(receiver.loc.x),
          yend=c(receiver.loc.y),
          sz=c(passer.net.rec$pass.total[1]/10)
        )
      } else {
        pass.segments <-  rbind(
          pass.segments,
          data.frame(
            x=c(passer.loc.x),
            y=c(passer.loc.y),
            xend=c(receiver.loc.x),
            yend=c(receiver.loc.y),
            sz=c(passer.net.rec$pass.total[1]/10)
          )
        )
      }
    }
  }
  gplt <- gplt + geom_point(data = players.locations, aes(x=location.x.avg, y=location.y.avg, size=(pass.per90/10)+10), color="dodgerblue3") 
  gplt <- gplt + annotate("text", x=players.locations$location.x.avg+1, y=players.locations$location.y.avg, label=sub(".* ", "", players.locations$player.name), color="dodgerblue3")

  pass.segments["offset.x"] <- sin(pass.segments$xend-pass.segments$x)*0
  pass.segments["offset.y"] <- cos(pass.segments$yend-pass.segments$y)*0
  gplt <- gplt + geom_segment(data = pass.segments, aes(x=x+offset.x, y=y+offset.y,
                                  xend=xend+offset.x, yend=yend+offset.y),
                              lineend = "round", size = pass.segments$sz, alpha=0.7, colour = "indianred4", arrow =
                                arrow(length = unit(0.07, "inches"), ends = "last", type = "open"))
  opposing.tm.name <- (df %>% filter(match_id==mt.id & team.id!=tm))$team.name
  gplt <- gplt + 
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.background = element_blank(),
      legend.text = element_blank(),
      legend.position = "none"
    ) +
    labs(title = paste(unique(events$team.name), opposing.tm.name, sep = " vs. "), 
         subtitle = paste("First sub in", as.character(firstSub.min), sep=" minute ")
    )
  gplt <- gplt + coord_flip()
  gplt <- gplt + facet_wrap(~match_id)
  plot(gplt)
  return(gplt)
}

save.passingNetwork.team_comp <- function(df, tm.id, comp="") {
  match.id.all <- unique((df %>% filter(team.id==tm.id))$match_id)
  for (mt.id in match.id.all) {
    directory <- paste(getwd(), tm.id, sep="/Passing_Networks/")
    ensure.directory(directory)
    png(paste(directory, paste(mt.id, ".png", sep="_passNet"), sep="/"), width=800, height=1200)
    plot.passingNetwork(df, mt.id, tm.id) 
    dev.off()
  }
}

get.match_info <-  function(df) {
  events <- df
  
  events["unit.goal"] <- ifelse(!is.na(events$shot.outcome.id) & events$shot.outcome.id==97, 1, 0)
  events["unit.shot"] <- ifelse(!is.na(events$type.id) & events$type.id==16, 1, 0)
  events["unit.pass"] <- ifelse(events$type.id==30, 1, 0)
  events["unit.pass.comp"] <- ifelse(is.na(events$pass.outcome.id) & events$type.id==30, 1, 0)
  events$shot.statsbomb_xg[is.na(events$shot.statsbomb_xg)] = 0
  matches <- merge(
    events %>% group_by(match_id, team.id, possession) %>%
      summarise(
        TimeInPoss=max(TimeInPoss)
      ) %>% group_by(match_id, team.id) %>% 
      summarise(TimeInPoss=sum(TimeInPoss)),
    events %>% group_by(match_id, team.id) %>%
      summarise(
        goals=sum(unit.goal),
        xG=sum(shot.statsbomb_xg),
        shots=sum(unit.shot),
        pass.attempts=sum(unit.pass),
        pass.completed=sum(unit.pass.comp)
      ),
    by=c("match_id", "team.id")
  )
  matches <- merge(
    matches,
    events %>% group_by(team.name, team.id) %>% summarise(),
    by="team.id"
  )

  matches$TimeInPoss <- matches$TimeInPoss/60
  
  poss.per <- c()
  for (i in 1:nrow(matches)) {
    md <- matches$match_id[i]
    tm <- matches$team.id[i]
    per <- match.info[match.info$match_id==md & match.info$team.id==tm,"TimeInPoss"] / 
      (match.info[match.info$match_id==md & match.info$team.id==tm,"TimeInPoss"] + 
         match.info[match.info$match_id==md & match.info$team.id!=tm,"TimeInPoss"])
    poss.per <- c(poss.per, per)
  }
  matches["possesion.percentage"] <- poss.per
  
  return(matches %>% select(match_id, team.id, team.name, goals, xG, shots, pass.attempts, pass.completed))
}
