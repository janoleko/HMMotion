######################################################################################
######################################################################################
#### BDB 2025 preprocessing: eliminate non rel players (only 5 def, 5 off)
######################################################################################
######################################################################################

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(vroom)
library(sp)


# Load data ---------------------------------------------------------------

games = read.csv("data/games.csv")
plays = read.csv("data/plays.csv")
players = read.csv("data/players.csv")
#tackles = read.csv("data/tackles.csv")
tracking = vroom("data/tracking_week_1.csv")
nweeks = 2 # only 3 weeks for data size
for(i in 2:nweeks){
  print(i)
  tracking = rbind(tracking, vroom(paste0("data/tracking_week_",i,".csv")))
}

# creating adjusted x value
leftInd = which(tracking$playDirection == "left")
tracking$x[leftInd] = tracking$x[leftInd] - 10
tracking$x[-leftInd] = (110) - tracking$x[-leftInd]
# creating adjusted y value
tracking$y = tracking$y - (53.3/2)

# creating adjusted direction
tracking$dir[leftInd] = (tracking$dir[leftInd] + 90) %% 360
tracking$dir[-leftInd] = (tracking$dir[-leftInd] + (360-90)) %% 360

# creating adjusted orientation
tracking$o[leftInd] = (tracking$o[leftInd] + 90) %% 360
tracking$o[-leftInd] = (tracking$o[-leftInd] + (360-90)) %% 360

plays$gameplayId = as.numeric(paste0(plays$gameId, plays$playId))
tracking$gameplayId = as.numeric(paste0(tracking$gameId, tracking$playId))


tracking_presnap1 = tracking %>% 
  #mutate(y = y - 53.3/2) %>% # center y-coordinate ## already done before
  mutate(uniId = paste0(gameId, playId, nflId)) %>%
  mutate(displayName_fac = factor(displayName, levels = unique(displayName))) %>% 
  mutate(playId_fac = factor(playId, levels = unique(playId))) %>% 
  group_by(displayName_fac, playId_fac) %>% #use group split to stay with the order
  group_split() %>% 
  map(~mutate(., initial_y = y[which(event == "line_set")[1]])) %>% 
  bind_rows() %>% 
  group_by(playId_fac) %>% 
  group_split() %>% 
  map(~filter(.,frameId >= frameId[which(event == "line_set")[1]])) %>% 
  bind_rows() %>% 
  # Take out the time where teams are in the huddle, i.e. before first line up
  filter(frameType == "BEFORE_SNAP") %>% # Filter out for data before the snap, i.e. the time of the motion
  # Connect with player and players df to receive information
  left_join(., players %>% dplyr::select(nflId, position), by = "nflId") %>% 
  left_join(., plays %>% dplyr::select(playId, gameId, absoluteYardlineNumber, possessionTeam, pff_manZone), 
            by = c("playId", "gameId")) %>% 
  mutate(off_def = ifelse(club == possessionTeam, 1, 0)) 

position_counts <- tracking_presnap1 %>%
  group_by(gameplayId) %>% 
  mutate(Count_all = n()/length(unique(nflId))) %>% 
  ungroup() %>% 
  group_by(gameplayId, position) %>% 
  summarise(Count = n(), .groups = "drop", Count_all = unique(Count_all)) %>%
  mutate(Count = Count/Count_all) %>% 
  #filter(position == "QB", Count == 2) %>% 
  filter(position %in% c("C", "G", "T")) %>% 
  group_by(gameplayId) %>% summarise(sum_oline = sum(Count)) %>% 
  filter(sum_oline > 5) %>% 
  pull(gameplayId)

position_counts2 <- tracking_presnap1 %>%
  group_by(gameplayId) %>% 
  mutate(Count_all = n()/length(unique(nflId))) %>% 
  ungroup() %>% 
  group_by(gameplayId, position) %>% 
  summarise(Count = n(), .groups = "drop", Count_all = unique(Count_all)) %>%
  mutate(Count = Count/Count_all) %>% 
  filter(position == "QB", Count == 2) %>% 
  pull(gameplayId)

valid_play_ids2 = tracking_presnap1 %>% group_by(gameplayId) %>% 
  summarise(Count = n(), .groups = "drop") %>% 
  mutate(invalid = ifelse(Count < 24, 1, 0)) %>% 
  filter(invalid != 1) %>% pull(gameplayId)

# 3. Behalte nur die relevanten PlayIds im ursprünglichen Datensatz
tracking_presnap1 <- tracking_presnap1 %>%
  filter(!(gameplayId %in% position_counts)) %>% 
  filter(!(gameplayId %in% position_counts2)) %>% 
  filter(gameplayId %in% valid_play_ids2)


nflIds <- readRDS("data/nflIds_def_per_play.rds")


tracking_premotion <- tracking_presnap1 |>
  filter(event == "line_set") |>
  filter(gameplayId %in% nflIds$gameplayId) ## load nflIds from results_df (see zvm.R file)
#filter(lag(event,n=3) == "line_set") ### doesn't fully work out sadly 

rm(tracking,tracking_presnap1)
gc()

trackingList = split(tracking_premotion, tracking_premotion$gameplayId)
out1 <- which(sapply(trackingList,nrow) != 23) ### needed but not clear what the problems is (hopefully only few!)
out2 <- which(sapply(trackingList, \(x) sum(x$position =="QB",na.rm = TRUE))>1) ## remove plays with two qbs
outinds <- union(out1,out2)

playernames = c(paste0("player_off", 1:5), paste0("player_def", 1:5))

chull_area <- function(x,y){
  ch <- chull(x,y)
  chf <- c(ch,ch[1])
  xy.coords <- cbind(x, y)
  chull.coords <- xy.coords[chf,]
  chull.poly <- Polygon(chull.coords, hole=F)
  chull.poly@area
}
chull_xsize <- function(x,y){
  ch <- chull(x,y)
  xy.coords <- cbind(x, y)
  chull.coords <- xy.coords[ch,]
  max(chull.coords[,1])-min(chull.coords[,1])
}
chull_ysize <- function(x,y){
  ch <- chull(x,y)
  xy.coords <- cbind(x, y)
  chull.coords <- xy.coords[ch,]
  max(chull.coords[,2])-min(chull.coords[,2])
}


if(length(outinds) == 0){
  tracking_reorder = trackingList %>% 
    #tracking_reorder = trackingList[1:30] %>% 
    #map(~mutate(., isballCarrier = ifelse(nflId == ballCarrierId, 1, 0))) %>% 
    #map(~mutate(., inballCarrierTeam = ifelse(club == club[which(isballCarrier==1)][1],1,0))) %>% 
    map(~mutate(., x_fb = rep(x[which(displayName == "football")], 23))) %>% 
    map(~mutate(., y_fb = rep(y[which(displayName == "football")], 23))) %>% 
    map(~mutate(., x_qb = rep(x[which(position == "QB")], 23))) %>% 
    map(~mutate(., y_qb = rep(y[which(position == "QB")], 23))) %>% 
    map(~mutate(., dir_qb = rep(dir[which(position == "QB")], 23))) %>% 
    map(~mutate(., x_rel = x - x_fb)) %>% 
    map(~mutate(., y_rel = y - y_fb)) %>%
    map(~mutate(., chull_area_o = chull_area(x[which(off_def == 1)], y[which(off_def == 1)]))) %>% 
    map(~mutate(., chull_area_d = chull_area(x[which(off_def == 0 & displayName != "football")], 
                                             y[which(off_def == 0 & displayName != "football")]))) %>% 
    map(~mutate(., chull_x_o = chull_xsize(x[which(off_def == 1)], y[which(off_def == 1)]))) %>% 
    map(~mutate(., chull_x_d = chull_xsize(x[which(off_def == 0 & displayName != "football")], 
                                           y[which(off_def == 0 & displayName != "football")]))) %>% 
    map(~mutate(., chull_y_o = chull_ysize(x[which(off_def == 1)], y[which(off_def == 1)]))) %>% 
    map(~mutate(., chull_y_d = chull_ysize(x[which(off_def == 0 & displayName != "football")], 
                                           y[which(off_def == 0 & displayName != "football")]))) %>% 
    #map(~mutate(., tackle_dummy = rep(build_tackledummy(.), 23))) %>% 
    #map(~mutate(., missed_tackle_dummy = rep(build_missed_tackledummy(.), 23))) %>% 
    map(~mutate(., isFootball = ifelse(displayName=="football", 1, 0))) %>% 
    map(~arrange(.,isFootball, -off_def,-y)) %>% 
    map(\(x){
      x |> filter(!(position %in% c("T", "G", "C", NA, "QB")))
    }) |>
    map(\(x){
      gpId <- unique(x$gameplayId)
      ids <- nflIds[which(nflIds$gameplayId == gpId),2:6]
      x |> filter(nflId %in% ids | off_def == 1)
    }) |>
    map(~mutate(., player = playernames)) %>% 
    bind_rows()
}else{
  tracking_reorder = trackingList[-union(out1,out2)] %>% 
    #tracking_reorder = trackingList[1:30] %>% 
    #map(~mutate(., isballCarrier = ifelse(nflId == ballCarrierId, 1, 0))) %>% 
    #map(~mutate(., inballCarrierTeam = ifelse(club == club[which(isballCarrier==1)][1],1,0))) %>% 
    map(~mutate(., x_fb = rep(x[which(displayName == "football")], 23))) %>% 
    map(~mutate(., y_fb = rep(y[which(displayName == "football")], 23))) %>% 
    map(~mutate(., x_qb = rep(x[which(position == "QB")], 23))) %>% 
    map(~mutate(., y_qb = rep(y[which(position == "QB")], 23))) %>% 
    map(~mutate(., dir_qb = rep(dir[which(position == "QB")], 23))) %>% 
    map(~mutate(., x_rel = x - x_fb)) %>% 
    map(~mutate(., y_rel = y - y_fb)) %>%
    map(~mutate(., chull_area_o = chull_area(x[which(off_def == 1)], y[which(off_def == 1)]))) %>% 
    map(~mutate(., chull_area_d = chull_area(x[which(off_def == 0 & displayName != "football")], 
                                             y[which(off_def == 0 & displayName != "football")]))) %>% 
    map(~mutate(., chull_x_o = chull_xsize(x[which(off_def == 1)], y[which(off_def == 1)]))) %>% 
    map(~mutate(., chull_x_d = chull_xsize(x[which(off_def == 0 & displayName != "football")], 
                                           y[which(off_def == 0 & displayName != "football")]))) %>% 
    map(~mutate(., chull_y_o = chull_ysize(x[which(off_def == 1)], y[which(off_def == 1)]))) %>% 
    map(~mutate(., chull_y_d = chull_ysize(x[which(off_def == 0 & displayName != "football")], 
                                           y[which(off_def == 0 & displayName != "football")]))) %>% 
    #map(~mutate(., tackle_dummy = rep(build_tackledummy(.), 23))) %>% 
    #map(~mutate(., missed_tackle_dummy = rep(build_missed_tackledummy(.), 23))) %>% 
    map(~mutate(., isFootball = ifelse(displayName=="football", 1, 0))) %>% 
    map(~arrange(.,isFootball, -off_def,-y)) %>% 
    map(\(x){
      x |> filter(!(position %in% c("T", "G", "C", NA, "QB")))
    }) |>
    map(\(x){
      gpId <- unique(x$gameplayId)
      ids <- nflIds[which(nflIds$gameplayId == gpId),2:6]
      x |> filter(nflId %in% ids | off_def == 1)
    }) |>
    map(~mutate(., player = playernames)) %>% 
    bind_rows()
}
## order here already by y axis!
#tracking_reorder = trackingList[-out] %>% 


trackingWide = tracking_reorder %>%
  select(-off_def,-initial_y,-position, -isFootball,-displayName_fac,
         -s,-a,-dis,-displayName,-jerseyNumber,-club,-uniId) %>% 
  pivot_wider(names_from = player, values_from = c(x,y,o,dir,x_rel, y_rel,nflId))


crs <- 23 ## start of relevant cols (-1) 
# pcc <- ncol(trackingWide)

# L2 distance to football
for(i in 1:5){
  oldcolnames = colnames(trackingWide)
  indx <- which(oldcolnames == paste0("x_player_off",i))
  indy <- which(oldcolnames == paste0("y_player_off",i))
  trackingWide$distance = sqrt((trackingWide$x_fb - trackingWide[,indx])^2 + (trackingWide$y_fb - trackingWide[,indy])^2)
  colnames(trackingWide) = c(oldcolnames, paste0("dist_fb_off",i))
}
for(i in 1:5){
  oldcolnames = colnames(trackingWide)
  indx <- which(oldcolnames == paste0("x_player_def",i))
  indy <- which(oldcolnames == paste0("y_player_def",i))
  trackingWide$distance = sqrt((trackingWide$x_fb - trackingWide[,indx])^2 + (trackingWide$y_fb - trackingWide[,indy])^2)
  colnames(trackingWide) = c(oldcolnames, paste0("dist_fb_def",i))
}
# x distance to football
for(i in 1:5){
  oldcolnames = colnames(trackingWide)
  indx = which(oldcolnames == paste0("x_player_off",i))
  trackingWide$distance = trackingWide$x_fb - trackingWide[,indx]
  colnames(trackingWide) = c(oldcolnames, paste0("xdist_fb_off",i))
}
for(i in 1:5){
  oldcolnames = colnames(trackingWide)
  indx = which(oldcolnames == paste0("x_player_def",i))
  trackingWide$distance = trackingWide$x_fb - trackingWide[,indx]
  colnames(trackingWide) = c(oldcolnames, paste0("xdist_fb_def",i))
}
# y distance to football
for(i in 1:5){
  oldcolnames = colnames(trackingWide)
  indy = which(oldcolnames == paste0("y_player_off",i))
  trackingWide$distance = trackingWide$y_fb - trackingWide[,indy]
  colnames(trackingWide) = c(oldcolnames, paste0("ydist_fb_off",i))
}
for(i in 1:5){
  oldcolnames = colnames(trackingWide)
  indy = which(oldcolnames == paste0("y_player_def",i))
  trackingWide$distance = trackingWide$y_fb - trackingWide[,indy]
  colnames(trackingWide) = c(oldcolnames, paste0("ydist_fb_def",i))
}
# angle to shortest segment defenders
for(i in 1:5){
  oldcolnames = colnames(trackingWide)
  indx <- which(oldcolnames == paste0("x_player_def",i))
  indy <- which(oldcolnames == paste0("y_player_def",i))
  shortest_segments = cbind(trackingWide$x_qb, trackingWide$y_qb) - cbind(trackingWide[,indx],trackingWide[,indy])
  angles = atan(shortest_segments[,2]/shortest_segments[,1])
  angles = angles*180/pi
  angles = (360 - angles + 540) %% 360
  inddir <- which(oldcolnames == paste0("dir_player_def",i))
  trackingWide$newangle = abs(angles - trackingWide[,inddir])
  colnames(trackingWide) = c(oldcolnames, paste0("def_dir_qb_diff",i))
}
# angle to shortest segment defenders orientation
for(i in 1:5){
  oldcolnames = colnames(trackingWide)
  indx <- which(oldcolnames == paste0("x_player_def",i))
  indy <- which(oldcolnames == paste0("y_player_def",i))
  shortest_segments = cbind(trackingWide$x_qb, trackingWide$y_qb) - cbind(trackingWide[,indx],trackingWide[,indy])
  angles = atan(shortest_segments[,2]/shortest_segments[,1])
  angles = angles*180/pi
  angles = (360 - angles + 540) %% 360
  indo <- which(oldcolnames == paste0("o_player_def",i))
  trackingWide$newangle = abs(angles - trackingWide[,indo])
  colnames(trackingWide) = c(oldcolnames, paste0("def_o_qb_diff",i))
}

############################
#### extract relevant cols
############################

### select relevant columns (might change!)

trackingWide_used <- trackingWide |>
  select(gameId,playId,gameplayId,pff_manZone,
         starts_with("chull_"),
         starts_with("x_rel_"),
         starts_with("y_rel_"),
         starts_with("dist_fb"),
         starts_with("xdist_fb"),
         starts_with("ydist_fb"),
         starts_with("def_o_qb_diff"),
         starts_with("nflId_"),)

### merge with pbp data

tw_full <- trackingWide_used |>
  left_join(plays |> select(gameplayId,
                            quarter,down,yardsToGo,
                            absoluteYardlineNumber,
                            preSnapHomeScore,
                            preSnapVisitorScore,
                            gameClock,
                            pff_passCoverage), by = "gameplayId")


saveRDS(tw_full,"data/w12_y_ord_5od.rds")
saveRDS(tw_full,"data/w34_y_ord_5od.rds")
saveRDS(tw_full,"data/w56_y_ord_5od.rds")
saveRDS(tw_full,"data/w78_y_ord_5od.rds")
saveRDS(tw_full,"data/w9_y_ord_5od.rds")
