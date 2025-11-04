################################################################################
######## Joint Preprocessing for Supervised and Unsupervised Approach ###########
################################################################################

### Load data and packages
library(dplyr)
library(tidyverse)
#devtools::install_github("janoleko/LaMa")
library(LaMa)
library(ggplot2)
library(here)
library(sp)
library(vroom)

# Data preprocessing ----------------------------------------------------------

# Set working directory and load data
players = read.csv(here("rawdata", "players.csv"))
plays = read.csv(here("rawdata","plays.csv"))
plays = plays |> filter(pff_manZone != "Other")

setwd("C:/Users/michels/sciebo/BDB 2025/rawdata")

tracking = vroom("tracking_week_1.csv")
nweeks = 9 # only 3 weeks for data size
for(jj in 2:nweeks){
  print(jj)
  #tracking = rbind(tracking, vroom(paste0("tracking_week_", i, ".csv")))
  setwd("C:/Users/michels/sciebo/BDB 2025/rawdata")
  tracking = vroom(paste0("tracking_week_", jj, ".csv"))
  
pre_process <- function(tracking_data){
  
  tracking_presnap1 = tracking_data %>% 
    mutate(uniId = paste0(gameId, playId, nflId)) %>% 
    mutate(game_play_id = paste0(gameId, playId)) %>% 
    mutate(y = y - 53.3/2) %>% # center y-coordinate
    mutate(x = x - 120/2) %>%  # center x-coordinate  
    mutate(displayName_fac = factor(displayName, levels = unique(displayName))) %>% 
    mutate(playId_fac = factor(playId, levels = unique(playId))) %>% 
    group_by(displayName_fac, playId_fac) %>% #use group split to stay with the order
    group_split() %>% 
    # Take out the time where teams are in the huddle, i.e. before first line up
    map(~mutate(., initial_y = y[which(event == "line_set")[1]])) %>% 
    map(~mutate(., initial_x = x[which(event == "line_set")[1]])) %>% 
    bind_rows() %>% 
    group_by(playId_fac) %>% 
    group_split() %>% 
    map(~filter(.,frameId >= frameId[which(event == "line_set")[1]])) %>% 
    bind_rows() %>% 
    filter(frameType == "BEFORE_SNAP") %>% # Filter out for data before the snap, i.e. the time of the motion
    # Connect with player and players df to receive information
    left_join(., players %>% dplyr::select(nflId, position), by = "nflId") %>% 
    left_join(., plays %>% filter(offenseFormation != "JUMBO", 
                                  offenseFormation != "WILDCAT") %>% 
                dplyr::select(playId, gameId, absoluteYardlineNumber, possessionTeam, pff_manZone), 
              by = c("playId", "gameId")) %>% 
    mutate(off_def = ifelse(club == possessionTeam, 1, 0)) 
  # 1. Keep only plays with motion
  id_motion_plays =  tracking_presnap1 |> 
    filter(event == "man_in_motion") %>% pull(game_play_id)
  
  tracking_presnap1 = tracking_presnap1 %>% 
    filter(game_play_id %in% id_motion_plays)
  
  # Ensure that online 5 Oline are on the field and only one QB
  position_counts <- tracking_presnap1 %>%
    group_by(game_play_id) %>% 
    mutate(Count_all = n()/length(unique(nflId))) %>% 
    ungroup() %>% 
    group_by(game_play_id, position) %>% 
    summarise(Count = n(), .groups = "drop", Count_all = unique(Count_all)) %>%
    mutate(Count = Count/Count_all) %>% 
    filter(position %in% c("C", "G", "T")) %>% 
    group_by(game_play_id) %>% summarise(sum_oline = sum(Count)) %>% 
    filter(sum_oline > 5) %>% 
    pull(game_play_id)
  
  # Ensure that only one QB is on the field
  position_counts2 <- tracking_presnap1 %>%
    group_by(game_play_id) %>% 
    mutate(Count_all = n()/length(unique(nflId))) %>% 
    ungroup() %>% 
    group_by(game_play_id, position) %>% 
    summarise(Count = n(), .groups = "drop", Count_all = unique(Count_all)) %>%
    mutate(Count = Count/Count_all) %>% 
    filter(position == "QB", Count == 2) %>% 
    pull(game_play_id)
  
  valid_play_ids2 = tracking_presnap1 %>% group_by(game_play_id) %>% 
    summarise(Count = n(), .groups = "drop") %>% 
    mutate(invalid = ifelse(Count < 24, 1, 0)) %>% 
    filter(invalid != 1) %>% pull(game_play_id)
  
  # 3. Keep only relevant playids
  tracking_presnap1 <- tracking_presnap1 %>%
    filter(!(game_play_id %in% position_counts)) %>% 
    filter(!(game_play_id %in% position_counts2)) %>% 
    filter(game_play_id %in% valid_play_ids2)
  
  #############################################################################
  ############ From here on, we filter out players NOT plays ##################
  #############################################################################
  
  # Positional adjustment
  # Filter out the ball, offensive and defensive line and the QB
  tracking_presnap2 = tracking_presnap1 %>% 
    filter(!(position %in% c("T", "G", "C", NA, "QB", "NT", "DT", "DE")))
  
  # Filter out pass rushing LB and OLB
  tracking_presnap3 = tracking_presnap1 %>% 
    filter((position %in% c(NA, "OLB", "LB")))
  
  df_new <- tracking_presnap3 %>%
    filter(displayName == "football") %>%
    select(game_play_id, frameId, Football.x = x, Football.y = y) 
  
  df_new =  tracking_presnap3 |> filter(displayName != "football") |> 
    left_join(df_new,
              by = c("game_play_id", "frameId")
    ) 
  
  # Schwellenwerte
  distance_thresh_x <- 2    # LOS-Abstand
  
  df_pass_rusher <- df_new |> filter(event == "line_set") %>%
    mutate(
      # Kriterien
      near_los_x = abs(x - Football.x) <= distance_thresh_x,
      # Finales Flag
      is_pass_rusher = near_los_x
    )
  # extract ids of pass rushers
  pass_rusher_ids <- df_pass_rusher %>%
    filter(is_pass_rusher) %>%
    pull(uniId) %>%
    unique()
  
  tracking_presnap2 = tracking_presnap2 |> filter(!(uniId %in% pass_rusher_ids))
  
  # extract offensive players
  off_players <- tracking_presnap2 %>%
    filter(off_def == 1) %>%
    group_by(gameId, playId) %>%
    mutate(player_order = rank(initial_y, ties.method = "first")) %>%
    arrange(gameId, playId, player_order) %>%
    ungroup() %>% 
    select(-player_order) %>% 
    group_by(gameId, playId, time) %>%
    mutate(off_num = row_number()) %>% 
    # arrange(initial_y) %>% 
    ungroup()
  # keep only first 5 offensive players
  off_data = off_players %>% 
    select(gameId, playId, time, off_num, y, x, s) %>%
    pivot_wider(
      names_from = off_num,
      values_from = c(x, y, s),
      names_glue = "player{off_num}_{.value}" # name columns as player1_x, player1_y, etc.
    )
  
  # off_data <- tracking_presnap %>%
  #   filter(off_def == 1) %>%
  #   select(time, nflId, y) %>%
  #   pivot_wider(
  #     names_from = nflId,
  #     values_from = c(y),
  #     names_glue = "player{nflId}_{.value}"
  #   )
  
  # extract defensive data
  def_data <- tracking_presnap2 %>%
    group_by(gameId, playId) %>%
    mutate(player_order = rank(initial_y, ties.method = "first")) %>%
    arrange(gameId, playId, player_order) %>%
    select(-player_order)  %>% 
    filter(off_def == 0)
  
  # Daten zusammenführen
  data <- def_data %>%
    left_join(off_data, by = c("time", "playId", "gameId"))
  
  # # write code that switches the sign of all x coordinates if the playDirection is left
  # data = data %>% 
  #   mutate(x = ifelse(playDirection != "left", -x, x)) %>% 
  #   mutate(across(starts_with("player") & ends_with("x"), ~ ifelse(playDirection != "left", -., .))) |> 
  #   group_by(uniId) %>% #use group split to stay with the order
  #   group_split() %>%
  #   map(~mutate(., initial_x = x[which(event == "line_set")[1]])) %>% 
  #   bind_rows()
  
  # # filter out deep safeties
  # safety_id = data |> filter(event == "line_set") |> 
  #   group_by(gameId, playId) |> 
  #   mutate(minX = min(player1_x, player2_x, player3_x, 
  #                     player4_x, player5_x, na.rm = TRUE)) |>  
  #   filter(abs(initial_x - minX) > 10) |> 
  #   pull(uniId) |> unique()
  # 
  # data = data |> filter(!(uniId %in% safety_id))
  
  # Keep only plays with 5-8 defensive players
  right_ids = data |> group_by(game_play_id) |> 
    filter(event == "line_set") |> summarise(count = n()) |> 
    filter(count > 4, count < 9) |> pull(game_play_id)
  
  data = data |> filter(game_play_id %in% right_ids)
  
  return(data)
}

data = pre_process(tracking)
# Spiel 2022091110, playId 291 as primary example

data = data |> mutate(uniId = paste0(gameId, playId, nflId))
plays = plays %>%
  mutate(game_play_id = paste0(gameId, playId))
n_att = 5

# Now filter out the most behind players (safeties)
data |> filter(event == "line_set") |> group_by(game_play_id) |>
  summarise(count = n()) |> filter(count == 6) |> pull(game_play_id) -> ids_6

data |> filter(event == "line_set") |> group_by(game_play_id) |>
  summarise(count = n()) |> filter(count == 7) |> pull(game_play_id) -> ids_7

data |> filter(event == "line_set") |> group_by(game_play_id) |>
  summarise(count = n()) |> filter(count == 8) |> pull(game_play_id) -> ids_8

data %>%
  filter(event == "line_set", game_play_id %in% c(ids_6), position != "CB") %>%
  group_by(game_play_id) %>%
  slice_min(order_by = initial_x, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  pull(uniId) -> get_out_6

data %>%
  filter(event == "line_set", game_play_id %in% c(ids_7), position != "CB") %>%
  group_by(game_play_id) %>%
  slice_min(order_by = initial_x, n = 2, with_ties = FALSE) %>%
  ungroup() %>%
  pull(uniId) -> get_out_7


data %>%
  filter(event == "line_set", game_play_id %in% c(ids_8), position != "CB") %>%
  group_by(game_play_id) %>%
  slice_min(order_by = initial_x, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  pull(uniId) -> get_out_8

data = data |> filter(!(uniId %in% get_out_6)) |> filter(!(uniId %in% get_out_7)) |> 
  filter(!(uniId %in% get_out_8))

max_lag = 5

create_offender_lag <- function(df, lag_steps, max_lag) {
  res <- do.call(rbind, lapply(split(df, df$uniId), function(subdf) {
    n <- nrow(subdf)
    # Lead = Lag in Richtung Vergleich mit Angreifer
    subdf$player1_y <- c(rep(NA, lag_steps), subdf$player1_y[1:(n - lag_steps)])
    subdf$player2_y <- c(rep(NA, lag_steps), subdf$player2_y[1:(n - lag_steps)])
    subdf$player3_y <- c(rep(NA, lag_steps), subdf$player3_y[1:(n - lag_steps)])
    subdf$player4_y <- c(rep(NA, lag_steps), subdf$player4_y[1:(n - lag_steps)])
    subdf$player5_y <- c(rep(NA, lag_steps), subdf$player5_y[1:(n - lag_steps)])
    # erste max_lag Zeilen entfernen
    subdf[(max_lag+1):n, ]
  }))
  rownames(res) <- NULL
  res
}

data = create_offender_lag(data, 4, 5)

nflIds = data$game_play_id |> unique()
nflplayerIds = data$uniId |> unique()

gc()


# Pre-snap features -------------------------------------------------------
#setwd("C:/Users/michels/sciebo/BDB 2025/rawdata")
#tracking = vroom(paste0("tracking_week_", jj, ".csv"))

tracking = tracking %>% 
  filter(event == "line_set") |>
  mutate(gameplayid = paste0(gameId, playId)) |> 
  filter(gameplayid %in% nflIds)
  
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
  mutate(uniId = paste0(gameId, playId, nflId)) %>% 
  #mutate(game_play_id = paste0(gameId, playId)) %>% 
  mutate(displayName_fac = factor(displayName, levels = unique(displayName))) %>% 
  mutate(playId_fac = factor(playId, levels = unique(playId))) %>% 
  group_by(displayName_fac, playId_fac) %>% #use group split to stay with the order
  group_split() %>% 
  # Take out the time where teams are in the huddle, i.e. before first line up
  map(~mutate(., initial_y = y[which(event == "line_set")[1]])) %>% 
  bind_rows() %>% 
  group_by(playId_fac) %>% 
  group_split() %>% 
  map(~filter(.,frameId >= frameId[which(event == "line_set")[1]])) %>% 
  bind_rows() %>% 
  filter(frameType == "BEFORE_SNAP") %>% # Filter out for data before the snap, i.e. the time of the motion
  # Connect with player and players df to receive information
  left_join(., players %>% dplyr::select(nflId, position), by = "nflId") %>% 
  left_join(., plays %>% filter(offenseFormation != "JUMBO", 
                                offenseFormation != "WILDCAT") %>% 
              dplyr::select(playId, gameId, absoluteYardlineNumber, possessionTeam, pff_manZone), 
            by = c("playId", "gameId")) %>% 
  mutate(off_def = ifelse(club == possessionTeam, 1, 0)) 

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

trackingList = split(tracking_presnap1, tracking_presnap1$gameplayId)

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
    #gpId <- unique(x$gameplayId)
    #ids <- nflIds[which(nflIds$gameplayId == gpId),2:6]
    #x |> filter(nflId %in% ids | off_def == 1)
    x |> filter(uniId %in% nflplayerIds | off_def == 1)
    }) |>
  map(~mutate(., player = playernames)) %>% 
  bind_rows()

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

rm(tracking)
gc()

# Post motion features ----------------------------------------------------
setwd("C:/Users/michels/sciebo/BDB 2025/rawdata")
tracking = vroom(paste0("tracking_week_", jj, ".csv"))

tracking = tracking %>% 
  mutate(gameplayid = paste0(gameId, playId)) |> 
  filter(gameplayid %in% nflIds)

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
  mutate(off_def = ifelse(club == possessionTeam, 1, 
                          ifelse(displayName == "football",NA,0)))


rm(tracking)
gc()

trackingList = split(tracking_presnap1, tracking_presnap1$gameplayId)


dist_infos <- as.data.frame(do.call(rbind,trackingList |>
                                      map(\(tl1){
                                        tl_o <- tl1 |> 
                                          filter(off_def == 1)
                                        tl_d <- tl1 |> 
                                          filter(off_def == 0)
                                        
                                        dist_info_o <- as.data.frame(do.call(rbind,tl_o |>
                                                                               mutate(diffx = c(0,diff(x)),
                                                                                      diffy = c(0,diff(y)),
                                                                                      dists = sqrt(diffx^2+diffy^2)) |>
                                                                               group_by(nflId) |>
                                                                               group_map(~{
                                                                                 x_dists <- sum(abs(.x$diffx[-1]))
                                                                                 y_dists <- sum(abs(.x$diffy[-1]))
                                                                                 tot_dists <- sum(.x$dists[-1])
                                                                                 return(c(x_dists,y_dists,tot_dists))
                                                                               })))
                                        names(dist_info_o) <- c("x_dist","y_dist","dist")
                                        dist_info_d <- as.data.frame(do.call(rbind,tl_d |>
                                                                               mutate(diffx = c(0,diff(x)),
                                                                                      diffy = c(0,diff(y)),
                                                                                      dists = sqrt(diffx^2+diffy^2)) |>
                                                                               group_by(nflId) |>
                                                                               group_map(~{
                                                                                 x_dists <- sum(abs(.x$diffx[-1]))
                                                                                 y_dists <- sum(abs(.x$diffy[-1]))
                                                                                 tot_dists <- sum(.x$dists[-1])
                                                                                 return(c(x_dists,y_dists,tot_dists))
                                                                               })))
                                        names(dist_info_d) <- c("x_dist","y_dist","dist")
                                        
                                        max_x_o <- max(dist_info_o$x_dist)
                                        max_y_o <- max(dist_info_o$y_dist)
                                        tot_dist_o <- sum(dist_info_o$dist)
                                        
                                        max_x_d <- max(dist_info_d$x_dist)
                                        max_y_d <- max(dist_info_d$y_dist)
                                        tot_dist_d <- sum(dist_info_d$dist)
                                        
                                        return(c(unique(tl1$gameplayId),max_x_o,max_y_o,tot_dist_o,max_x_d,max_y_d,tot_dist_d))
                                      })))

names(dist_infos) <- c("gameplayId","max_x_o","max_y_o","tot_dist_o","max_x_d","max_y_d","tot_dist_d")

data = data |> filter(game_play_id %in% dist_infos$gameplayId) |> 
  filter(game_play_id %in% tw_full$gameplayId)

tw_full = tw_full |> filter(gameplayId %in% dist_infos$gameplayId) |> 
  filter(gameplayId %in% data$game_play_id)

dist_infos = dist_infos |> filter(gameplayId %in% tw_full$gameplayId) |> 
  filter(gameplayId %in% data$game_play_id)

write.csv(data, here("rawdata", paste0("final_preprocessed_week_", jj, 
                                       "_lag4.csv")), 
          row.names = FALSE)

saveRDS(tw_full, here("rawdata", paste0("week", jj, "_y_ord_od.rds")))

saveRDS(dist_infos, here("rawdata", paste0("week_", jj, "_dist_adder.rds")))

}
