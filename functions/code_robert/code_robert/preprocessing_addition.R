######################################################################################
######################################################################################
#### BDB 2025 preprocessing: additional distances (post motion)
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
tracking = vroom("data/tracking_week_9.csv")
nweeks = 8 # only 3 weeks for data size
for(i in 8:nweeks){
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
  
saveRDS(dist_infos,"data/first_dist_adder.rds")
saveRDS(dist_infos,"data/second_dist_adder.rds")
saveRDS(dist_infos,"data/w56_dist_adder.rds")
saveRDS(dist_infos,"data/w78_dist_adder.rds")
saveRDS(dist_infos,"data/w9_dist_adder.rds")

