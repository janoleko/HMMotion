#### BDB 2025 - Coverage model - data preprocessing ####

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(LaMa)
library(ggplot2)
library(here)
library(stringr)

# Data preprocessing ----------------------------------------------------------

# Set working directory and load data
players = read.csv(here("rawdata", "players.csv"))
plays = read.csv(here("rawdata","plays.csv"))

pre_process <- function(tracking_data){
  
  tracking_data = tracking_data %>% 
    mutate(game_play_id = paste0(gameId, playId))
  
  tracking_presnap1 = tracking_data %>% 
    mutate(uniId = paste0(gameId, playId, nflId)) %>% 
    #mutate(game_play_id = paste0(gameId, playId)) %>% 
    mutate(y = y - 53.3/2) %>% # center y-coordinate
    mutate(x = x - 120/2) %>%  # center x-coordinate  
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
  
  # write code that switches the sign of all x coordinates if the playDirection is left
  data = data %>% 
    mutate(x = ifelse(playDirection != "left", -x, x)) %>% 
    mutate(across(starts_with("player") & ends_with("x"), ~ ifelse(playDirection != "left", -., .))) |> 
    group_by(uniId) %>% #use group split to stay with the order
    group_split() %>%
    map(~mutate(., initial_x = x[which(event == "line_set")[1]])) %>% 
    bind_rows()
  
  # filter out deep safeties
  safety_id = data |> filter(event == "line_set") |> 
    group_by(gameId, playId) |> 
    mutate(minX = min(player1_x, player2_x, player3_x, 
                      player4_x, player5_x, na.rm = TRUE)) |>  
    filter(abs(initial_x - minX) > 10) |> 
    pull(uniId) |> unique()
  
  data = data |> filter(!(uniId %in% safety_id))
  
  # Keep only plays with 5-7 defensive players
  right_ids = data |> group_by(game_play_id) |> 
    filter(event == "line_set") |> summarise(count = n()) |> 
    filter(count > 4, count < 8) |> pull(game_play_id)
  
  data = data |> filter(game_play_id %in% right_ids)
  
  return(data)
}

data <- read.csv(here("rawdata/large", "final_preprocessed_all_weeks_lag4.csv"))

# smaller data
game_play_ids <- unique(data$game_play_id)
set.seed(134)
w <- sample(1:length(game_play_ids), 100)
data <- data[which(data$game_play_id %in% game_play_ids[w]), ] # smaller data set


# Prepwork for model fitting ----------------------------------------------

### Berechnung der Startverteilung für einen Verteidiger
# Input:
#   y_def: y-Koordinate des Verteidigers (ein Wert)
#   y_att: numerischer Vektor mit y-Koordinaten der Angreifer
#   alpha: Einflussparameter für Distanz (höher = stärkere Zuordnung)
# Output:
#   Vektor mit Wahrscheinlichkeiten über die Angreifer

# Anzahl Angreifer
n_att <- 5
alpha <- 1  # Einflussparameter für Abstand

# We initialize the defender-attacker assignments using a softmax over the negative vertical distances between players, 
# following standard probabilistic matching approaches (Bishop, 2006; Cuturi, 2013). The parameter𝛼
# controls the sharpness of the distribution, with higher values leading to almost deterministic assignments.
Deltas <- do.call(rbind, lapply(split(data, data$uniId)[as.character(unique(data$uniId))],
                                function(x){
                                  # y-Koordinate des Verteidigers
                                  y_def <- x$y[1]
                                  
                                  # Indizes der Angreifer-Spalten
                                  att_idx <- which(str_detect(names(x), "player"))[1] - 1 + n_att + 1:n_att
                                  
                                  # y-Koordinaten der Angreifer
                                  y_att <- x[1, att_idx]
                                  
                                  # Score = exp(-alpha * Abstand)
                                  scores <- exp(-alpha * abs(y_att - y_def))
                                  
                                  # Normalisieren auf Summe = 1
                                  scores / sum(scores)
                                }))


### Computing pairwise distances between attackers
Att_y <- as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + n_att + 1:n_att])

# Compute distances for each pair only
dist_mat <- matrix(NA, nrow = nrow(data), ncol = n_att * (n_att-1) / 2)
nms <- rep(NA, ncol(dist_mat))
m <- 1
for(i in 1:n_att) {
  for(j in 1:n_att) {
    if(j > i) {
      dist_mat[, m] <- abs(Att_y[, j] - Att_y[, i])
      nms[m] <- paste0("dist_y_", j, "_", i)
      m <- m + 1
    }
  }
}
colnames(dist_mat) <- nms

# for indexing, so that we only have one distance per pair (i,j)
dist_idx <- matrix(0, nrow = n_att, ncol = n_att)
dist_idx[lower.tri(dist_idx)] <- 1:ncol(dist_mat)
dist_idx <- dist_idx + t(dist_idx)
dist_idx <- as.vector(dist_idx)
dist_idx <- dist_idx[dist_idx != 0]

head(dist_mat[, dist_idx], 3)

rm(Att_y, pairs) # memory cleanup
gc()


# Model fitting -----------------------------------------------------------

data$club <- as.factor(data$club)
data$pos <- as.factor(data$position)
n_clubs <- length(unique(data$club))
n_pos <- length(unique(data$position))
data$club_num <- as.integer(data$club)
data$pos_num <- as.integer(data$pos)
data$play_num <- as.integer(as.factor(data$game_play_id))

dat = list(y_pos = data$y,
           X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + n_att + 1:n_att]),
           dist_mat = dist_mat,
           dist_idx = dist_idx,
           ID = data$uniId,
           n_clubs = n_clubs,
           club_num = data$club_num,
           pos_num = data$pos_num,
           n_att = n_att, 
           Delta = as.matrix(Deltas),
           play_num = data$play_num)

# Initialising random coefficients with nice names
beta_pos0 = rep(0, n_pos)
names(beta_pos0) <- unique(data$pos)
beta_club0 = rep(0, n_clubs)
names(beta_club0) <- unique(data$club)
beta_play0 = rep(0, length(unique(data$play_num)))
names(beta_play0) <- unique(data$game_play_id)

par = list(beta0 = -5,
           beta_dist = 0,
           beta_club = beta_club0,
           beta_pos = beta_pos0,
           beta_play = beta_play0,
           logsigma = log(2),
           logsigma_club = log(0.2),
           logsigma_pos = log(0.2),
           logsigma_play = log(0.1))

jnll = function(par){
  getAll(par, dat)
  
  # Linear predictor matrix
  Eta <- beta0 + 
    beta_dist * dist_mat[, dist_idx] + # distance covariate
    beta_club[club_num] + # team random effect
    beta_pos[pos_num] + # position random effect
    beta_play[play_num] # play random effect
  
  REPORT(beta0); REPORT(beta_dist)
  REPORT(beta_club); REPORT(beta_pos); REPORT(beta_play)
  
  Gamma <- tpm_g(Eta = Eta)
  
  sigma <- exp(logsigma); REPORT(sigma)
  sigma_club <- exp(logsigma_club); REPORT(sigma_club)
  sigma_pos <- exp(logsigma_pos); REPORT(sigma_pos)
  sigma_play <- exp(logsigma_play); REPORT(sigma_play)
  
  # Matrix of state-dependent densities
  logallprobs <- matrix(0, length(y_pos), n_att)
  ind <- which(!is.na(y_pos))
  for(j in 1:n_att){
    logallprobs[ind,j] = dnorm(y_pos, X[,j], sigma, log = TRUE)
  }
  
  # HMM likelihood
  nll <- -forward_g(Delta, Gamma, logallprobs, trackID = ID, logspace = TRUE)
  
  # Club random effect likelihood
  nll <- nll - sum(dnorm(beta_club, 0, sigma_club, log = TRUE))
  
  # Position random effect likelihood
  nll <- nll - sum(dnorm(beta_pos, 0, sigma_pos, log = TRUE))
  
  # Play random effect likelihood
  nll <- nll - sum(dnorm(beta_play, 0, sigma_play, log = TRUE))
  
  nll
}

TMB::config(tmbad.sparse_hessian_compress = TRUE)
TapeConfig(matmul = "plain") # essential to get sparsity in matrix mult

obj <- MakeADFun(jnll, par, 
                 random = c("beta_club", "beta_pos", "beta_play"))
print("done")

# H <- obj2$env$spHess(random = TRUE)
# SparseM::image(H)

system.time(
  opt <- nlminb(obj$par, obj$fn, obj$gr)
)

mod <- obj$report()
sdr <- sdreport(obj)
mod$sdr <- sdr

stateprobs <- stateprobs_g(mod = mod)