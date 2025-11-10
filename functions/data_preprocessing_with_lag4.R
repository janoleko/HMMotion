#### BDB 2025 - Coverage model - data preprocessing ####

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)
#devtools::install_github("janoleko/LaMa")
library(LaMa)
library(ggplot2)
library(here)

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

# tracking_data = (read.csv(here("rawdata", paste0("tracking_week_", 1, ".csv"))))
# 
# # # Read in tracking data and directly preprocess to save memory for all weeks
# all_tracking_data = pre_process(read.csv(here("rawdata", paste0("tracking_week_", 1, ".csv"))))
# # 
#  for (week in 2:9) {
#    tracking_data = pre_process(read.csv(here("rawdata", 
#                                              paste0("tracking_week_", week, ".csv"))))
#    all_tracking_data = bind_rows(all_tracking_data, tracking_data)
#  }

#write.csv(all_tracking_data, here("rawdata", "full_tracking_data_preprocessed_w1.csv"), row.names = FALSE)

# data = read.csv(here("rawdata/large", "full_tracking_data_preprocessed_w1to4.csv"))
# # Spiel 2022091110, playId 291 as primary example
# 
# data = data |> mutate(uniId = paste0(gameId, playId, nflId))
# plays = plays %>% 
#   mutate(game_play_id = paste0(gameId, playId)) 
# n_att = 5
# 
# 
# # Fit model ---------------------------------------------------------------
# #data = tracking_data #|> filter(gameId == 2022091110) #|> filter(playId == 291)
# 
# # Now filter out the most behind players (safeties)
# data |> filter(event == "line_set") |> group_by(game_play_id) |> 
#   summarise(count = n()) |> filter(count == 6) |> pull(game_play_id) -> ids_6
# 
# data |> filter(event == "line_set") |> group_by(game_play_id) |> 
#   summarise(count = n()) |> filter(count == 7) |> pull(game_play_id) -> ids_7
# 
# # schreibe mir eine Funktion, die sich die ids_6 zum Event line_set anschaut und 
# # dann den Spieler rausschmeißt, der das kleinste y hat
# 
# data %>%
#   filter(event == "line_set", game_play_id %in% c(ids_6), position != "CB") %>%
#   group_by(game_play_id) %>%
#   slice_min(order_by = initial_x, n = 1, with_ties = FALSE) %>%
#   ungroup() %>%
#   pull(uniId) -> get_out_6
# 
# data %>%
#   # Markiere die Zeilen, die raus sollen
#   filter(event == "line_set", game_play_id %in% c(ids_7), position != "CB") %>%
#   group_by(game_play_id) %>%
#   slice_min(order_by = initial_x, n = 2, with_ties = FALSE) %>%
#   ungroup() %>%
#   pull(uniId) -> get_out_7
# 
# data_fin = data |> filter(!(uniId %in% get_out_6)) |> filter(!(uniId %in% get_out_7))
# 
# 
# data = data_fin
# 
# write.csv(data, here("rawdata", "final_preprocessed_all_weeks.csv"), row.names = FALSE)

# data <- read.csv(here("rawdata", "final_preprocessed_all_weeks.csv"))
# data <- read.csv(here("rawdata", "full_tracking_data_preprocessed_w1to4.csv"))
#data <- read.csv(here("rawdata", "full_tracking_data_preprocessed_w1.csv"))

#max_lag <- 5

# create_defender_lag <- function(df, lag_steps, max_lag) {
#   res <- do.call(rbind, lapply(split(df, df$uniId), function(subdf) {
#     n <- nrow(subdf)
#     # Lead = Lag in Richtung Vergleich mit Angreifer
#     subdf$y_defender_lag <- c(subdf$y[(lag_steps + 1):n], rep(NA, lag_steps))
#     subdf$s_defender_lag <- c(subdf$s[(lag_steps + 1):n], rep(NA, lag_steps))
#     subdf$a_defender_lag <- c(subdf$a[(lag_steps + 1):n], rep(NA, lag_steps))
#     # Letzte max_lag Zeilen entfernen
#     subdf[1:(n - max_lag), ]
#   }))
#   rownames(res) <- NULL
#   res
# }

# create_offender_lag <- function(df, lag_steps, max_lag) {
#   res <- do.call(rbind, lapply(split(df, df$uniId), function(subdf) {
#     n <- nrow(subdf)
#     # Lead = Lag in Richtung Vergleich mit Angreifer
#     subdf$player1_y <- c(rep(NA, lag_steps), subdf$player1_y[1:(n - lag_steps)])
#     subdf$player2_y <- c(rep(NA, lag_steps), subdf$player2_y[1:(n - lag_steps)])
#     subdf$player3_y <- c(rep(NA, lag_steps), subdf$player3_y[1:(n - lag_steps)])
#     subdf$player4_y <- c(rep(NA, lag_steps), subdf$player4_y[1:(n - lag_steps)])
#     subdf$player5_y <- c(rep(NA, lag_steps), subdf$player5_y[1:(n - lag_steps)])
#     # erste max_lag Zeilen entfernen
#     subdf[(max_lag+1):n, ]
#   }))
#   rownames(res) <- NULL
#   res
# }

# # --- Lag 0: Original ohne Lag, letzte max_lag Zeilen entfernen ---
# df_no_lag <- do.call(rbind, lapply(split(data, data$uniId), function(subdf) {
#   n <- nrow(subdf)
#   # subdf$y_defender_lag <- subdf$y
#   # subdf$s_defender_lag <- subdf$s
#   # subdf$a_defender_lag <- subdf$a
#   subdf[(max_lag+1):n, ]
# }))
# rownames(df_no_lag) <- NULL
# 
# datasets <- list(lag_0 = df_no_lag)

# # --- Lag 1 bis Lag 5 ---
# for (i in 1:max_lag) {
#   lagged_df <- create_offender_lag(data, i, max_lag)
#   datasets[[paste0("lag_", i)]] <- lagged_df
# }
# 
# # Kontrolle: alle gleich groß?
# sapply(datasets, nrow)
# sapply(datasets, str)

#data = create_offender_lag(data, 4, 5)

#write.csv(data, here("rawdata", "final_preprocessed_all_weeks_lag4.csv"), row.names = FALSE)


data <- read.csv(here("rawdata/large", "final_preprocessed_all_weeks_lag4.csv"))

# Model fitting -----------------------------------------------------------
# Berechnung der Startverteilung für einen Verteidiger
# Input:
#   y_def: y-Koordinate des Verteidigers (ein Wert)
#   y_att: numerischer Vektor mit y-Koordinaten der Angreifer
#   alpha: Einflussparameter für Distanz (höher = stärkere Zuordnung)
# Output:
#   Vektor mit Wahrscheinlichkeiten über die Angreifer

library(stringr)

# Anzahl Angreifer
n_att <- 5
alpha <- 1  # Einflussparameter für Abstand

#We initialize the defender-attacker assignments using a softmax over the negative vertical distances between players, 
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

# Old Deltas --------------------------------------------------------------
## deterministic initial distribution
# Deltas =   t(
#   sapply((split(data, data$uniId))[as.character(unique(data$uniId))],
#          function(x){
#            delta = numeric(n_att)
#            delta[which.min(abs(x$y[1] - x[1, which(str_detect(names(x),"player"))[1]-1 + n_att + 1:n_att]))] = 1
#            delta
#          })
# )
# ## deterministic initial distribution
# # only defenders that are close to offensive players
# Delta2 = t(
#   sapply(split(data, data$uniId)[as.character(unique(data$uniId))],
#          function(x){
#            delta = numeric(n_att)
#            off_indice = which(str_detect(names(x),"player1_y"))
#            delta[which.min(abs(x$y[1] - x[1, off_indice[1]-1 + 1:n_att]))] = 
#              min(abs(x$y[1] - x[1, off_indice[1]-1 + 1:n_att]))
#            delta
#          })
# )# # only defenders that are close to offensive players
# 
# # weighted version
# # Idea: always select the defender closest to the attacker first until
# #library(clue)
# # each attacker is assigned
# Delta2 <- t(
#   sapply(split(data, data$uniId)[as.character(unique(data$uniId))],
#          function(x){
#            delta <- numeric(n_att)  # Initialize the result vectors
#            
#            # Extract the relevant columns
#            y_def <- x$y[1]  # Y-coordinate of the defender
#            x_def <- x$x[1]  # X-coordinate of the defender
#            
#            # Indices of attackers (columns with "player" in the name)
#            offender_indices_start <- which(str_detect(names(x), "player1_x"))
#            offender_indices = offender_indices_start + 0:(n_att-1)
#            # Calculate the weighted distances
#            distances <- sapply(offender_indices, function(i) {
#              y_off <- x[1,i+n_att]
#              x_off <- x[1,i]
#              
#              # Weighted distance calculation
#              wy <- 0.95  # Weight for the y-distance
#              wx <- 1-wy  # Weight for the x-distance
#              sqrt(wy * (y_off - y_def)^2 + wx * (x_off - x_def)^2)
#            }) %>% unlist()
#            
#            # Find the attacker with the minimum distance
#            #closest_defender <- which.min(distances)
#            delta =#[closest_defender] 
#              distances#[closest_defender]
#            delta
#          })
# )
# 
# rnames = rownames(Delta2)
# Delta2 = as.data.frame(Delta2)
# Delta2$game_play_id = str_sub(rownames(Delta2), 1, str_length(rownames(Delta2))-5)
# Delta2$rnames = rnames
# 
# # Hungarian Algorithm (alternative)
# # Delta3 <- #t(
# #   lapply(split(Delta2, Delta2$game_play_id)[as.character(unique(Delta2$game_play_id))],
# #          function(x){
# #   # Number of defenders (rows) and attackers (columns)
# #   n_defenders <- nrow(x)
# #   n_attackers <- n_att
# # 
# #   # Add dummy columns with high costs to make the matrix square
# #   if (n_defenders > n_attackers) {
# #     dummy_cols <- matrix(10^6, nrow = n_defenders, ncol = n_defenders - n_attackers)
# #     x <- cbind(x, dummy_cols)
# #   }
# #   
# #   game_play_id = x$game_play_id[1]
# #   rnames = x$rnames[1]
# #   x = x[,-c(6,7)] %>% as.matrix() 
# #   assignment <- solve_LSAP(x)
# # 
# #   # Display the result
# #   result <- data.frame(
# #     rnames = rownames(x),  # Defenders (rows)
# #     Attacker = paste0("player", assignment, "_y"),  # Assigned attackers (columns)
# #     Cost = x[cbind(1:nrow(x), assignment)]  # Distance
# #   )
# # 
# #   result = result[which(result$Cost < 10^5),]
# # 
# #   # One-hot encoding with model.matrix
# #   one_hot <- model.matrix(~ Attacker - 1, result)
# #   colnames(one_hot) = paste0("player", 1:5, "_y")
# # 
# #   # Add the one-hot columns to the original table
# #   data_one_hot <- cbind(result, one_hot) %>% dplyr::select(-c("Attacker", "Cost"))
# #          }
# #   )
# # 
# # Delta3 = bind_rows(Delta3)
# # 
# # rownames(Delta3) = Delta3$rnames
# # 
# 
# #Delta2 = Delta2[1:47,]
# Delta3 <- #t(
#   lapply(split(Delta2, Delta2$game_play_id)[as.character(unique(Delta2$game_play_id))],
#          function(distance_matrix){
#            # Number of attackers and defenders
#            n_defenders <- nrow(distance_matrix)
#            n_attackers <- n_att
#            
#            # Result list: attacker -> defender
#            assignments <- rep(NA, n_attackers)
#            
#            # Copy the matrix to modify it
#            matrix_copy <- distance_matrix[,1:n_att]
#            
#            # Repeat until all attackers are assigned
#            for (i in 1:n_att) {
#              # Find the global smallest distance
#              min_index <- which(matrix_copy == min(matrix_copy, na.rm = TRUE), arr.ind = TRUE)[1,]
#              
#              # Row and column of the minimum value
#              defender <- min_index[1]
#              attacker <- min_index[2]
#              
#              # Save the assignment
#              assignments[attacker] <- defender
#              
#              # Remove the row (defender) and column (attacker) from the matrix
#              matrix_copy[defender, ] <- NA  # Defender is no longer available
#              matrix_copy[, attacker] <- NA  # Attacker is assigned
#            }
#            
#            assignments2 = assignments[1:n_att]
#            
#            # Create the binary matrix based on the original order
#            binary_matrix <- matrix(0, nrow = n_defenders, ncol = n_att) %>% as.data.frame()
#            
#            # Set ones according to `row_indices`
#            for (i in seq_along(assignments2)) {
#              binary_matrix[assignments2[i], i] <- 1
#            }
#            
#            rownames(binary_matrix) <- rownames(distance_matrix)
#            colnames(binary_matrix) <- c(paste0("player", 1:5, "_y"))
#            
#            binary_matrix = binary_matrix[rowSums(binary_matrix[,1:5]) > 0, ]
#            
#            
#            return(binary_matrix)
#          }
#   )
# 
# Delta3 = bind_rows(Delta3)
# rnames_new = rownames(Delta3)
# 
# data = data %>% filter(uniId %in% rnames_new) 


# Compute pairwise distances between attackers
Att_y <- as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + n_att + 1:n_att])
pairs <- combn(n_att, 2)
# Compute pairwise distances
dist_mat <- sapply(1:ncol(pairs), function(k) {
  i <- pairs[1, k]
  j <- pairs[2, k]
  abs(Att_y[, i] - Att_y[, j])
})

# Assign meaningful column names
colnames(dist_mat) <- apply(pairs, 2, function(idx)
  paste0("dist_y_", idx[1], "_", idx[2])
)



# Model fitting -----------------------------------------------------------

dat = list(y_pos = data$y,
           X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + n_att + 1:n_att]),
           ID = data$uniId,
           n_att = n_att, 
           Delta = as.matrix(Deltas))

par = list(eta = rep(-4, n_att * (n_att - 1)),
           logsigma = log(1))

nll = function(par){
  getAll(par, dat)
  
  Gamma = tpm(eta)
  
  sigma = exp(logsigma)
  
  Mu = X
  REPORT(Mu)
  REPORT(alpha)
  
  allprobs = matrix(1, length(y_pos), n_att)
  ind = which(!is.na(y_pos))
  for(j in 1:n_att){
    allprobs[ind,j] = dnorm(y_pos, Mu[,j], sigma)
  }
  
  -forward(Delta, Gamma, allprobs, trackID = ID)
}

# # all off-diagonal probablities are the same
# map = list(eta = factor(rep(1, n_att * (n_att - 1))))

# Anzahl off-diagonal Elemente
n_off <- n_att * (n_att - 1)

# Wir wollen pro Diagonale einen eigenen Faktor (hier: 4 Diagonalen)
# Diagonal-Index = (Spalte - Zeile)
idx <- outer(1:n_att, 1:n_att, "-")  # Matrix mit Differenzen
idx <- as.vector(idx)

# Off-Diagonal-Indices herausfiltern
off_diag <- which(idx != 0)
diag_indices <- idx[off_diag]

# Diagonal-Shift auf 1..(n_att-1)
diag_indices <- abs(diag_indices)

# Faktor für Mapping
map <- list(eta = factor(diag_indices))
 

obj = MakeADFun(nll, par, map = map)
opt = nlminb(obj$par, obj$fn, obj$gr)

mod = obj$report()
(Delta = mod$delta)
(Gamma = mod$Gamma)
allprobs = mod$allprobs
trackID = mod$trackID
(alpha = mod$alpha)

probs = stateprobs(mod = mod)

mod = readRDS("C:/Users/michels/sciebo/BDB 2025/mod_full.rds")
probs = readRDS("C:/Users/michels/sciebo/BDB 2025/stateprobs_full.rds")
colnames(probs) = paste0("attacker_", 1:n_att)
#probs = cbind(ID = trackID, probs)
probs = cbind(ID = data$uniId, probs)

probs = (split(as.data.frame(probs), probs[,1]))[as.character(unique(probs[,1]))]
probs = lapply(probs, as.matrix)

# Function to calculate entropy
calculate_entropy <- function(Z) {
  Z <- Z[Z > 0]  # Remove zero values
  entropy <- -sum(Z * log(Z))  # Calculate entropy
  return(entropy)
}

analyze_data <- function(df) {
  # Convert columns 2 to 6 to numeric values
  numeric_values <- apply(df[, 2:6], 2, function(col) as.numeric(as.character(col)))
  
  # Handle missing values (NA) if conversion fails
  if (anyNA(numeric_values)) {
    warning("There are NA values after the conversion. Please check!")
  }
  #
  # if(nrow(numeric_values) > 3){
  # numeric_values = numeric_values[-c(1:2),] # Delete the first 5 rows that can occur due to incorrect assignments
  # }
  
  sd = mean(apply(numeric_values, 2, sd))
  # Determine the maximum value per row
  max_values <- apply(numeric_values, 1, which.max)
  
  # Calculate the number of changes in the maximum value
  changes <- sum(c(NA, diff(max_values)) != 0, na.rm = TRUE)
  
  # Calculate Zn(j, k): The proportion of time points where each attacker was covered
  Zn <- table(factor(max_values, levels = 1:5)) / nrow(df)
  
  # Calculate the defensive entropy
  entropy <- calculate_entropy(Zn)
  
  # Return results as a list
  data.frame(sd = sd, num_changes = changes, entropy = entropy)
}

results_df <- do.call(rbind, lapply(probs, analyze_data))
results_df$gameId = str_sub(rownames(results_df), 1, 10)
results_df$playId = str_sub(rownames(results_df), 11, str_length(rownames(results_df))-5)

save(results_df, file = "HMM_features_lag_4.rds")
save(probs, file = "stateprobs_with_ids.rds")

res_df = results_df %>% group_by(gameId, playId) %>% 
  mutate(player_change = ifelse(num_changes > 0, 1, 0)) %>% 
  summarize(average = mean(num_changes),
            sum = sum(num_changes),
            nr_player_changes = sum(player_change), 
            average_sd = mean(sd),
            average_ent = mean(entropy))

res_df = merge(plays, res_df, by = c("gameId", "playId"))

res_df = res_df %>% filter(pff_manZone != "Other")

save(res_df, file = "HMM_features_lag_4_per_play.rds")


# Visualize the frequency of categories by group
ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = nr_player_changes, fill = pff_manZone)) +
  geom_bar(position = "dodge", aes(y = ..prop.., group = pff_manZone)) + # ..prop.. calculates relative frequency
  labs(title = "Relative Frequency of Categories by Group", 
       x = "Category", 
       y = "Relative Frequency") +
  scale_y_continuous(labels = scales::percent) + # Percent format on the y-axis
  theme_minimal()

ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = sum, fill = pff_manZone)) +
  geom_bar(position = "dodge", aes(y = ..prop.., group = pff_manZone)) + # ..prop.. calculates relative frequency
  labs(title = "Relative Frequency of Categories by Group", 
       x = "Category", 
       y = "Relative Frequency") +
  scale_y_continuous(labels = scales::percent) + # Percent format on the y-axis
  theme_minimal()

ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = average_sd, fill = pff_manZone)) +
  geom_histogram(aes(y = ..density..), position = "dodge", alpha = 0.7, binwidth = 0.01) +
  labs(title = "Relative Frequency of Categories by Group", 
       x = "Value of average_sd", 
       y = "Relative Frequency (Density)") +
  scale_y_continuous(labels = scales::percent) + # Percent format on the y-axis
  theme_minimal()

ggplot(res_df %>% filter(pff_manZone != "Other"), aes(x = average_ent, fill = pff_manZone)) +
  geom_histogram(aes(y = ..density..), position = "dodge", alpha = 0.7, binwidth = 0.01) +
  labs(title = "Relative Frequency of Categories by Group", 
       x = "Value of average_ent", 
       y = "Relative Frequency (Density)") +
  scale_y_continuous(labels = scales::percent) + # Percent format on the y-axis
  theme_minimal()

res_df$day = str_sub(res_df$gameId, 1, 8)
days = unique(res_df$day)
res_df = res_df %>% filter(day %in% days[7:11])

pl_data = res_df %>% filter(pff_manZone != "Other")
plot(pl_data$average_ent, as.factor(pl_data$pff_manZone))

par(mfrow = c(1,2))
boxplot(pl_data$average_ent[which(pl_data$pff_manZone != "Zone")], ylim = c(0,1))
boxplot(pl_data$average_ent[which(pl_data$pff_manZone == "Zone")], ylim = c(0,1))

mod_log = glm(as.factor(pff_manZone) ~ average_ent, data = pl_data, family = "binomial")
summary(mod_log)




# Model with random effects for teams -------------------------------------

# library(Matrix)

data$club <- as.factor(data$club)
data$pos <- as.factor(data$position)
n_clubs <- length(unique(data$club))
n_pos <- length(unique(data$position))
data$club_num <- as.integer(data$club)
data$pos_num <- as.integer(data$pos)
  
# modmat <- make_matrices(~ s(club, bs = "re") + s(pos, bs = "re"), data = data)
# Z <- Matrix(modmat$Z, sparse = TRUE)
# Z2 <- modmat$Z

dat = list(y_pos = data$y,
           X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + n_att + 1:n_att]),
           ID = data$uniId,
           n_clubs = n_clubs,
           club_num = data$club_num,
           pos_num = data$pos_num,
           n_att = n_att, 
           Delta = as.matrix(Deltas))

par = list(beta0 = -5,
           beta_club = rep(0, n_clubs),
           beta_pos = rep(0, n_pos),
           logsigma = log(2),
           logsigma_club = log(0.1),
           logsigma_pos = log(0.1))

jnll = function(par){
  getAll(par, dat)
  
  # beta <- t(c(beta0, beta_club, beta_pos))
  # beta <- beta[rep(1, n_att*(n_att-1)), ]
  
  # Gamma <- tpm_g(Z, beta, ad = TRUE)
  beta0 <- beta0[rep(1, n_att*(n_att-1))]

  Gamma <- AD(array(NaN, dim = c(n_att, n_att, n_clubs, n_pos)))
  for(i in 1:n_clubs) {
    for(j in 1:n_pos)
    Gamma[,,i,j] <- tpm(beta0 + beta_club[i] + beta_pos[j])
  }
  REPORT(Gamma)
  REPORT(beta_club)
  REPORT(beta_pos)
  
  sigma <- exp(logsigma); REPORT(sigma)
  sigma_club <- exp(logsigma_club); REPORT(sigma_club)
  sigma_pos <- exp(logsigma_pos); REPORT(sigma_pos)
  
  allprobs <- matrix(1, length(y_pos), n_att)
  ind <- which(!is.na(y_pos))
  for(j in 1:n_att){
    allprobs[ind,j] = dnorm(y_pos, X[,j], sigma)
  }
  
  # nll <- -forward_g(Delta, Gamma[,,club_num], allprobs, 
  #                   trackID = ID, report = FALSE)
  
  # nll <- -forward_g(Delta, Gamma, allprobs, trackID = ID)
  nll <- 0 
  uID <- unique(ID)
  i <- 1

  for(id in uID){
    idx <- which(ID == id)
    club_i <- club_num[idx[1]]
    pos_i <- pos_num[idx[1]]
    
    # logallprobs <- matrix(0, length(idx), n_att)
    # this_y <- y_pos[idx]
    # ind <- which(!is.na(this_y))
    # for(j in 1:n_att){
    #   logallprobs[ind,j] <- dnorm(this_y[ind], X[idx[ind],j], sigma, log = TRUE)
    # }
    
    # nll <- nll - forward(Delta[i, ], Gamma[,, club_i, pos_i], logallprobs, report = FALSE, logspace = TRUE)
    
    nll <- nll - forward(Delta[i, ], Gamma[,, club_i, pos_i], allprobs[idx, ], report = FALSE)
    i <- i + 1
  }
  
  REPORT(allprobs)
  
  # club random effect likelihood
  nll <- nll - sum(dnorm(beta_club, 0, sigma_club, log = TRUE))
  
  # pos random effect likelihood
  nll <- nll - sum(dnorm(beta_pos, 0, sigma_pos, log = TRUE))
  
  nll
}


TMB::config(tmbad.sparse_hessian_compress=TRUE)
TapeConfig(matmul="plain") ## unrelated, but seems beneficial when matrices a small

# obj2 <- MakeADFun(jnll, par, random = c("beta_club", "beta_pos"))
# print("done")
# 
# # H <- obj2$env$spHess(random = TRUE)
# # SparseM::image(H)
# 
# system.time(
#   opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr)
# )

# mod2 <- obj2$report()



# saveRDS(mod2, "./results/mod_full.rds")
# saveRDS(mod2, "./results/mod_w5to9.rds")

mod_full <- readRDS("./results/mod_full.rds")


# state decoding

stateprobs <- list()

uID <- unique(dat$ID)
i <- 1
for(id in uID){
  idx <- which(dat$ID == id)
  club_i <- dat$club_num[idx[1]]
  pos_i <- dat$pos_num[idx[1]]
  
  stateprobs[[i]] <- stateprobs(dat$Delta[i, ], mod_full$Gamma[,, club_i, pos_i], mod_full$allprobs[idx, ])
  i <- i + 1
}

# turn into matrix
stateprobs_mat <- do.call(rbind, stateprobs)
colnames(stateprobs_mat) <- paste0("attacker_", 1:n_att)
stateprobs_df <- as.data.frame(stateprobs_mat)

saveRDS(stateprobs_df, "./results/stateprobs_full.rds")
saveRDS(stateprobs_mat, "./results/stateprobs_full_mat.rds")



mod14 <- readRDS("./results/mod_w1to4.rds")
mod59 <- readRDS("./results/mod_w5to9.rds")

names(mod14$beta_pos) <- levels(data$pos)
names(mod59$beta_pos) <- levels(data$pos)

rbind(mod14$beta_pos, mod59$beta_pos)

names(mod14$beta_club) <- levels(data$club)
names(mod59$beta_club) <- levels(data$club)

rbind(mod14$beta_club, mod59$beta_club)

plot(mod14$beta_club, mod59$beta_club)
abline(0,1)

mod <- lm(mod59$beta_club ~ mod14$beta_club)

mod2$sigma_club
mod2$sigma_pos
Gamma <- mod2$Gamma

gammas <- Gamma[1,1,,]

summary(as.vector(gammas))


# sdr <- sdreport(obj2, ignore.parm.uncertainty = TRUE)
# par_est <- as.list(sdr, "Est")

# beta_club <- par_est$beta_club
beta_club <- mod2$beta_club
names(beta_club) <- levels(data$club)
# beta_pos <- par_est$beta_pos
beta_pos <- mod2$beta_pos
names(beta_pos) <- levels(data$pos)

mod2$beta_pos = beta_pos
mod2[beta_club] = beta_club
mod2$names_beta_club = names(beta_club)
mod2$names_beta_pos = names(beta_pos)

apply(gammas, 2, which.min)
apply(gammas, 2, which.max)

data$club[which(data$club_num == 28)[1]]
data$club[which(data$club_num == 5)[1]]

dim(mod2$Gamma)
nrow(mod2$allprobs)
dim(mod2$delta)

# decoding muss in Schleife wie in likelihood passieren

