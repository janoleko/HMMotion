#### BDB 2025 - Coverage model - data preprocessing ####

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)
#devtools::install_github("janoleko/LaMa")
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
playIds <- unique(data$playId)
set.seed(134)
w <- sample(1:length(playIds), 200)
data <- data[which(data$playId %in% playIds[w]), ] # smaller data set

# Model fitting -----------------------------------------------------------
# Berechnung der Startverteilung für einen Verteidiger
# Input:
#   y_def: y-Koordinate des Verteidigers (ein Wert)
#   y_att: numerischer Vektor mit y-Koordinaten der Angreifer
#   alpha: Einflussparameter für Distanz (höher = stärkere Zuordnung)
# Output:
#   Vektor mit Wahrscheinlichkeiten über die Angreifer

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

# Compute pairwise distances between attackers
Att_y <- as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + n_att + 1:n_att])

# All ordered pairs i != j
pairs <- expand.grid(i = 1:n_att, j = 1:n_att)
pairs <- pairs[pairs$i != pairs$j, ]  # remove i == j

# Compute distances
dist_mat <- sapply(1:nrow(pairs), function(k) {
  i <- pairs$i[k]
  j <- pairs$j[k]
  abs(Att_y[, i] - Att_y[, j])
})

# Name the columns
colnames(dist_mat) <- paste0("dist_y_", pairs$i, "_", pairs$j)

head(dist_mat)



# Model fitting -----------------------------------------------------------

dat = list(y_pos = data$y,
           X = as.matrix(data[, which(str_detect(names(data),"player"))[1]-1 + n_att + 1:n_att]),
           dist_mat = dist_mat,
           ID = data$uniId,
           n_att = n_att, 
           Delta = as.matrix(Deltas))

par = list(beta = c(-4, 0),  # transition prob coefficients
           logsigma = log(1))

tpm_g3 <- function(Eta, byrow = FALSE) {
  K <- ncol(Eta)
  N <- 0.5 + sqrt(0.25 + K)
  Gamma <- AD(array(1, dim = c(N, N, nrow(Eta))))
  
  expEta <- exp(Eta)
  
  ## Loop over entries (stuff over time happens vectorised which speeds up the tape)
  col_ind <- 1
  for(i in seq_len(N)){ # loop over rows
    for(j in seq_len(N)){ # loop over columns
      if(j != i){ # only if non-diagonal
        if(byrow){
          Gamma[i, j, ] <- expEta[, col_ind]
        } else{
          Gamma[j, i, ] <- expEta[, col_ind]
        }
        col_ind = col_ind + 1
      }
    }
  }
  # Normalise rows to sum to 1
  for(i in seq_len(N)){
    # transposing is necessary because Gamma[i,,] / colSums(Gamma[i,,]) does not work as expected
    Gamma[i, , ] <- t(t(Gamma[i, , ]) / rowSums(t(Gamma[i, , ])))
  }
  Gamma
}

nll <- function(par){
  getAll(par, dat)
  
  Gamma <- tpm_g3(beta[1] + beta[2] * dist_mat); REPORT(beta)
  
  sigma <- exp(logsigma)
  
  Mu = X
  REPORT(Mu)
  REPORT(alpha)
  
  allprobs = matrix(1, length(y_pos), n_att)
  ind = which(!is.na(y_pos))
  for(j in 1:n_att){
    allprobs[ind,j] = dnorm(y_pos, Mu[,j], sigma)
  }
  
  -forward_g(Delta, Gamma, allprobs, trackID = ID)
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
# map <- list(eta = factor(diag_indices))

map <- NULL

obj = MakeADFun(nll, par, map = map)
opt = nlminb(obj$par, obj$fn, obj$gr)

mod = obj$report()
(Delta = mod$delta)
(Gamma = mod$Gamma)
allprobs = mod$allprobs
trackID = mod$trackID
(alpha = mod$alpha)

probs = stateprobs(mod = mod)
colnames(probs) = paste0("attacker_", 1:n_att)
probs = cbind(ID = trackID, probs)

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

res_df = results_df %>% group_by(gameId, playId) %>% 
  mutate(player_change = ifelse(num_changes > 0, 1, 0)) %>% 
  summarize(average = mean(num_changes),
            sum = sum(num_changes),
            nr_player_changes = sum(player_change), 
            average_sd = mean(sd),
            average_ent = mean(entropy))

res_df = merge(plays, res_df, by = c("gameId", "playId"))

res_df = res_df %>% filter(pff_manZone != "Other")

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
# and distance effect

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
           dist_mat = dist_mat,
           ID = data$uniId,
           n_clubs = n_clubs,
           club_num = data$club_num,
           pos_num = data$pos_num,
           n_att = n_att, 
           Delta = as.matrix(Deltas))

par = list(beta0 = -5,
           beta_dist = 0,
           beta_club = rep(0, n_clubs),
           beta_pos = rep(0, n_pos),
           logsigma = log(2),
           logsigma_club = log(0.1),
           logsigma_pos = log(0.1))

jnll = function(par){
  getAll(par, dat)
  
  # linear predictor
  Eta <- beta0 + beta_dist * dist_mat + # distance
    beta_club[club_num] + beta_pos[pos_num] # random effects
  Gamma <- tpm_g3(Eta); REPORT(beta)
  
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
  
  nll <- -forward_g(Delta, Gamma[,,club_num], allprobs,
                    trackID = ID, report = FALSE)
  
  REPORT(allprobs)
  
  # club random effect likelihood
  nll <- nll - sum(dnorm(beta_club, 0, sigma_club, log = TRUE))
  
  # pos random effect likelihood
  nll <- nll - sum(dnorm(beta_pos, 0, sigma_pos, log = TRUE))
  
  nll
}


TMB::config(tmbad.sparse_hessian_compress = TRUE)
TapeConfig(matmul = "plain") # essential to get sparsity in matrix mult

obj2 <- MakeADFun(jnll, par, random = c("beta_club", "beta_pos"))
print("done")

# H <- obj2$env$spHess(random = TRUE)
# SparseM::image(H)

system.time(
  opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr)
)

mod2 <- obj2$report()

sdr <- sdreport(obj2)

saveRDS(mod2, "./results/mod200_w_dist.rds")


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


which.min(gammas)
which.max(gammas)

data$club[which(data$club_num == 14)[1]]
data$club[which(data$club_num == 30)[1]]

dim(mod2$Gamma)
nrow(mod2$allprobs)
dim(mod2$delta)

# decoding muss in Schleife wie in likelihood passieren

