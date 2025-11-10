####################################################################
############# Model fitting HMM ###################################
###################################################################
# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyverse)
#devtools::install_github("janoleko/LaMa")
library(LaMa)
library(ggplot2)
library(here)

data <- vroom(here("rawdata", "final_preprocessed_week_1_lag4.csv"))
for (i in 2:9) {
  data <- rbind(data, vroom(here("rawdata", paste0("final_preprocessed_week_",i,
                                                              "_lag4.csv"))))
}

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

obj2 <- MakeADFun(jnll, par, random = c("beta_club", "beta_pos"))
print("done")

# H <- obj2$env$spHess(random = TRUE)
# SparseM::image(H)

system.time(
  opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr)
)

mod2 <- obj2$report()

saveRDS(mod2, "./results/mod_w5to9.rds")

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

