######################################################################################
######################################################################################
#### HMMotion: data creation
######################################################################################
######################################################################################

# Load data ---------------------------------------------------------------

# premotion_data <- readRDS("data/w12_y_ord_5od.rds")
# premotion_data <- rbind(premotion_data,readRDS("data/w34_y_ord_5od.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w56_y_ord_5od.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w78_y_ord_5od.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w9_y_ord_5od.rds"))
premotion_data <- readRDS(here("rawdata", "week1_y_ord_od.rds"))
for (i in 2:9) {
  premotion_data <- rbind(premotion_data, readRDS(here("rawdata", 
                                                       paste0("week",i,"_y_ord_od.rds"))))
}


### naive distances
postmotion_adder <- readRDS(here("rawdata", "week_1_dist_adder.rds"))
for (i in 2:9) {
  postmotion_adder <- rbind(postmotion_adder, readRDS(here("rawdata", 
                                                       paste0("week_",i,"_dist_adder.rds"))))
}
# postmotion_adder <- rbind(postmotion_adder,readRDS("data/second_dist_adder.rds"))
# postmotion_adder <- rbind(postmotion_adder,readRDS("data/w56_dist_adder.rds"))
# postmotion_adder <- rbind(postmotion_adder,readRDS("data/w78_dist_adder.rds"))
# postmotion_adder <- rbind(postmotion_adder,readRDS("data/w9_dist_adder.rds"))

### HMM data
#load("stateprobs_with_ids.rds") ### needs load function because probably not save via saveRDS!
load("HMM_features_lag_4_per_play.rds") ### needs load function because probably not save via saveRDS!
#load("HMM_features_lag_4.rds") ### needs load function because probably not save via saveRDS!

res_df <- res_df |> 
  mutate(gameplayId = as.numeric(paste0(gameId, playId)))

full_data <- premotion_data |> #select(-nflId_football) |>
  left_join(postmotion_adder,by = "gameplayId") |>
  left_join(res_df |> ungroup() |> select(gameplayId,average,sum,nr_player_changes,ends_with("_ent")),by = "gameplayId") |>
  drop_na()


saveRDS(full_data,"data/preprocessed_HMM_data.rds")
