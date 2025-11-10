


# Load data ---------------------------------------------------------------

full_data <- readRDS("data/preprocessed_HMM_data.rds")

######################
### clock manipulations


quarter_seconds <- function(timestr){
  st <- str_split(timestr,":")
  sapply(st,function(s){
    as.numeric(s[1])*60+as.numeric(s[2])
  })
}

games = read.csv("data/games.csv")
plays = read.csv("data/plays.csv") |>
  mutate(gameplayId = as.numeric(paste0(gameId, playId)))

fm_add <- full_data |>
  left_join(games |> select(gameId,week),by = "gameId") |>
  left_join(plays |> select(gameplayId,possessionTeam))

model_data_full <- fm_add |>
  filter(pff_manZone != "Other") |>
  mutate(half_seconds_remaining = if_else(quarter %in% c(1,3),
                                          quarter_seconds(gameClock)+15*60,
                                          quarter_seconds(gameClock))) |>
  select(-gameClock,-starts_with("nflId")) |>
  mutate(pff_manZone_num = if_else(pff_manZone == "Man",1,0)) |>
  select(-pff_manZone) 


##  All teams -----------------------------------------


teams <- unique(model_data_full$possessionTeam)

all_team_zvm <- list()
for(team in teams){
  
  cat(team," ")
  Team_plays <- model_data_full |> 
    filter(possessionTeam == team) |>
    select(-week,-possessionTeam)
  
  #prop.table(table(Team_plays$pff_manZone_num))
  
  NTeam_plays <- model_data_full |> 
    filter(possessionTeam != team) |>
    select(-week,-possessionTeam)
  
  #prop.table(table(NTeam_plays$pff_manZone_num))
  
  seedu <- 42
  
  Team_xy_npo <- zvm_models(NTeam_plays,Team_plays,
                            ml_type = "xgboost",
                            excl_vars = c("average","sum","nr_player_changes","average_ent",
                                          names(model_data_full)[grep("dist_fb",names(model_data_full))],
                                          names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))],
                                          max_x_o,max_y_o,tot_dist_o,
                                          max_x_d,max_y_d,tot_dist_d),
                            seed = seedu,
                            return_preds = TRUE,
                            verbose = FALSE)
  
  Team_xy_po <- zvm_models(NTeam_plays,Team_plays,
                           ml_type = "xgboost",
                           excl_vars = c(
                             names(model_data_full)[grep("dist_fb",names(model_data_full))],
                             names(model_data_full)[grep("def_o_qb_diff",names(model_data_full))]),
                           seed = seedu,
                           return_preds = TRUE,
                           verbose = FALSE)
  
  
  Team_res_tab <- data.frame(zvm_num = Team_plays$pff_manZone_num,
                             zvm = ifelse(Team_plays$pff_manZone_num == 1, "Man","Zone"),
                             pre_man_prob = Team_xy_npo$preds,
                             post_man_prob = Team_xy_po$preds,
                             pre_zone_prob = 1-Team_xy_npo$preds,
                             post_zone_prob = 1-Team_xy_po$preds
  )
  
  Team_zone <- Team_res_tab |> 
    filter(zvm == "Zone") |>
    select(pre_zone_prob,post_zone_prob) |>
    mutate(improvement = ifelse(pre_zone_prob < post_zone_prob,1,0))
  
  #table(Team_zone$improvement)
  
  Team_man <- Team_res_tab |> 
    filter(zvm == "Man") |>
    select(pre_man_prob,post_man_prob) |>
    mutate(improvement = ifelse(pre_man_prob < post_man_prob,1,0))
  
  #table(Team_man$improvement)
  
  all_team_zvm[[team]] <- list(zvm_prop_team = prop.table(table(Team_plays$pff_manZone_num)),
                               zvm_prop_others = prop.table(table(NTeam_plays$pff_manZone_num)),
                               pre_model = Team_xy_npo,
                               post_model = Team_xy_po,
                               team_results = Team_res_tab,
                               zone_table = Team_zone,
                               man_table = Team_man,
                               zone_improvement = table(Team_zone$improvement),
                               man_improvement = table(Team_man$improvement))
}

saveRDS(all_team_zvm,"data/teams_zvm_240925.rds")

# Results and plots ---------------------------------------------------------------

all_team_zvm <- readRDS("data/teams_zvm_030125.rds")

accs <- data.frame(teams = names(all_team_zvm),
                   pre_acc = sapply(all_team_zvm,\(x){x$pre_model$acc}),
                   post_acc = sapply(all_team_zvm,\(x){x$post_model$acc}))

AUCs <- data.frame(teams = names(all_team_zvm),
                   pre_auc = sapply(all_team_zvm,\(x){x$pre_model$auc}),
                   post_auc = sapply(all_team_zvm,\(x){x$post_model$auc}))


all_team_zvm$BUF$zone_table


diffsl <- lapply(all_team_zvm,\(x){
  zt <- x$zone_table |>
    mutate(diff = post_zone_prob-pre_zone_prob)
  mt <- x$man_table |>
    mutate(diff = post_man_prob-pre_man_prob)
  
  return(c(zt$diff,mt$diff))
})

diff_tab <- lapply(seq_along(diffsl), \(i){
  data.frame(diff = diffsl[[i]],team = names(diffsl)[[i]])
}) |>
  bind_rows()


library(ggtext)
nfl_analytics_theme <- function(..., base_size = 12) {
  theme(
    text = element_text(family = "Roboto", size = base_size, color = "black"),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 16,
                                  vjust = .02,
                                  hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.background = element_rect(fill = "#f7f7f7"),
    panel.border = element_blank(),
    legend.background = element_rect(color = "#F7F7F7"),
    legend.key = element_rect(color = "#F7F7F7"),
    legend.title = element_text(face = "bold"),
    legend.title.align = 0.5,
    strip.text = element_text(face = "bold"))
}


diff_tab |> 
  ggplot(aes(x = reorder(team, diff, FUN=median),y = diff,
             fill = reorder(team, diff, FUN=median),
             color = reorder(team, diff, FUN=median)))+
  geom_hline(yintercept = 0, col = "red", linetype = 2)+
  geom_boxplot() +
  #geom_jitter(alpha = 0.09)+
  nflplotR::scale_fill_nfl(alpha = 0.7) +
  nflplotR::scale_color_nfl(type = "secondary") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  nfl_analytics_theme() +
  ylab("Probability differences of post and pre motion models") +
  xlab("") +
  labs(title = "**Differences in pre and post motion probability for predicting the correct defensive scheme**",
       subtitle = "*NFL Teams*") +
  theme(axis.text.x = element_nfl_logo(size = 0.75))


diff_info_l <- lapply(all_team_zvm,\(x){
  zt <- x$zone_table |>
    mutate(diff = post_zone_prob-pre_zone_prob,
           scheme = "zone") |>
    rename(post_prob = post_zone_prob,
           pre_prob = pre_zone_prob)
  mt <- x$man_table |>
    mutate(diff = post_man_prob-pre_man_prob,
           scheme = "man") |>
    rename(post_prob = post_man_prob,
           pre_prob = pre_man_prob)
  
  return(rbind(zt,mt))
})

sapply(diff_info_l,nrow)[order(sapply(diff_info_l,nrow))]
lapply(diff_info_l,\(x) table(x$improvement))
lapply(diff_info_l,\(x) round(prop.table(table(x$improvement)),3))

diff_tab <- lapply(seq_along(diffsl), \(i){
  data.frame(diff = diffsl[[i]],team = names(diffsl)[[i]])
}) |>
  bind_rows()

sapply(diff_info_l,\(x){ x |> filter(diff > 0.1) |> nrow()})-sapply(diff_info_l,\(x){ x |> filter(diff < -0.1) |> nrow()})

up <- sapply(diff_info_l,\(x){ x |> filter(diff > 0.2) |> nrow()})
up[order(up)]

down <- sapply(diff_info_l,\(x){ x |> filter(diff < -0.2) |> nrow()})
down[order(down)]

teams <- nflreadr::load_teams(current = TRUE) %>%
  select(team_abbr, team_logo_wikipedia)

team_motion = data.frame(team = names(diff_info_l),
                         good = sapply(diff_info_l,\(x){ x |> filter(diff > 0.1) |> nrow()}),
                         bad = sapply(diff_info_l,\(x){ x |> filter(diff < -0.1) |> nrow()}),
                         n = sapply(diff_info_l,nrow)) |>
  left_join(teams, by = join_by(team == team_abbr)) |>
  mutate(pgood = good/n,
         pbad = bad/n)

library(ggimage)

team_motion |> 
  ggplot(aes(x = pbad, y = pgood)) +
  geom_image(aes(image = team_logo_wikipedia), asp = 16/9) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_comma()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_comma())+
  geom_abline(intercept = 0,col = "red")+
  ylim(0,0.3)+
  xlim(0,0.3)


team_info <- data.frame(team = names(diff_info_l),
                        n = sapply(diff_info_l,nrow),
                        n_improv = do.call(rbind,lapply(diff_info_l,\(x) table(x$improvement)))[,2],
                        prop_improv = do.call(rbind,lapply(diff_info_l,\(x) prop.table(table(x$improvement))))[,2]) |>
  left_join(teams, by = join_by(team == team_abbr)) |>
  rename(url = team_logo_wikipedia)

saveRDS(team_info,"team_tab_240925.rds")

library(reactable)
library(reactablefmtr)
library(scales)

make_pal <- function(value,col_low = "red",col_middle = "skyblue",col_high = "darkgreen",nq = 10){
  colourer <- col_quantile(
    palette = c(col_low,col_middle,col_high),
    domain = c(min(value), max(value)),
    n = nq)
  colourer(value)
}

team_pals <- team_info |> 
  mutate(col1 = make_pal(n),
         col2 = make_pal(n_improv),
         col3 = make_pal(prop_improv))

reactable(team_info |> select(url,n,n_improv,prop_improv),
          bordered = TRUE,
          defaultSorted = "prop_improv",
          defaultSortOrder = "desc",
          
          columns = list(
            url = colDef(
              name = "Team",
              maxWidth = 70,
              align = "center",
              cell = embed_img(height = 25, width = 40)
            ),
            n = colDef(
              name = "# Motions",
              format = colFormat(digits = 0),
              style = color_scales(team_pals, color_ref = "col1"),
              maxWidth = 90
            ),
            n_improv = colDef(
              name = "# Plays with improved probability of of predicting correct defensive scheme in post motion model",
              format = colFormat(digits = 0),
              style = color_scales(team_pals, color_ref = "col2")
            ),
            prop_improv = colDef(
              name = "Percentation of plays with improved probability of predicting correct defensive scheme in post motion model",
              format = colFormat(digits = 2),
              style = color_scales(team_pals, color_ref = "col3")
            )
            #   avg_per_tackle2 = colDef(
            #     name = "Avg PEP[2]",
            #     format = colFormat(digits = 2),
            #     style = color_scales(best_tacklers_u, color_ref = "col4"),
            #     maxWidth = 80
            #   )
          )
          
)
