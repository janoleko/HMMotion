######################################################################################
######################################################################################
#### BDB 2025 modeling: zone vs man full modeling
######################################################################################
######################################################################################

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(pROC)
library(glmnet)
library(glmnetUtils)
library(ranger)
library(xgboost)

# ZVM modeling ---------------------------------------------------------------


### relevant link for xgboost: https://stats.stackexchange.com/questions/342552/what-is-the-binarylogistic-objective-function-in-xgboost

zvm_models <- function(model_data,
                       test_data,
                       ml_type = c("glmnet","rf","xgboost"),
                       full_cv = TRUE,
                       excl_vars = NULL,
                       param_grid = NULL,
                       seed = 123,
                       return_preds = FALSE,
                       ...){
  dta <- model_data |>
    select(-gameId,-playId,
           -gameplayId,-pff_passCoverage,-c({{excl_vars}}))
  
  dta_test <- test_data |>
    select(-gameId,-playId,
           -gameplayId,-pff_passCoverage,-c({{excl_vars}}))
  
  ml_type = match.arg(ml_type)
  
  if(ml_type == "glmnet"){
    set.seed(seed)
    if(full_cv){
      mods <- cva.glmnet(x = as.matrix(dta |> select(-pff_manZone_num)),y = dta$pff_manZone_num,family = binomial(),...)
      mins <- sapply(mods$modlist,\(x) min(x$cvm))
      modu <- mods$modlist[[which.min(mins)]]
    }else{
      modu <- cv.glmnet(x = as.matrix(dta |> select(-pff_manZone_num)),y = dta$pff_manZone_num,family = binomial(),...)
    }
    coefs <- coef(modu)
    newxu <- as.matrix(dta_test |> select(-pff_manZone_num))
    preds <- predict(modu,newx = newxu, s = "lambda.min",type = "response")
    out <- preds > 0.5 
    acc <- mean(out == dta_test$pff_manZone_num)
    
    m_roc <- roc(dta_test$pff_manZone_num,c(preds))
    m_auc <- auc(m_roc)
    #plot(m_lasso_roc)
    logloss <- -mean((dta_test$pff_manZone_num*log(preds)+(1-dta_test$pff_manZone_num)*log(1-preds)))
  }else if(ml_type == "rf"){
    modu <- ranger(pff_manZone_num ~ .,data = dta,
                   num.trees = 1000, importance = "permutation",...)  
    
    preds <- predict(modu,data = dta_test,type = "response")
    out <- preds$predictions > 0.5 
    acc <- mean(out == dta_test$pff_manZone_num)
    
    m_roc <- roc(dta_test$pff_manZone_num,preds_rf$predictions)
    m_auc <- auc(m_roc)
    #lines(m_rf_roc,col = "skyblue")
    logloss <- -mean((dta_test$pff_manZone_num*log(preds)+(1-dta_test$pff_manZone_num)*log(1-preds)))
  }else if(ml_type == "xgboost"){
    
    xgb_mat <- xgboost::xgb.DMatrix(as.matrix(dta |> select(-pff_manZone_num)),
                                    label = dta$pff_manZone_num)
    
    if(full_cv){
      if(is.null(param_grid)){
        param_grid <- expand_grid(eta = c(0.01,0.05,0.1,0.3,0.5,1),
                                  max_depth = c(1,3,5,10))
      }
      xgb_list <- list()
      set.seed(seed)
      for(i in 1:nrow(param_grid)){
        params <- list(eval_metric = c("logloss"),
                       objective = "binary:logistic",
                       eta = param_grid[i,1],
                       max_depth = param_grid[i,2],
                       colsample_bynode = 0.3)
        xgb_list[[i]] <- xgb.cv(params = params,data = xgb_mat,
                                nfold = 5,showsd = T, nrounds = 1500,
                                print_every_n = 100,early_stopping_rounds = 15,...)
        
      }
      cv_losses <- sapply(xgb_list,function(r){
        iter <- r$best_iteration
        r$evaluation_log[iter]$test_logloss_mean
      })
      bpi <- which.min(cv_losses)
      bp <- xgb_list[[bpi]]$params
      nr <- xgb_list[[bpi]]$best_iteration
      modu <- xgboost(xgb_mat, max_depth = param_grid[bpi,2], eta = param_grid[bpi,1],
                      nrounds = nr, gamma = 0.5,colsample_bynode = 0.3, objective = "binary:logistic",
                      ...)
    }else{
      modu <- xgboost(xgb_mat, ...)
    }
    
    xgb_test <- xgboost::xgb.DMatrix(as.matrix(dta_test |> select(-pff_manZone_num)),
                                     label = dta_test$pff_manZone_num)
    
    preds <- predict(modu,newdata = xgb_test)
    out <- as.numeric(preds > 0.5)
    acc <- mean(out == dta_test$pff_manZone_num)
    
    m_roc <- roc(dta_test$pff_manZone_num,preds)
    m_auc <- auc(m_roc)
    logloss <- -mean((dta_test$pff_manZone_num*log(preds)+(1-dta_test$pff_manZone_num)*log(1-preds)))
  }
  if(return_preds){
    return(list(model = modu,
                acc = acc,
                roc = m_roc,
                auc = m_auc,
                preds = preds,
                logloss = logloss,
                npred = ncol(dta)))
  }else{
    return(list(model = modu,
                acc = acc,
                roc = m_roc,
                auc = m_auc,
                #preds = preds,
                logloss = logloss,
                npred = ncol(dta)))
  }
  
}


# Load data ---------------------------------------------------------------


# premotion_data <- readRDS("data/w12_y_ord.rds")
# premotion_data <- rbind(premotion_data,readRDS("data/w34_y_ord.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w56_y_ord.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w78_y_ord.rds"))
# premotion_data <- rbind(premotion_data,readRDS("data/w9_y_ord.rds"))
premotion_data <- readRDS("data/w12_y_ord_5od.rds")
premotion_data <- rbind(premotion_data,readRDS("data/w34_y_ord_5od.rds"))
premotion_data <- rbind(premotion_data,readRDS("data/w56_y_ord_5od.rds"))
premotion_data <- rbind(premotion_data,readRDS("data/w78_y_ord_5od.rds"))
premotion_data <- rbind(premotion_data,readRDS("data/w9_y_ord_5od.rds"))

### naive distances
postmotion_adder <- readRDS("data/first_dist_adder.rds")
postmotion_adder <- rbind(postmotion_adder,readRDS("data/second_dist_adder.rds"))
postmotion_adder <- rbind(postmotion_adder,readRDS("data/w56_dist_adder.rds"))
postmotion_adder <- rbind(postmotion_adder,readRDS("data/w78_dist_adder.rds"))
postmotion_adder <- rbind(postmotion_adder,readRDS("data/w9_dist_adder.rds"))

#load("probs_4weeks.RData")
load("probs_all_weeks.RData")

calculate_entropy <- function(Z) {
  Z <- Z[Z > 0]  # Entferne 0-Werte
  entropy <- -sum(Z * log(Z))  # Berechne Entropie
  return(entropy)
}

analyze_data <- function(df) {
  # Spalten 2 bis 6 in numerische Werte umwandeln
  numeric_values <- apply(df[, 2:6], 2, function(col) as.numeric(as.character(col)))
  
  # Fehlende Werte (NA) behandeln, falls Umwandlung fehlschlägt
  if (anyNA(numeric_values)) {
    warning("Es gibt NA-Werte nach der Umwandlung. Bitte prüfen!")
  }
  #
  # if(nrow(numeric_values) > 3){
  # numeric_values = numeric_values[-c(1:2),] #delete first 5 rows that can happen due to wrong assignments
  # }
  
  sd = mean(apply(numeric_values, 2, sd))
  # Den größten Wert pro Zeile bestimmen
  max_values <- apply(numeric_values, 1, which.max)
  
  # Anzahl der Änderungen des größten Wertes berechnen
  changes <- sum(c(NA, diff(max_values)) != 0, na.rm = TRUE)
  
  # Berechne Zn(j, k): Den Anteil der Zeitpunkte, in denen jeder Angreifer gedeckt wurde
  Zn <- table(factor(max_values, levels = 1:5)) / nrow(df)
  
  # Berechne die defensive Entropie
  entropy <- calculate_entropy(Zn)
  
  # Ergebnis als Liste zurückgeben
  data.frame(sd = sd, num_changes = changes, entropy = entropy)
  
  #data.frame(#max_columns = paste(max_column_names, collapse = ", "),
  #num_changes = changes)
}

results_df <- do.call(rbind, lapply(probs, analyze_data))
results_df$gameId = str_sub(rownames(results_df), 1, 10)
results_df$playId = str_sub(rownames(results_df), 11, str_length(rownames(results_df))-5)
# results_df$nflId = str_sub(rownames(results_df), str_length(rownames(results_df))-4, str_length(rownames(results_df)))

res_df = results_df %>% group_by(gameId, playId) %>% 
  mutate(player_change = ifelse(num_changes > 0, 1, 0)) %>% 
  summarize(average = mean(num_changes),
            sum = sum(num_changes),
            nr_player_changes = sum(player_change), 
            average_sd = mean(sd),
            average_ent = mean(entropy)) |>
  mutate(gameplayId = as.numeric(paste0(gameId, playId)))

# nflIds <- as.data.frame(do.call(rbind,results_df %>% group_by(gameId, playId) %>% 
#   group_map(\(x,y){
#     c(as.numeric(paste0(unique(x$gameId),unique(x$playId))),as.numeric(x$nflId))
#   },.keep = TRUE))) 
# names(nflIds) <- c("gameplayId",paste0("def",1:5))
# 
# saveRDS(nflIds,"data/nflIds_def_per_play.rds")
# 
# load("res_df_all_weeks.RData")
# res_df <- res_df |>
#   mutate(gameplayId = as.numeric(paste0(gameId, playId))) |>
#   select(gameplayId,average,sum,nr_player_changes,ends_with("_ent"))

######################
#### combine data

# data_train <- premotion_data |> select(-nflId_football) |>
#   left_join(postmotion_adder,by = "gameplayId") |>
#   left_join(res_df |> ungroup() |> select(gameplayId,average,sum,nr_player_changes),by = "gameplayId")
# 
# data_train_s <- data_train |> na.omit()
# 
# data_test <- premotion_data2 |> select(-nflId_football) |>
#   left_join(postmotion_adder2,by = "gameplayId") |>
#   left_join(res_df |> ungroup() |> select(gameplayId,average,sum,nr_player_changes),by = "gameplayId")
# 
# data_test_s <- data_test |> na.omit()

full_data <- premotion_data |> #select(-nflId_football) |>
  left_join(postmotion_adder,by = "gameplayId") |>
  left_join(res_df |> ungroup() |> select(gameplayId,average,sum,nr_player_changes,ends_with("_ent")),by = "gameplayId") 

# full_data_s <- premotion_data |> select(-nflId_football) |>
#   left_join(postmotion_adder,by = "gameplayId") |>
#   left_join(res_df |> ungroup() |> select(gameplayId,average,sum,nr_player_changes,ends_with("_ent")),by = "gameplayId") |>
#   na.omit()


######################
### clock manipulations


quarter_seconds <- function(timestr){
  st <- str_split(timestr,":")
  sapply(st,function(s){
    as.numeric(s[1])*60+as.numeric(s[2])
  })
}


# model_data <- data_train |>
#   filter(pff_manZone != "Other") |>
#   mutate(half_seconds_remaining = if_else(quarter %in% c(1,3),
#                                           quarter_seconds(gameClock)+15*60,
#                                           quarter_seconds(gameClock))) |>
#   select(-gameClock,-starts_with("nflId")) |>
#   mutate(pff_manZone_num = if_else(pff_manZone == "Man",1,0)) |>
#   select(-pff_manZone)
# 
# test_data <- data_test |>
#   filter(pff_manZone != "Other") |>
#   mutate(half_seconds_remaining = if_else(quarter %in% c(1,3),
#                                           quarter_seconds(gameClock)+15*60,
#                                           quarter_seconds(gameClock))) |>
#   select(-gameClock,-starts_with("nflId")) |>
#   mutate(pff_manZone_num = if_else(pff_manZone == "Man",1,0)) |>
#   select(-pff_manZone)
# 
# model_data_s <- full_data_s |>
#   filter(pff_manZone != "Other") |>
#   mutate(half_seconds_remaining = if_else(quarter %in% c(1,3),
#                                           quarter_seconds(gameClock)+15*60,
#                                           quarter_seconds(gameClock))) |>
#   select(-gameClock,-starts_with("nflId")) |>
#   mutate(pff_manZone_num = if_else(pff_manZone == "Man",1,0)) |>
#   select(-pff_manZone) |>
#   head(3339) ### Caution this is manually adjusted... (4173*0.8)
# 
# test_data_s <- full_data_s |>
#   filter(pff_manZone != "Other") |>
#   mutate(half_seconds_remaining = if_else(quarter %in% c(1,3),
#                                           quarter_seconds(gameClock)+15*60,
#                                           quarter_seconds(gameClock))) |>
#   select(-gameClock,-starts_with("nflId")) |>
#   mutate(pff_manZone_num = if_else(pff_manZone == "Man",1,0)) |>
#   select(-pff_manZone) |>
#   tail(nrow(full_data)-3339)

model_data <- full_data |>
  filter(pff_manZone != "Other") |>
  mutate(half_seconds_remaining = if_else(quarter %in% c(1,3),
                                          quarter_seconds(gameClock)+15*60,
                                          quarter_seconds(gameClock))) |>
  select(-gameClock,-starts_with("nflId")) |>
  mutate(pff_manZone_num = if_else(pff_manZone == "Man",1,0)) |>
  select(-pff_manZone) |>
  head(3339) ### Caution this is manually adjusted... (4173*0.8)

test_data <- full_data |>
  filter(pff_manZone != "Other") |>
  mutate(half_seconds_remaining = if_else(quarter %in% c(1,3),
                                          quarter_seconds(gameClock)+15*60,
                                          quarter_seconds(gameClock))) |>
  select(-gameClock,-starts_with("nflId")) |>
  mutate(pff_manZone_num = if_else(pff_manZone == "Man",1,0)) |>
  select(-pff_manZone) |>
  tail(nrow(full_data)-3339)


# Models ---------------------------------------------------------------


seedu <- 12 # default 123!
 
# glmnet_full <- zvm_models(model_data,test_data,
#                          ml_type = "glmnet",
#                          excl_vars = c("average","sum","nr_player_changes"),
#                          trace.it = TRUE)

glmnet_full <- zvm_models(model_data,test_data,
                         ml_type = "glmnet",
                         trace.it = TRUE,
                         seed = seedu)

glmnet_full2 <- zvm_models(model_data,test_data,
                         ml_type = "glmnet",
                         excl_vars = c("average","sum","nr_player_changes","average_ent"),
                         trace.it = TRUE,
                         seed = seedu)

# glmnet_s <- zvm_models(model_data_s,test_data_s,
#                          ml_type = "glmnet",
#                          trace.it = TRUE)

glmnet_af_npo <- zvm_models(model_data,test_data,
                            ml_type = "glmnet",
                            excl_vars = c(
                              max_x_o,max_y_o,tot_dist_o,
                              max_x_d,max_y_d,tot_dist_d),
                            trace.it = TRUE,
                            seed = seedu)

glmnet_af_npo2 <- zvm_models(model_data,test_data,
                             ml_type = "glmnet",
                             excl_vars = c("average","sum","nr_player_changes","average_ent",
                                           max_x_o,max_y_o,tot_dist_o,
                                           max_x_d,max_y_d,tot_dist_d),
                             trace.it = TRUE,
                             seed = seedu)


glmnet_xy_npo <- zvm_models(model_data,test_data,
                         ml_type = "glmnet",
                         excl_vars = c(
                                  names(model_data)[grep("dist_fb",names(model_data))],
                                  names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                  max_x_o,max_y_o,tot_dist_o,
                                  max_x_d,max_y_d,tot_dist_d),
                         trace.it = TRUE,
                         seed = seedu)

glmnet_xy_npo2 <- zvm_models(model_data,test_data,
                         ml_type = "glmnet",
                         excl_vars = c("average","sum","nr_player_changes","average_ent",
                                  names(model_data)[grep("dist_fb",names(model_data))],
                                  names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                  max_x_o,max_y_o,tot_dist_o,
                                  max_x_d,max_y_d,tot_dist_d),
                         trace.it = TRUE,
                         seed = seedu)

# glmnet_xy_s <- zvm_models(model_data_s,test_data_s,
#                          ml_type = "glmnet",
#                          excl_vars = c(names(model_data)[grep("dist_fb",names(model_data))],
#                                   names(model_data)[grep("def_o_qb_diff",names(model_data))]),
#                          trace.it = TRUE)
# 
# glmnet_xy_s2 <- zvm_models(model_data_s,test_data_s,
#                          ml_type = "glmnet",
#                          excl_vars = c("average","sum","nr_player_changes",names(model_data)[grep("dist_fb",names(model_data))],
#                                   names(model_data)[grep("def_o_qb_diff",names(model_data))]),
#                          trace.it = TRUE)

xgb_full <- zvm_models(model_data,test_data,
                         ml_type = "xgboost",
                       seed = seedu)

xgb_full2 <- zvm_models(model_data,test_data,
                        ml_type = "xgboost",
                        excl_vars = c("average","sum","nr_player_changes","average_ent"),
                        seed = seedu)

# xgb_s <- zvm_models(model_data_s,test_data_s,
#                          ml_type = "xgboost")
# 
# xgb_s2 <- zvm_models(model_data_s,test_data_s,
#                     ml_type = "xgboost",
#                     excl_vars = c("average","sum","nr_player_changes"))

xgb_af_npo <- zvm_models(model_data,test_data,
                         ml_type = "xgboost",
                         excl_vars = c(
                           max_x_o,max_y_o,tot_dist_o,
                           max_x_d,max_y_d,tot_dist_d),
                         seed = seedu)

xgb_af_npo2 <- zvm_models(model_data,test_data,
                          ml_type = "xgboost",
                          excl_vars = c("average","sum","nr_player_changes","average_ent",
                                        max_x_o,max_y_o,tot_dist_o,
                                        max_x_d,max_y_d,tot_dist_d),
                          seed = seedu)


xgb_xy_npo <- zvm_models(model_data,test_data,
                         ml_type = "xgboost",
                         excl_vars = c(
                                  names(model_data)[grep("dist_fb",names(model_data))],
                                  names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                  max_x_o,max_y_o,tot_dist_o,
                                  max_x_d,max_y_d,tot_dist_d),
                         seed = seedu)

xgb_xy_npo2 <- zvm_models(model_data,test_data,
                        ml_type = "xgboost",
                        excl_vars = c("average","sum","nr_player_changes","average_ent",
                                 names(model_data)[grep("dist_fb",names(model_data))],
                                 names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                 max_x_o,max_y_o,tot_dist_o,
                                 max_x_d,max_y_d,tot_dist_d),
                        seed = seedu)


# xgb_xy_s <- zvm_models(model_data_s,test_data_s,
#                          ml_type = "xgboost",
#                          excl_vars = c(names(model_data_s)[grep("dist_fb",names(model_data_s))],
#                                   names(model_data_s)[grep("def_o_qb_diff",names(model_data_s))]))
# 
# xgb_xy_s2 <- zvm_models(model_data_s,test_data_s,
#                         ml_type = "xgboost",
#                         excl_vars = c("average","sum","nr_player_changes",
#                                       names(model_data_s)[grep("ent",names(model_data_s))],
#                                       names(model_data_s)[grep("dist_fb",names(model_data_s))],
#                                       names(model_data_s)[grep("def_o_qb_diff",names(model_data_s))]))
# 
# glmnet_xy_s2 <- zvm_models(model_data_s,test_data_s,
#                         ml_type = "glmnet",
#                         full_cv = FALSE,
#                         excl_vars = c("average","sum","nr_player_changes",
#                                       names(model_data_s)[grep("ent",names(model_data_s))],
#                                       names(model_data_s)[grep("dist_fb",names(model_data_s))],
#                                       names(model_data_s)[grep("def_o_qb_diff",names(model_data_s))],
#                                       max_x_o,max_y_o,tot_dist_o,
#                                       max_x_d,max_y_d,tot_dist_d))
# 
# xgb_xy_s2.n <- zvm_models(model_data_s,test_data_s,
#                          ml_type = "xgboost",
#                          excl_vars = c("average","sum",
#                                   names(model_data)[grep("dist_fb",names(model_data))],
#                                   names(model_data)[grep("def_o_qb_diff",names(model_data))]))
# 
# # xgb_xy_s2.s <- zvm_models(model_data_s,test_data_s,
# #                          ml_type = "xgboost",
# #                          excl_vars = c("average","nr_player_changes",
# #                                   names(model_data)[grep("dist_fb",names(model_data))],
# #                                   names(model_data)[grep("def_o_qb_diff",names(model_data))]))
# # 
# # xgb_xy_s2.a <- zvm_models(model_data_s,test_data_s,
# #                         ml_type = "xgboost",
# #                         excl_vars = c("sum","nr_player_changes",
# #                                       names(model_data)[grep("dist_fb",names(model_data))],
# #                                       names(model_data)[grep("def_o_qb_diff",names(model_data))]))
# 
# xgb_xy_s2.efull <- zvm_models(model_data_s,test_data_s,
#                         ml_type = "xgboost",
#                         excl_vars = c("sum","nr_player_changes","average",
#                                       names(model_data)[grep("dist_fb",names(model_data))],
#                                       names(model_data)[grep("def_o_qb_diff",names(model_data))]))
# 
# xgb_xy_s2.ea <- zvm_models(model_data_s,test_data_s,
#                         ml_type = "xgboost",
#                         excl_vars = c("sum","nr_player_changes","average",
#                                       "med_ent","max_ent","min_ent",
#                                       names(model_data)[grep("dist_fb",names(model_data))],
#                                       names(model_data)[grep("def_o_qb_diff",names(model_data))]))
# 
# glmnet_xy_s2.ea <- zvm_models(model_data_s,test_data_s,
#                         ml_type = "glmnet",
#                         full_cv = FALSE,
#                         excl_vars = c("sum","nr_player_changes","average",
#                                       "med_ent","max_ent","min_ent",
#                                       names(model_data)[grep("dist_fb",names(model_data))],
#                                       names(model_data)[grep("def_o_qb_diff",names(model_data))],
#                                       max_x_o,max_y_o,tot_dist_o,
#                                       max_x_d,max_y_d,tot_dist_d))
# 
# xgb_xy_s2.ea2 <- zvm_models(model_data_s,test_data_s,
#                         ml_type = "xgboost",
#                         excl_vars = c("sum","nr_player_changes","average",
#                                       "med_ent","max_ent","min_ent",
#                                       names(model_data)[grep("dist_fb",names(model_data))],
#                                       names(model_data)[grep("def_o_qb_diff",names(model_data))],
#                                       max_x_o,max_y_o,tot_dist_o,
#                                       max_x_d,max_y_d,tot_dist_d))
# 
# xgb_xy_s2.woepm <- zvm_models(model_data_s,test_data_s,
#                         ml_type = "xgboost",
#                         excl_vars = c("sum","nr_player_changes","average",
#                                       "med_ent","max_ent","min_ent","average_ent",
#                                       names(model_data)[grep("dist_fb",names(model_data))],
#                                       names(model_data)[grep("def_o_qb_diff",names(model_data))],
#                                       max_x_o,max_y_o,tot_dist_o,
#                                       max_x_d,max_y_d,tot_dist_d))


# Analyses ---------------------------------------------------------------



## Tables -------------------

glm_all_mods <- list(glmnet_full,glmnet_full2,
                     glmnet_xy_af_npo,glmnet_xy_af_npo2, ## not xy atm!!
                     glmnet_xy_npo,glmnet_xy_npo2)
names(glm_all_mods) <- paste0("glm_",c("af","af_ex_hmm",
                                       "af_npo","af_npo_ex_hmm",
                                       "xy_npo","xy_npo_ex_hmm"))

glm_accs <- sapply(glm_all_mods,"[[",2)
glm_aucs <- sapply(glm_all_mods,"[[",4)
glm_lls <- sapply(glm_all_mods,"[[",5)

xgb_all_mods <- list(xgb_full,glmnet_full2,
                     xgb_xy_af_npo,xgb_xy_af_npo2, ## not xy atm!!
                     xgb_xy_npo,xgb_xy_npo2)
names(xgb_all_mods) <- paste0("xgb_",c("af","af_ex_hmm",
                                       "af_npo","af_npo_ex_hmm",
                                       "xy_npo","xy_npo_ex_hmm"))

xgb_accs <- sapply(xgb_all_mods,"[[",2)
xgb_aucs <- sapply(xgb_all_mods,"[[",4)
xgb_lls <- sapply(xgb_all_mods,"[[",5)


########


full_accs <- as.data.frame(rbind(glm_accs,xgb_accs))
full_accs$mod <- c("glmnet","xgboost")
names(full_accs) <- c("af","af_ex_hmm",
                      "af_npo","af_npo_ex_hmm",
                      "xy_npo","xy_npo_ex_hmm","mod")

full_aucs <- as.data.frame(rbind(glm_aucs,xgb_aucs))
full_aucs$mod <- c("glmnet","xgboost")
names(full_aucs) <- c("af","af_ex_hmm",
                      "af_npo","af_npo_ex_hmm",
                      "xy_npo","xy_npo_ex_hmm","mod")

full_lls <- as.data.frame(rbind(glm_lls,xgb_lls))
full_lls$mod <- c("glmnet","xgboost")
names(full_lls) <- c("af","af_ex_hmm",
                      "af_npo","af_npo_ex_hmm",
                      "xy_npo","xy_npo_ex_hmm","mod")

full_accs
full_aucs
full_lls

## Plots -------------------


plot(xgb_xy_s$roc, col = "lightblue")
lines(xgb_s$roc, col = "blue")
lines(xgb_xy_s2$roc, col = "darkgreen")
lines(xgb_s2$roc, col = "green")
lines(xgb_xy_s2.n$roc, col = "red")
lines(xgb_xy_s2.s$roc, col = "orange")
lines(xgb_xy_s2.a$roc, col = "yellow")

xgb_xy_s$auc
xgb_s$auc
xgb_xy_s2$auc
xgb_s2$auc
xgb_xy_s2.n$auc
xgb_xy_s2.s$auc
xgb_xy_s2.a$auc


xgb_xy_s2
xgb_xy_s2tt
xgb_xy_s2.ea
xgb_xy_s2.eatt



plot(xgb_xy_s2$roc, col = "lightblue")
lines(xgb_xy_s2tt$roc, col = "blue")
lines(xgb_xy_s2.ea$roc, col = "darkgreen")
lines(xgb_xy_s2.eatt$roc, col = "green")
