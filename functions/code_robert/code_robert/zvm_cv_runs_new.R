######################################################################################
######################################################################################
#### zone vs. man cross valid new
######################################################################################
######################################################################################


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(pROC)
library(glmnet)
library(glmnetUtils)
library(ranger)
library(xgboost)

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

model_data_full <- model_data_full |>
  select(-week,-possessionTeam)

# Modelling function ---------------------------------------------------------------

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


run_models <- function(seedu){
  
  n <- nrow(model_data_full)
  sample_idx <- sample(n, size = 0.85*n)
  model_data <- model_data_full[sample_idx,]  
  test_data <- model_data_full[-sample_idx,]
  
  
  # GLMNET -------------------------------------------------
  
  glmnet_full <- zvm_models(model_data,test_data,
                            ml_type = "glmnet",
                            #trace.it = TRUE,
                            seed = seedu)
  
  glmnet_full2 <- zvm_models(model_data,test_data,
                             ml_type = "glmnet",
                             excl_vars = c("average","sum","nr_player_changes","average_ent"),
                             #trace.it = TRUE,
                             seed = seedu)
  
  
  glmnet_af_npo <- zvm_models(model_data,test_data,
                              ml_type = "glmnet",
                              excl_vars = c(
                                max_x_o,max_y_o,tot_dist_o,
                                max_x_d,max_y_d,tot_dist_d),
                              #trace.it = TRUE,
                              seed = seedu)
  
  glmnet_af_npo2 <- zvm_models(model_data,test_data,
                               ml_type = "glmnet",
                               excl_vars = c("average","sum","nr_player_changes","average_ent",
                                             max_x_o,max_y_o,tot_dist_o,
                                             max_x_d,max_y_d,tot_dist_d),
                               #trace.it = TRUE,
                               seed = seedu)
  
  glmnet_xy <- zvm_models(model_data,test_data,
                          ml_type = "glmnet",
                          excl_vars = c(
                            names(model_data)[grep("dist_fb",names(model_data))],
                            names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                          #trace.it = TRUE,
                          seed = seedu)
  
  glmnet_xy2 <- zvm_models(model_data,test_data,
                           ml_type = "glmnet",
                           excl_vars = c("average","sum","nr_player_changes","average_ent",
                                         names(model_data)[grep("dist_fb",names(model_data))],
                                         names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                           #trace.it = TRUE,
                           seed = seedu)
  
  glmnet_xy_npo <- zvm_models(model_data,test_data,
                              ml_type = "glmnet",
                              excl_vars = c(
                                names(model_data)[grep("dist_fb",names(model_data))],
                                names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                max_x_o,max_y_o,tot_dist_o,
                                max_x_d,max_y_d,tot_dist_d),
                              #trace.it = TRUE,
                              seed = seedu)
  
  glmnet_xy_npo2 <- zvm_models(model_data,test_data,
                               ml_type = "glmnet",
                               excl_vars = c("average","sum","nr_player_changes","average_ent",
                                             names(model_data)[grep("dist_fb",names(model_data))],
                                             names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                             max_x_o,max_y_o,tot_dist_o,
                                             max_x_d,max_y_d,tot_dist_d),
                               #trace.it = TRUE,
                               seed = seedu)
  
  # XGBOOST -------------------------------------------------
  
  xgb_full <- zvm_models(model_data,test_data,
                         ml_type = "xgboost",
                         seed = seedu,
                         verbose = FALSE)
  
  xgb_full2 <- zvm_models(model_data,test_data,
                          ml_type = "xgboost",
                          excl_vars = c("average","sum","nr_player_changes","average_ent"),
                          seed = seedu,
                          verbose = FALSE)
  
  
  xgb_af_npo <- zvm_models(model_data,test_data,
                           ml_type = "xgboost",
                           excl_vars = c(
                             max_x_o,max_y_o,tot_dist_o,
                             max_x_d,max_y_d,tot_dist_d),
                           seed = seedu,
                           verbose = FALSE)
  
  xgb_af_npo2 <- zvm_models(model_data,test_data,
                            ml_type = "xgboost",
                            excl_vars = c("average","sum","nr_player_changes","average_ent",
                                          max_x_o,max_y_o,tot_dist_o,
                                          max_x_d,max_y_d,tot_dist_d),
                            seed = seedu,
                            verbose = FALSE)
  
  xgb_xy <- zvm_models(model_data,test_data,
                       ml_type = "xgboost",
                       excl_vars = c(
                         names(model_data)[grep("dist_fb",names(model_data))],
                         names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                       seed = seedu,
                       verbose = FALSE)
  
  xgb_xy2 <- zvm_models(model_data,test_data,
                        ml_type = "xgboost",
                        excl_vars = c("average","sum","nr_player_changes","average_ent",
                                      names(model_data)[grep("dist_fb",names(model_data))],
                                      names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                        seed = seedu,
                        verbose = FALSE)
  
  
  xgb_xy_npo <- zvm_models(model_data,test_data,
                           ml_type = "xgboost",
                           excl_vars = c(
                             names(model_data)[grep("dist_fb",names(model_data))],
                             names(model_data)[grep("def_o_qb_diff",names(model_data))],
                             max_x_o,max_y_o,tot_dist_o,
                             max_x_d,max_y_d,tot_dist_d),
                           seed = seedu,
                           verbose = FALSE)
  
  xgb_xy_npo2 <- zvm_models(model_data,test_data,
                            ml_type = "xgboost",
                            excl_vars = c("average","sum","nr_player_changes","average_ent",
                                          names(model_data)[grep("dist_fb",names(model_data))],
                                          names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                          max_x_o,max_y_o,tot_dist_o,
                                          max_x_d,max_y_d,tot_dist_d),
                            seed = seedu,
                            verbose = FALSE)
  
  # Result Tables -------------------------------------------------
  
  glm_all_mods <- list(glmnet_full,glmnet_full2,
                       glmnet_af_npo,glmnet_af_npo2, 
                       glmnet_xy,glmnet_xy2, 
                       glmnet_xy_npo,glmnet_xy_npo2)
  names(glm_all_mods) <- paste0("glm_",c("af","af_ex_hmm",
                                         "af_npo","af_npo_ex_hmm",
                                         "xy_f","xy_f_ex_hmm",
                                         "xy_npo","xy_npo_ex_hmm"))
  
  glm_accs <- sapply(glm_all_mods,"[[",2)
  glm_aucs <- sapply(glm_all_mods,"[[",4)
  glm_lls <- sapply(glm_all_mods,"[[",5)
  
  xgb_all_mods <- list(xgb_full,glmnet_full2,
                       xgb_af_npo,xgb_af_npo2, 
                       xgb_xy,xgb_xy2, 
                       xgb_xy_npo,xgb_xy_npo2)
  names(xgb_all_mods) <- paste0("xgb_",c("af","af_ex_hmm",
                                         "af_npo","af_npo_ex_hmm",
                                         "xy_f","xy_f_ex_hmm",
                                         "xy_npo","xy_npo_ex_hmm"))
  
  xgb_accs <- sapply(xgb_all_mods,"[[",2)
  xgb_aucs <- sapply(xgb_all_mods,"[[",4)
  xgb_lls <- sapply(xgb_all_mods,"[[",5)
  
  
  ########
  
  
  full_accs <- as.data.frame(rbind(glm_accs,xgb_accs))
  full_accs$mod <- c("glmnet","xgboost")
  names(full_accs) <- c("af","af_ex_hmm",
                        "af_npo","af_npo_ex_hmm",
                        "xy_f","xy_f_ex_hmm",
                        "xy_npo","xy_npo_ex_hmm","mod")
  
  full_aucs <- as.data.frame(rbind(glm_aucs,xgb_aucs))
  full_aucs$mod <- c("glmnet","xgboost")
  names(full_aucs) <- c("af","af_ex_hmm",
                        "af_npo","af_npo_ex_hmm",
                        "xy_f","xy_f_ex_hmm",
                        "xy_npo","xy_npo_ex_hmm","mod")
  
  full_lls <- as.data.frame(rbind(glm_lls,xgb_lls))
  full_lls$mod <- c("glmnet","xgboost")
  names(full_lls) <- c("af","af_ex_hmm",
                       "af_npo","af_npo_ex_hmm",
                       "xy_f","xy_f_ex_hmm",
                       "xy_npo","xy_npo_ex_hmm","mod")
  
  return(list(accuracy = full_accs,
              AUC = full_aucs,
              logloss = full_lls))
  
}

run_models_small <- function(seedu){
  
  n <- nrow(model_data_full)
  sample_idx <- sample(n, size = 0.85*n)
  model_data <- model_data_full[sample_idx,]  
  test_data <- model_data_full[-sample_idx,]
  
  
  # GLMNET -------------------------------------------------
  

  glmnet_xy <- zvm_models(model_data,test_data,
                          ml_type = "glmnet",
                          excl_vars = c(
                            names(model_data)[grep("dist_fb",names(model_data))],
                            names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                          #trace.it = TRUE,
                          seed = seedu)
  
  glmnet_xy2 <- zvm_models(model_data,test_data,
                           ml_type = "glmnet",
                           excl_vars = c("average","sum","nr_player_changes","average_ent",
                                         names(model_data)[grep("dist_fb",names(model_data))],
                                         names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                           #trace.it = TRUE,
                           seed = seedu)
  
  
  glmnet_xy_npo2 <- zvm_models(model_data,test_data,
                               ml_type = "glmnet",
                               excl_vars = c("average","sum","nr_player_changes","average_ent",
                                             names(model_data)[grep("dist_fb",names(model_data))],
                                             names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                             max_x_o,max_y_o,tot_dist_o,
                                             max_x_d,max_y_d,tot_dist_d),
                               #trace.it = TRUE,
                               seed = seedu)
  
  # XGBOOST -------------------------------------------------

  
  xgb_xy <- zvm_models(model_data,test_data,
                       ml_type = "xgboost",
                       excl_vars = c(
                         names(model_data)[grep("dist_fb",names(model_data))],
                         names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                       seed = seedu,
                       verbose = FALSE)
  
  xgb_xy2 <- zvm_models(model_data,test_data,
                        ml_type = "xgboost",
                        excl_vars = c("average","sum","nr_player_changes","average_ent",
                                      names(model_data)[grep("dist_fb",names(model_data))],
                                      names(model_data)[grep("def_o_qb_diff",names(model_data))]),
                        seed = seedu,
                        verbose = FALSE)
  
  
  
  xgb_xy_npo2 <- zvm_models(model_data,test_data,
                            ml_type = "xgboost",
                            excl_vars = c("average","sum","nr_player_changes","average_ent",
                                          names(model_data)[grep("dist_fb",names(model_data))],
                                          names(model_data)[grep("def_o_qb_diff",names(model_data))],
                                          max_x_o,max_y_o,tot_dist_o,
                                          max_x_d,max_y_d,tot_dist_d),
                            seed = seedu,
                            verbose = FALSE)
  
  # Result Tables -------------------------------------------------
  
  glm_all_mods <- list(glmnet_xy,glmnet_xy2,glmnet_xy_npo2)
  names(glm_all_mods) <- paste0("glm_",c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm"))
  
  glm_accs <- sapply(glm_all_mods,"[[",2)
  glm_aucs <- sapply(glm_all_mods,"[[",4)
  glm_lls <- sapply(glm_all_mods,"[[",5)
  
  xgb_all_mods <- list(xgb_xy,xgb_xy2,xgb_xy_npo2)
  names(xgb_all_mods) <- paste0("xgb_",c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm"))
  
  xgb_accs <- sapply(xgb_all_mods,"[[",2)
  xgb_aucs <- sapply(xgb_all_mods,"[[",4)
  xgb_lls <- sapply(xgb_all_mods,"[[",5)
  
  
  ########
  
  
  full_accs <- as.data.frame(rbind(glm_accs,xgb_accs))
  full_accs$mod <- c("glmnet","xgboost")
  names(full_accs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  
  full_aucs <- as.data.frame(rbind(glm_aucs,xgb_aucs))
  full_aucs$mod <- c("glmnet","xgboost")
  names(full_aucs) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  
  full_lls <- as.data.frame(rbind(glm_lls,xgb_lls))
  full_lls$mod <- c("glmnet","xgboost")
  names(full_lls) <- c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm","mod")
  
  return(list(accuracy = full_accs,
              AUC = full_aucs,
              logloss = full_lls))
  
}

nsim = 50
seeds <- round(seq(1,1000,length.out = nsim))

results_list <- list()
for(i in 1:nsim){
  seedu = seeds[i]
  cat("\n\n")
  cat("Iteration ",i)
  cat("\n Seed: ",seedu)
  cat("\n\n")
  
  results_list[[i]] <- run_models_small(seedu)
}


#saveRDS(results_list,"data/zvm_cv_runs_240925_full_n14.rds")
saveRDS(results_list,"data/zvm_cv_runs_240925_small.rds")


# Results ---------------------------------------------------------------

results_list <- readRDS("data/zvm_cv_runs_240925_small.rds")

#### tables and averages

accs <- do.call(rbind,lapply(results_list,"[[",1))
accs_glmnet <- accs |> filter(mod == "glmnet") |> select(-mod)
accs_xgb <- accs |> filter(mod == "xgboost") |> select(-mod)

#apply(accs_glmnet,2,mean)
colMeans(accs_glmnet)
colMeans(accs_xgb)


aucs <- do.call(rbind,lapply(results_list,"[[",2))
aucs_glmnet <- aucs |> filter(mod == "glmnet") |> select(-mod)
aucs_xgb <- aucs |> filter(mod == "xgboost") |> select(-mod)

#apply(aucs_glmnet,2,mean)
colMeans(aucs_glmnet)
colMeans(aucs_xgb)

lls <- do.call(rbind,lapply(results_list,"[[",3))
lls_glmnet <- lls |> filter(mod == "glmnet") |> select(-mod)
lls_xgb <- lls |> filter(mod == "xgboost") |> select(-mod)

colMeans(lls_glmnet)
colMeans(lls_xgb)


#### plots

col_vals <- c("#38761d","yellowgreen","gray50")
#names(col_vals) <- c("HMM","post","pre")

accsl <- accs |> pivot_longer(-mod)

accs_bdb <- accsl |>
  filter(name %in% c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm")) |>
  mutate(display_name = ifelse(name == "xy_f","HMM post",
                               ifelse(name == "xy_f_ex_hmm","naive post","pre")))
accs_bdb$display_name = factor(accs_bdb$display_name, levels = c("pre","naive post","HMM post"))

accs_bdb |>
  ggplot(aes(x=reorder(display_name, value, FUN=median),y =value,fill = display_name)) +
  geom_boxplot()+
  scale_fill_manual(values = rev(col_vals))+
  facet_wrap(~mod) +
  labs(title = "Accuracys of models (50 different seeds)",
       x = "Model",
       y = "Accuracy",
       fill = "Model")


aucsl <- aucs |> pivot_longer(-mod)

aucsl |>
  ggplot(aes(x=name,y =value,fill = name)) +
  geom_boxplot()+
  scale_fill_manual(values = col_vals)+
  facet_wrap(~mod) +
  labs(title = "AUCs of models (50 different seeds)")


aucs_bdb |>
  ggplot(aes(x=reorder(display_name, value, FUN=median),y =value,fill = reorder(display_name, value, FUN=median))) +
  geom_boxplot()+
  scale_fill_manual(values = rev(col_vals))+
  facet_wrap(~mod) +
  labs(title = "AUCs of models (50 different seeds)",
       x = "Model",
       y = "AUC",
       fill = "Model")

# aucs_app |>
#   ggplot(aes(x=dp_fac,y =value,fill = dp_fac)) +
#   geom_boxplot()+
#   scale_fill_manual(values = col_vals)+
#   facet_wrap(~mod) +
#   labs(title = "AUCs of models (50 different seeds)",
#        x = "Model",
#        y = "AUC",
#        fill = "Model")


llsl <- lls |> pivot_longer(-mod)



lls_bdb <- llsl |>
  filter(name %in% c("xy_f","xy_f_ex_hmm","xy_npo_ex_hmm")) |>
  mutate(display_name = ifelse(name == "xy_f","HMM post",
                               ifelse(name == "xy_f_ex_hmm","naive post","pre")))

lls_bdb |>
  ggplot(aes(x=reorder(display_name, -value, FUN=median),y =-value,fill = reorder(display_name, -value, FUN=median))) +
  geom_boxplot()+
  scale_fill_manual(values = rev(col_vals))+
  facet_wrap(~mod) +
  labs(title = "Negative loglosses of models (50 different seeds)",
       x = "Model",
       y = "Negative logloss (log-likelihood)",
       fill = "Model")

