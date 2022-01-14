
# SETUP -------------------------------------------------------------------
  
  # Packages
  library(tidyverse)
  library(caTools) #sample.split
  library(nnet) #multinorm
  library(pROC) #roc
  library(caret) #confusionMatrix


# LOAD DATA ---------------------------------------------------------------

  pbp_dat_filt_feat <- readRDS("./data/output/pbp_dat_filt_feat.rds")


# PREP DATA ---------------------------------------------------------------

  # Select Variables for model
  # temp was removed from model data as it was deemed insignificant when first being run through the multinom regression
  xpc_modeldat <- pbp_dat_filt_feat %>%
    select(
      #Stadium & Weather
      roof, surface, wind, # temp,
      #Game Status
      qtr, half_seconds_remaining, game_seconds_remaining, score_differential, yardline_100,
      ydstogo, goal_to_go, posteam_timeouts_remaining, defteam_timeouts_remaining,
      #Play Info
      down, yards_gained, shotgun, no_huddle, previous_play, play_type,
      #Pass
      total_passes,
      total_pass_first_down, total_pass_third_down_cvrt, total_pass_third_down_fail,
      total_pass_yards, total_complete_passes, total_incomplete_passes, total_interceptions,
      #Run
      total_runs, total_runs_first_down, total_runs_third_down_cvrt, total_runs_third_down_fail,
      total_run_yards,
      #Punt
      #
      #FGA
      total_fga_made, total_fga_missed, total_fga_blocked
    )
  
  # Remove NAs from Dataset
  NA_Count <- data.frame(colSums(is.na(xpc_modeldat)))
  
  # colSums.is.na.xpc_modeldat..
  # roof                                                  0
  # surface                                               0
  # temp                                             203640
  # wind                                             203640
  # half_seconds_remaining                                0
  # game_seconds_remaining                                0
  # qtr                                                   0
  # score_differential                                    0
  # yardline_100                                          0
  # ydstogo                                               0
  # goal_to_go                                            0
  # yards_gained                                          0
  # down                                                  0
  # posteam_timeouts_remaining                            0
  # defteam_timeouts_remaining                            0
  # shotgun                                               0
  # no_huddle                                             0
  # play_type                                             0
  
  # Test for high number of NAs in temp and wind
  na_test <- xpc_modeldat %>% subset(is.na(temp) | is.na(wind))
  unique(na_test$roof)
  table(na_test$roof)
  # The majority of the cases appear to come from dome stadiums or retractable roofs that are closed at game time.
  # The wind for these plays can be set to 0 and the temperature will be estimated at 70 degrees (roughly "room temperature")
  # The open and outdoor games with NA temp and/or wind will need to be removed from the model data
  
  # Code chunk to adjust with temp included
    # xpc_modeldat <- xpc_modeldat %>%
    #   mutate(
    #     adj_temp = case_when(
    #       roof == 'closed' & is.na(temp) ~ 70,
    #       roof == 'dome' & is.na(temp) ~ 70,
    #       TRUE ~ as.numeric(temp)
    #     ),
    #     adj_wind = case_when(
    #       roof == 'closed' & is.na(wind) ~ 0,
    #       roof == 'dome' & is.na(wind) ~ 0,
    #       TRUE ~ as.numeric(wind)
    #     ) 
    #   ) %>%
    #   drop_na(
    #     adj_temp, adj_wind
    #   ) %>%
    #   select(
    #     -temp, -wind
    #   ) %>%
    #   relocate(
    #     c(adj_temp, adj_wind),
    #     .after = surface
    #   )
  # Code to adjust for just wind (as temp was removed due to being an insignificant var)
  xpc_modeldat <- xpc_modeldat %>%
    mutate(
      adj_wind = case_when(
        roof == 'closed' & is.na(wind) ~ 0,
        roof == 'dome' & is.na(wind) ~ 0,
        TRUE ~ as.numeric(wind)
      ) 
    ) %>%
    drop_na(
      adj_wind
    ) %>%
    select(
      -wind
    ) %>%
    relocate(
      c(adj_wind),
      .after = surface
    )
  

# RUN MODEL ---------------------------------------------------------------

  # Separate Between Test & Train
  set.seed(101) 
  sample_set <- sample.split(xpc_modeldat$play_type, SplitRatio = 0.8)
  train_set <- subset(xpc_modeldat, sample_set == TRUE)
  test_set <- subset(xpc_modeldat, sample_set == FALSE)
  
  # Get list of independent variables
  dataset_names <- xpc_modeldat %>%
    select(
      -play_type
    ) %>%
    colnames()

  # Specify baseline play call
  train_set$play_type2 <- relevel(factor(train_set$play_type), ref = "pass")
  
  # Generate model formula
  model_formula <- as.formula(
    paste0("play_type2 ~", paste(dataset_names, sep = "", collapse = " + "))
  )
  
  # Run model
  xpc_model <- multinom(model_formula, data = train_set, maxit = 1000)
  
  summary(xpc_model)
  
  # Get z-scores/p-values of variables
  z_train <- summary(xpc_model)$coefficients/summary(xpc_model)$standard.errors
  z_train
  p_train <- (1 - pnorm(abs(z_train), 0, 1)) * 2
  p_train
  
  # Most variables show signs of significance (temp having already been removed), with the caveat that some do not for one specific outcome
  # but are quite important for remaining outcomes.


# TEST & VALIDATE MODEL ---------------------------------------------------

  # Generate list of fitted probabilities for each play based on model output
  pcp <- round(fitted(xpc_model), digits = 8)
  
  # Combine with model data
  train_set <- cbind(train_set, pcp)
  
  # Clean output and add xpc vs apc match flag
  # First tried by setting 0.5 threshold
    # train_set <- train_set %>%
    #   mutate(
    #     pass = round(pass, digits = 2),
    #     field_goal = round(field_goal, digits = 2),
    #     punt = round(punt, digits = 2),
    #     run = round(run, digits = 2),
    #     pcp = case_when(
    #       pass >= 0.5 ~ 'pass',
    #       run >= 0.5 ~ 'run',
    #       field_goal >= 0.5 ~ 'field_goal',
    #       punt >= 0.5 ~ 'punt'
    #     ),
    #     pcp_match = case_when(
    #       play_type == pcp ~ 1,
    #       TRUE ~ 0
    #     )
    #   )
  # Adjusted to be based on play call showing max probability
  train_set <- train_set %>%
    mutate(
      pcp = case_when(
        pass > run & pass > punt & pass > field_goal ~ 'pass',
        run > pass & run > punt & run > field_goal ~ 'run',
        field_goal > pass & field_goal > run & field_goal > punt ~ 'field_goal',
        punt > pass & punt > run & punt > field_goal ~ 'punt'
      ),
      pcp_match = case_when(
        play_type == pcp ~ 1,
        TRUE ~ 0
      )
    )
  
  # Calculate predictive accuracy
  sum(train_set$pcp_match)/count(train_set)
  
  # Repeat for test data to ensure they are similar
  pcp_test <- round(
    predict(xpc_model, newdata = test_set, type = "probs"),
    digits = 8
  )
  test_set <- cbind(test_set, pcp_test)
  test_set <- test_set %>%
    mutate(
      pcp = case_when(
        pass > run & pass > punt & pass > field_goal ~ 'pass',
        run > pass & run > punt & run > field_goal ~ 'run',
        field_goal > pass & field_goal > run & field_goal > punt ~ 'field_goal',
        punt > pass & punt > run & punt > field_goal ~ 'punt'
      ),
      pcp_match = case_when(
        play_type == pcp ~ 1,
        TRUE ~ 0
      )
    )
  sum(test_set$pcp_match)/count(test_set)
  
  # Run Model Validation Techniques to Test Validity
  model_valid <- PlayCallModValid(pcpredict = pcp, traindat = train_set, modout = xpc_model)
  
  # ROC Plots
  plot(model_valid$pass_roc_plot,
       print.auc = TRUE,
       main = paste0("Pass ROC", "\nPredict Acc: ", model_valid$pass_predict_pct)
  )
  plot(model_valid$run_roc_plot,
       print.auc = TRUE,
       main = paste0("Run ROC", "\nPredict Acc: ", model_valid$run_predict_pct)
  )
  plot(model_valid$punt_roc_plot,
       print.auc = TRUE,
       main = paste0("Punt ROC", "\nPredict Acc: ", model_valid$punt_predict_pct)
  )
  plot(model_valid$fga_roc_plot,
       print.auc = TRUE,
       main = paste0("FGA ROC", "\nPredict Acc: ", model_valid$fga_predict_pct)
  )
  
  # Confustion Matrix
  model_valid$pcp_confumat
  
  # As shown through the validation techniques used the model is performing relatively well
  # The main issue that is shown both within the individual ROC plots and the confusion matrix is
  # that the model performs very well at predicting punts and field goals but has an issue identifying
  # pass vs run plays. This shows within the lower prediction accuracy, lower AUC, and the confusion
  # matrix confirms the problem is between run and pass based on the false negatives/positives between
  # the two plays.
  

# ADD XPC TO PBP DATA -----------------------------------------------------
  
  # Add Adjusted Wind to Data
  pbp_dat_filt_feat <- pbp_dat_filt_feat %>%
    mutate(
      adj_wind = case_when(
        roof == 'closed' & is.na(wind) ~ 0,
        roof == 'dome' & is.na(wind) ~ 0,
        TRUE ~ as.numeric(wind)
      ) 
    ) %>%
    drop_na(
      adj_wind
    )
  
  # Generate Predictions for Full Dataset
  xpc_pbp <- round(
    predict(xpc_model, newdata = pbp_dat_filt_feat, type = "probs"),
    digits = 8
  )
  
  # Add XPC to 
  pbp_dat_filt_feat_xpc <- cbind(pbp_dat_filt_feat, xpc_pbp)
  pbp_dat_filt_feat_xpc <-  pbp_dat_filt_feat_xpc %>%
    mutate(
      xpc = case_when(
        pass > run & pass > punt & pass > field_goal ~ 'pass',
        run > pass & run > punt & run > field_goal ~ 'run',
        field_goal > pass & field_goal > run & field_goal > punt ~ 'field_goal',
        punt > pass & punt > run & punt > field_goal ~ 'punt'
      ),
      xpc_match = case_when(
        play_type == xpc ~ 1,
        TRUE ~ 0
      )
    )
  
  # Calculate Overall Prediction Accuracy
  sum(pbp_dat_filt_feat_xpc$xpc_match)/count(pbp_dat_filt_feat_xpc)
  
    
# SAVE OUTPUT -------------------------------------------------------------
  
  # Model Files
  saveRDS(xpc_modeldat, file = "./model/xpc_modeldat.rds")
  saveRDS(train_set, file = "./model/train_set.rds")
  saveRDS(test_set, file = "./model/test_set.rds")
  saveRDS(xpc_model, file = "./model/xpc_model.rds")
  
  # PBP Data w/ XPC Added
  saveRDS(pbp_dat_filt_feat_xpc, file = "./data/output/pbp_dat_filt_feat_xpc.rds")
  
  
  