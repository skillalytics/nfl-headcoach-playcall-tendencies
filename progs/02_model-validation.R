
# MODEL VALIDATION FUNCTION -----------------------------------------------

  PlayCallModValid <- function(pcpredict, traindat, modout){
    
    # ROC/AUC ---------------------------------------------------------------
    
    # Pass
    pass_predict <- data.frame(pcpredict)$pass
    pass_act <- traindat %>%
      mutate(
        pass_play = case_when(
          play_type == 'pass' ~ 1,
          TRUE ~ 0
        )
      ) %>%
      select(
        pass_play, pass, pcp
      )
    pass_act_result <- pass_act %>%
      mutate(
        result = case_when(
          pcp == 'pass' ~ 1,
          TRUE ~ 0
        ),
        match = case_when(
          pass_play == result ~ TRUE,
          TRUE ~ FALSE
        ),
      )
    sum(pass_act_result$match == TRUE)
    pass_predict_pct <- round(sum(pass_act_result$match == TRUE)/length(pass_act_result$match), digits = 4)
    pass_roc_plot <- roc(pass_act$pass_play, pass_predict)
    
    # Run
    run_predict <- data.frame(pcpredict)$run
    run_act <- traindat %>%
      mutate(
        run_play = case_when(
          play_type == 'run' ~ 1,
          TRUE ~ 0
        )
      ) %>%
      select(
        run_play, run, pcp
      )
    run_act_result <- run_act %>%
      mutate(
        result = case_when(
          pcp == 'run' ~ 1,
          TRUE ~ 0
        ),
        match = case_when(
          run_play == result ~ TRUE,
          TRUE ~ FALSE
        ),
      )
    sum(run_act_result$match == TRUE)
    run_predict_pct <- round(sum(run_act_result$match == TRUE)/length(run_act_result$match), digits = 4)
    run_roc_plot <- roc(run_act$run_play, run_predict)
    
    # Punt
    punt_predict <- data.frame(pcpredict)$punt
    punt_act <- traindat %>%
      mutate(
        punt_play = case_when(
          play_type == 'punt' ~ 1,
          TRUE ~ 0
        )
      ) %>%
      select(
        punt_play, punt, pcp
      )
    punt_act_result <- punt_act %>%
      mutate(
        result = case_when(
          pcp == 'punt' ~ 1,
          TRUE ~ 0
        ),
        match = case_when(
          punt_play == result ~ TRUE,
          TRUE ~ FALSE
        ),
      )
    sum(punt_act_result$match == TRUE)
    punt_predict_pct <- round(sum(punt_act_result$match == TRUE)/length(punt_act_result$match), digits = 4)
    punt_roc_plot <- roc(punt_act$punt_play, punt_predict)
    
    # FGA
    fga_predict <- data.frame(pcpredict)$field_goal
    fga_act <- traindat %>%
      mutate(
        fga_play = case_when(
          play_type == 'field_goal' ~ 1,
          TRUE ~ 0
        )
      ) %>%
      select(
        fga_play, field_goal, pcp
      )
    fga_act_result <- fga_act %>%
      mutate(
        result = case_when(
          pcp == 'field_goal' ~ 1,
          TRUE ~ 0
        ),
        match = case_when(
          fga_play == result ~ TRUE,
          TRUE ~ FALSE
        ),
      )
    sum(fga_act_result$match == TRUE)
    fga_predict_pct <- round(sum(fga_act_result$match == TRUE)/length(fga_act_result$match), digits = 4)
    fga_roc_plot <- roc(fga_act$fga_play, fga_predict)
    
    
    # CONFUSION MATRIX ------------------------------------------------------
    
    # Generate Confusion Matrix
    pcp_class <- predict(modout, traindat, type="class")
    pcp_confumat <- confusionMatrix(pcp_class, traindat$play_type2)
    

    # RETURN OUTPUT -----------------------------------------------------------
    
    # Create list of items to return
    modvalid_return <- list(
      "pass_roc_plot" = pass_roc_plot,
      "pass_predict_pct" = pass_predict_pct,
      "run_roc_plot" = run_roc_plot,
      "run_predict_pct" = run_predict_pct,
      "punt_roc_plot" = punt_roc_plot,
      "punt_predict_pct" = pass_predict_pct,
      "fga_roc_plot" = fga_roc_plot,
      "fga_predict_pct" = pass_predict_pct,
      "pcp_confumat" = pcp_confumat
    )
    # Return items
    return(modvalid_return)
    
  }

  


