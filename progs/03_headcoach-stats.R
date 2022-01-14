
# SETUP -------------------------------------------------------------------

  # Packages
  library(tidyverse)
  library(tictoc)


# LOAD DATA ---------------------------------------------------------------

  pbp_dat_filt_feat_xpc <- readRDS("./data/output/pbp_dat_filt_feat_xpc.rds")


# HEAD COACH STATS BY GAME ------------------------------------------------

  hc_gm_stats <- pbp_dat_filt_feat_xpc %>%
    mutate(
      score_diff = case_when(
        score_differential >= (7*3) ~ 'Up 3-Scores or More',
        score_differential >= (7*2) ~ 'Up 2-Scores',
        score_differential >= (7*1) ~ 'Up 1-Score',
        score_differential == 0 ~ 'Tied',
        score_differential <= (7*1) ~ 'Down 1-Score',
        score_differential <= (7*2) ~ 'Down 2-Scores',
        score_differential <= (7*3) ~ 'Down 3-Scores or More',
      ),
      field_pos = case_when(
        yardline_100 <= 20 ~ 'Own 0-20 Yardline',
        yardline_100 <= 40 ~ 'Own 20-40 Yardline',
        yardline_100 <= 60 ~ 'Midfield Range',
        yardline_100 <= 80 ~ 'Opp 40-20 Yardline',
        yardline_100 <= 100 ~ 'Redzone',
      ),
      pass_length = ifelse(
        is.na(pass_length),
        pass_length,
        paste0(toupper(substring(pass_length,1,1)), substring(pass_length,2))
        ),
      pass_location = ifelse(
        is.na(pass_location),
        pass_location,
        paste0(toupper(substring(pass_location,1,1)), substring(pass_location,2))
      ),
      run_location = ifelse(
        is.na(run_location),
        run_location,
        paste0(toupper(substring(run_location,1,1)), substring(run_location,2))
      ),
      run_gap = ifelse(
        is.na(run_gap),
        run_gap,
        paste0(toupper(substring(run_gap,1,1)), substring(run_gap,2))
      )
    ) %>%
    select(
      posteam_coach, season, season_type, game_id, game_date, down, play_type,
      score_diff, field_pos,
      pass_length, pass_location, passes_first_down, pass_third_down_cvrt,
      pass_third_down_fail, pass_fourth_down_cvrt, pass_fourth_down_fail, pass_yards,
      pass_epa, pass_wpa,
      run_location, run_gap, runs_first_down, run_third_down_cvrt, run_third_down_fail,
      run_fourth_down_cvrt, run_fourth_down_fail, run_yards, run_epa, run_wpa,
      punt_epa, punt_wpa, fga_epa, fga_wpa,
      xpc
    ) %>%
    group_by(
      posteam_coach, season, season_type, game_id, game_date, down, score_diff, field_pos,
      pass_length, pass_location, run_location, run_gap
    ) %>%
    summarize(
      plays_called = length(game_id), #inclusive of the completed plays selected for this dataset (pass, rush, fga, punt), while ignoring things like penalties that cause repeat of down
      # Pass Stats
      pass = sum(play_type == 'pass'),
      xpass = sum(xpc == 'pass'),
      pass_first_downs = sum(passes_first_down),
      pass_third_downs_cvrt = sum(pass_third_down_cvrt),
      pass_third_downs_fail = sum(pass_third_down_fail),
      pass_third_downs_att = sum(pass_third_down_cvrt) + sum(pass_third_down_fail),
      pass_fourth_downs_cvrt = sum(pass_fourth_down_cvrt),
      pass_fourth_downs_fail = sum(pass_fourth_down_fail),
      pass_fourth_downs_att = sum(pass_fourth_down_cvrt) + sum(pass_fourth_down_fail),
      pass_yrds_gained = sum(pass_yards),
      pass_epa = sum(pass_epa),
      pass_wpa = sum(pass_wpa),
      # Run Stats
      runs = sum(play_type == 'run'),
      xruns = sum(xpc == 'run'),
      run_first_downs = sum(runs_first_down),
      run_third_downs_cvrt = sum(run_third_down_cvrt),
      run_third_downs_fail = sum(run_third_down_fail),
      run_third_downs_att = sum(run_third_down_cvrt) + sum(run_third_down_fail),
      run_fourth_downs_cvrt = sum(run_fourth_down_cvrt),
      run_fourth_downs_fail = sum(run_fourth_down_fail),
      run_fourth_downs_att = sum(run_fourth_down_cvrt) + sum(run_fourth_down_fail),
      run_yrds_gained = sum(run_yards),
      run_epa = sum(run_epa),
      run_wpa = sum(run_wpa),
      # Punt Stats
      punts = sum(play_type == 'punt'),
      xpunts = sum(xpc == 'punt'),
      punt_epa = sum(punt_epa),
      punt_wpa = sum(punt_wpa),
      # FGA Stats
      fga = sum(play_type == 'field_goal'),
      xfga = sum(xpc == 'field_goal'),
      fga_epa = sum(fga_epa),
      fga_wpa = sum(fga_wpa)
    ) %>%
    ungroup()
  

# SAVE OUTPUT -------------------------------------------------------------

  saveRDS(hc_gm_stats, file = "./data/output/hc_gm_stats.rds")
  

  