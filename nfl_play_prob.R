
#---  SETUP  --#

  # Load Packages
  library(tidyverse)
  library(tictoc)
  
  # Set Directory
  dat_fpath <- 'https://github.com/nflverse/nflfastR-data/blob/master/data/'

  # Load PBP Data
  tic()
  
    seasons <- c(1999:2020)
    
    pbp_dat <- map_df(seasons, function(x) {
      readRDS(
        url(
          paste(dat_fpath, 'play_by_play_', {x}, '.rds?raw=TRUE', sep="")
        )
      )
    })
    
  toc()

#---  ANALYSIS  ---#
  
  # Create Dataset for Analysis
  tic()
  
    play_prob_dat <- pbp_dat %>%
      dplyr::select(
        season, season_type, week, game_date, game_id, game_stadium, roof, surface, temp, wind,
        home_team, home_coach, away_team, away_coach, play_id, posteam, defteam, half_seconds_remaining, game_seconds_remaining, qtr,
        score_differential, yardline_100, ydstogo, goal_to_go, yards_gained, down, posteam_timeouts_remaining, defteam_timeouts_remaining, 
        shotgun, no_huddle, play_type, pass_length, pass_location, run_location, run_gap, field_goal_result,
        first_down_pass, first_down_rush, first_down_penalty, penalty
      ) %>%
      dplyr::filter(
        !is.na(down),
        !is.na(yardline_100),
        !is.na(play_type),
        play_type == "pass" | play_type == "run" | play_type == "punt" | play_type == "field_goal"
      ) %>%
      dplyr::mutate(
        third_long = if_else(down == 3 & ydstogo > 5, 1, 0),
        third_verylong = if_else(down == 3 & ydstogo > 10, 1, 0),
        in_red_zone = if_else(yardline_100 <= 20, 1, 0),
        in_fg_range = if_else(yardline_100 <= 35, 1, 0),
        two_min_drill = if_else(half_seconds_remaining <= 120, 1, 0),
        posteam_coach = if_else(posteam == home_team, home_coach, away_coach),
        defteam_coach = ifelse(defteam == home_team, home_coach, away_coach),
        game_type = ifelse(season_type == 'REG', 'Regular Season', 'Playoffs'),
        adj_qtr = ifelse(qtr > 4, 5, qtr),
        pass_type = paste(str_to_title(pass_length), str_to_title(pass_location), sep="-"),
        run_type = paste(str_to_title(run_location), str_to_title(run_gap), sep="-")
      ) %>%
      dplyr::relocate(
        posteam_coach, defteam_coach, .after = defteam
      ) %>%
      dplyr::arrange(
        season, week, game_date, game_id, play_id
      ) %>%
      dplyr::collect()
  
  toc()
  
  # Add Features
  tic()
  
    feat_play_prob_dat <- play_prob_dat %>%
      group_by(game_id, posteam) %>%
      mutate(
        run = if_else(play_type == "run", 1, 0),
        pass = if_else(play_type == "pass", 1, 0),
        punt = if_else(play_type == "punt", 1, 0),
        field_goal = if_else(play_type == "field_goal", 1, 0),
        total_runs = if_else(play_type == "run", cumsum(run) - 1, cumsum(run)),
        total_pass = if_else(play_type == "pass", cumsum(pass) - 1, cumsum(pass)),
        previous_play = if_else(posteam == lag(posteam),
                                lag(play_type), "first play of drive"
        ),
        previous_play = if_else(is.na(previous_play),
                                replace_na("first play of drive"), previous_play
        )
      ) %>%
      ungroup() %>%
      mutate_at(vars(
        play_type, season, posteam, defteam, shotgun, down, no_huddle,
        posteam_timeouts_remaining, defteam_timeouts_remaining, in_red_zone,
        in_fg_range, previous_play, goal_to_go, two_min_drill
      ), as.factor)
  
  toc()

  # Analyze Historical Rates
  tic()
  
    coach_summ_dat <- feat_play_prob_dat %>%
      group_by(head_coach = posteam_coach) %>%
      mutate(
        fd_plays = if_else(down == 1, 1, 0),
        fd_run = if_else(play_type == "run" & down == 1, 1, 0)
      ) %>%
      summarise(
        games_coached = length(unique(game_id)),
        plays_called = length(play_id),
        fd_plays_called = sum(fd_plays),
        run_plays = sum(run),
        pass_plays = sum(pass),
        fd_run_plays = sum(fd_run)
      ) %>%
      mutate(
        run_pct = run_plays/plays_called,
        pass_pct = pass_plays/plays_called,
        fd_run_pct = fd_run_plays/fd_plays_called
      )

  toc()
  
  # Test Results
  check_dataset <- subset(feat_play_prob_dat,
                          posteam_coach == 'Bill Belichick' & down == 1 & score_differential == 0 & ydstogo == 10 & yardline_100 == 20
                          )

#---  OUTPUT  ---#
    
write.csv(feat_play_prob_dat, "./nfl-head-coach-play-calling/feat_play_prob_dat.csv", row.names = FALSE)
 


