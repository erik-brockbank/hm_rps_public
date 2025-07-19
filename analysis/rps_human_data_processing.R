
#'
#' This script converts raw human performance data in csv format to .RData
#' NB: this only needs to be run once
#'


# INIT ----
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(tidyverse)


# GLOBALS ----
DATA_PATH = '../data/human_data' # path to directory containing raw data files (csv)
DATA_FILE = 'rps_human_trial_data.csv' # name of file containing full trial dataset for all rounds
FREE_RESP_FILE = 'rps_human_free_resp.csv' # name of file containing free response strategy data for all participants
SLIDER_FILE = 'rps_human_slider.csv' # name of file containing slider questionnaire data for all participants
OUTPUT_PATH = 'data_processed' # path to directory where processed data will be saved

GAME_ROUNDS = 300
STRATEGY_LEVELS = c('prev_move_positive', 'prev_move_negative',
                    'opponent_prev_move_positive', 'opponent_prev_move_nil',
                    'win_nil_lose_positive', 'win_positive_lose_negative',
                    'outcome_transition_dual_dependency')




# READ DATA ----
# Note: the raw data csv has more unique participant data than we keep in this function.
# Some participants did not complete the full set of rounds.
# Several managed to complete 300 rounds twice with the same survey code.
# In this instance, we keep the earlier of the two to ensure parity with other participants.
# One participant emailed saying she had completed the full set of rounds, but we don't have complete data.
# All told, 218 students were given credit; the below produces 217 complete participant data sets.
read_bot_data = function(filename, strategy_levels, game_rounds) {
  # Read csv
  data = read_csv(filename)
  data$bot_strategy = factor(data$bot_strategy, levels = strategy_levels)
  # Remove all incomplete games
  incomplete_games = data  |>
    group_by(game_id, player_id) |>
    summarize(rounds = max(round_index)) |>
    filter(rounds < game_rounds) |>
    select(game_id) |>
    unique()
  data = data |>
    filter(!(game_id %in% incomplete_games$game_id))
  # Remove any duplicate complete games that have the same completion code
  # NB: this can happen if somebody played all the way through but exited before receiving credit
  # First, fetch survey codes with multiple complete games
  repeat_codes = data |>
    group_by(sona_survey_code) |>
    filter(is_bot == 0) |>
    summarize(trials = n()) |>
    filter(trials > game_rounds) |>
    select(sona_survey_code)
  # Next, get game id for the later complete game and remove it (below)
  duplicate_games = data |>
    filter(sona_survey_code %in% repeat_codes$sona_survey_code &
             is_bot == 0  &
             round_index == game_rounds) |>
    select(sona_survey_code, game_id, player_id, round_begin_ts) |>
    group_by(sona_survey_code) |>
    filter(round_begin_ts == max(round_begin_ts)) |>
    distinct(game_id)
  data = data |>
    filter(!game_id %in% duplicate_games$game_id)

  return(data)
}


# Read data
human_trial_data = read_bot_data(
  paste(DATA_PATH, DATA_FILE, sep = '/'),
  STRATEGY_LEVELS,
  GAME_ROUNDS
)
human_free_resp_data = read_csv(paste(DATA_PATH, FREE_RESP_FILE, sep = '/'))
human_slider_data = read_csv(paste(DATA_PATH, SLIDER_FILE, sep = '/'))


# SUMMARIZE + FILTER ----
# Summarize trial data, perform additional filtering to remove participants where outcomes weren't logged for all 300 rounds
trial_data_summary = human_trial_data |>
  filter(is_bot == 0) |>
  group_by(
    game_id, player_id, bot_strategy
  ) |>
  summarize(
    total_points = sum(player_points),
    total_win = sum(player_outcome == 'win', na.rm = T),
    total_loss = sum(player_outcome == 'loss', na.rm = T),
    total_tie = sum(player_outcome == 'tie', na.rm = T),
    games = sum(total_win, total_loss, total_tie), # sanity check: how many NAs were removed in the above
    win_pct = total_win / games
  )

# Filter out any participants for whom we did not capture outcomes for all 300 rounds
trial_data_summary_clean = trial_data_summary |>
  filter(games == 300)
human_trial_data_clean = human_trial_data |>
  filter(player_id %in% unique(trial_data_summary_clean$player_id)) # NB: this will remove all bot rows in addition to incomplete human rows

# Summary: how many participants in each condition
trial_data_summary_clean |>
  group_by(bot_strategy) |>
  summarize(
    participants = n_distinct(player_id)
  )

# Filter free response and slider data to include only participants for whom we have summary win data above
human_free_resp_data_clean = human_free_resp_data |>
  inner_join(
    trial_data_summary_clean,
    by = c('game_id', 'player_id')
  )
glimpse(human_free_resp_data_clean)

human_slider_data_clean = human_slider_data |>
  inner_join(
    trial_data_summary_clean,
    by = c("game_id", "player_id")
  )
glimpse(human_slider_data_clean)



# SAVE ----
# Save as RData
save(human_trial_data_clean, file = paste(DATA_PATH, OUTPUT_PATH, 'rps_human_trial_data.RData', sep = '/'))
save(human_free_resp_data_clean, file = paste(DATA_PATH, OUTPUT_PATH, 'rps_human_free_resp_data.RData', sep = '/'))
save(human_slider_data_clean, file = paste(DATA_PATH, OUTPUT_PATH, 'rps_human_slider_data.RData', sep = '/'))

# Save as CSV
write_excel_csv(trial_data_summary_clean, file = paste(DATA_PATH, OUTPUT_PATH, 'rps_human_trial_data_summary.csv', sep = '/'))
write_excel_csv(human_free_resp_data_clean, file = paste(DATA_PATH, OUTPUT_PATH, 'rps_human_free_resp_data.csv', sep = '/'))
write_excel_csv(human_slider_data_clean, file = paste(DATA_PATH, OUTPUT_PATH, 'rps_human_slider_data.csv', sep = '/'))




