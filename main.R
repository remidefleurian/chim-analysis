# Setup ------------------------------------------------------------------------
source("scripts/load-libraries.R")
source("scripts/load-functions.R")
source("scripts/load-config.R")

# Raw data ---------------------------------------------------------------------
df_chim <- read_csv("data/chim-1.0.0.csv")

# Spotify data -----------------------------------------------------------------
if (refresh_data) {
  df_chil <- df_chim %>% get_chills_tracks()
  matches <- df_chil %>% search_potential_matches(n = 50)
  df_mtch <- df_chil %>% get_matched_tracks(from = matches, iter = 10)
  # Save data
  df_chil %>% write_csv("output/data/features-chills.csv")
  matches %>% write_csv("output/data/potential-matches.csv")
  df_mtch %>% write_csv("output/data/features-matches.csv")
} else {
  df_chil <- read_csv("output/data/features-chills.csv")
  matches <- read_csv("output/data/potential-matches.csv")
  df_mtch <- read_csv("output/data/features-matches.csv")
}

# Analysis ---------------------------------------------------------------------
if (refresh_data) {
  df_stat <- df_chil %>% analyse_data(with = df_mtch)
  df_stat %>% write_csv("output/analysis/results.csv")
} else {
  df_stat <- read_csv("output/analysis/results.csv")
}
