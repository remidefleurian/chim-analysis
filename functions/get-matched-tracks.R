# Top-level function -----------------------------------------------------------
get_matched_tracks <- function(df, from, iter) {
  df %>% 
    standardise_cols(.with = from) %>% 
    {select_matches(.df = .[1][[1]], .from = .[2][[1]], .iter = iter)} %>% 
    assign_ids(.type = "match") %>% 
    get_tracks_features() %>% 
    clean_tracks()
}

# Helper functions -------------------------------------------------------------
## Standardise features across chills and matches ------------------------------
standardise_cols <- function(.df, .with) {
  df_std <- .df %>% 
    select(artist_id, track_id, duration_ms, popularity) %>%
    mutate(type = "Chills")
  
  with_std <- .with %>% 
    select(artist_id, track_id, duration_ms, popularity) %>% 
    mutate(type = "Match (available)")
  
  all_std <- df_std %>% 
    bind_rows(with_std) %>% 
    mutate(
      duration_ms_std = scale(duration_ms),
      popularity_std = scale(popularity)
    ) %>% 
    select(-artist_id, -type)
    
  .df <- .df %>% 
    left_join(all_std)
  
  .with <- .with %>% 
    left_join(all_std)
  
  list(distinct(.df), distinct(.with))
}

## Get n matches for each chills track -----------------------------------------
select_matches <- function(.df, .from, .iter) {
  1:nrow(.df) %>% 
    map_dfr(~select_match(slice(.df, .x), .from, .iter))
}

### Return n matches with shortest Eucl. dist. or n NA rows if no matches found
select_match <- function(.row, .from, .iter) {
  rows <- .from %>% 
    filter(artist_id == pull(select(.row, artist_id)))
  
  if(nrow(rows) == 0) {
    rows <- tibble(
      artist = rep(pull(select(.row, artist)), .iter),
      title = rep(NA_character_, .iter),
      track_id = rep(NA_character_, .iter),
      analysis_id = rep(NA, .iter),
      duration_ms = rep(NA, .iter), 
      popularity = rep(NA, .iter),
      released = rep(as.Date(NA), .iter),
      track_name = rep(NA_character_, .iter), 
      album_name = rep(NA_character_, .iter),
      artist_id = rep(pull(select(.row, artist_id)), .iter), 
      album_id= rep(NA_character_, .iter),
      n_tracks = rep(NA, .iter), 
      eucl_dist = rep(NA, .iter)
    )
  } else {
    rows <- rows %>% 
      mutate(eucl_dist = sqrt(
        (duration_ms_std - as.numeric(select(.row, duration_ms_std)))^2 + 
          (popularity_std - as.numeric(select(.row, popularity_std)))^2)) %>% 
      arrange(eucl_dist) %>%
      mutate(analysis_id = paste0("a", row_number())) %>% 
      select(artist:track_id, analysis_id, duration_ms:n_tracks, eucl_dist) %>% 
      head(.iter) %>%
      bind_rows(tibble(
        artist = rep(pull(select(.row, artist)), .iter - nrow(.)),
        artist_id = rep(pull(select(.row, artist_id)), .iter - nrow(.))
      ))
  }
  
  rows
}
