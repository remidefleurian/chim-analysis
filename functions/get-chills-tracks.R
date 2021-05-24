# Top-level function -----------------------------------------------------------
get_chills_tracks <- function(df) {
  df %>%
    replace_artists(.by = "Arrangement by") %>%
    replace_artists(.by = "Interpreted by") %>%
    get_unique_songs() %>%
    search_top_tracks() %>%
    get_tracks_features() %>%
    clean_tracks() %>%
    get_artist_ids() %>%
    assign_ids(.type = "chills")
}

# Helper functions -------------------------------------------------------------
## Replace `artist` column with `notes` column ---------------------------------
replace_artists <- function(.df, .by) {
  .df %>%
    mutate(artist = ifelse(
      !is.na(notes) & str_starts(notes, .by),
      str_replace(notes, paste0(.by, " "), ""),
      artist
    ))
}

## Remove duplicate songs ------------------------------------------------------
get_unique_songs <- function(.df) {
  df_no_dup <- .df %>%
    select(artist, title) %>%
    distinct()
  
  if (nrow(.df) - nrow(df_no_dup) > 0) {
    warning(paste(
      "Removed",
      nrow(.df) - nrow(df_no_dup),
      "rows containing duplicate songs."
    ))
  }
  
  df_no_dup
}

## Search top Spotify result for each track ------------------------------------
search_top_tracks <- function(.df) {
  .df %>% 
    mutate(item = ifelse(!is.na(artist), paste(artist, title), title)) %>% 
    pull(item) %>% 
    map_dfr(~search_top_track(.x)) %>%
    bind_cols(.df) %>% 
    select(
      artist, 
      title, 
      track_id = id, 
      duration_ms, 
      popularity,
      released = album.release_date, 
      track_name = name,
      album_name = album.name, 
      artist_id = artists, 
      album_id = album.id, 
      n_tracks = album.total_tracks
    )
}

### Return top Spotify result for single track or NA row if no results found
search_top_track <- function(.item) {
  row <- search_spotify(.item, type = "track", limit = 1)
  
  if (nrow(row) == 0) {
    row <- tibble(
      artists = NA,
      available_markets = NA,
      disc_number = NA,
      duration_ms = NA,
      explicit = NA,
      href = NA,
      id = NA,
      is_local = NA,
      name = NA,
      popularity = NA,
      preview_url = NA,
      track_number = NA,
      type = NA,
      uri = NA,
      album.album_type = NA,
      album.artists = NA,
      album.available_markets = NA,
      album.href = NA,
      album.id = NA,
      album.images = NA,
      album.name = NA,
      album.release_date = NA,
      album.release_date_precision = NA,
      album.total_tracks = NA,
      album.type = NA,
      album.uri = NA,
      album.external_urls.spotify = NA,
      external_ids.isrc = NA,
      external_urls.spotify = NA
    )
  }
  
  if (nrow(row) == 1) {
    row <- row %>%
      mutate(album.release_date = ymd(album.release_date, truncated = 2)) %>%
      mutate(artists = artists[[1]]['id'][[1]][1])
  }
  
  row
}

## Get Spotify features for each track -----------------------------------------
get_tracks_features <- function(.df) {
  .df %>%
    select(track_id) %>%
    pull() %>%
    map_dfr(~get_track_features(.x)) %>%
    select(-duration_ms) %>%
    bind_cols(.df) %>%
    select(
      artist:popularity,
      key,
      mode,
      tempo,
      time_signature,
      loudness,
      valence,
      danceability,
      energy,
      acousticness,
      instrumentalness,
      speechiness,
      liveness,
      released,
      track_name,
      album_name,
      artist_id,
      album_id,
      n_tracks:last_col()
    )
}

### Return Spotify features for single track or NA row if no results found
get_track_features <- function(.track_id) {
  if (!is.na(.track_id)) {
    row <- get_track_audio_features(.track_id)
  }
  
  if (is.na(.track_id) || ncol(row) == 1) {
    row <- tibble(
      danceability = NA,
      energy = NA,
      key = NA,
      loudness = NA,
      mode = NA,
      speechiness = NA,
      acousticness = NA,
      instrumentalness = NA,
      liveness = NA,
      valence = NA,
      tempo = NA,
      type = NA,
      id = NA,
      uri = NA,
      track_href = NA,
      analysis_url = NA,
      duration_ms = NA,
      time_signature = NA
    )
  }
  
  row %>%
    select(-type)
}

## Remove NA rows --------------------------------------------------------------
clean_tracks <- function(.df) {
  df_no_na <- .df %>%
    filter(!is.na(track_id)) %>%
    filter(!is.na(key))
  
  if (nrow(.df) - nrow(df_no_na) > 0) {
    warning(paste(
      "Removed",
      nrow(.df) - nrow(df_no_na),
      "rows containing missing values."
    ))
  }
  
  df_no_na
}

## Get unique `artist_id` for non-NA artists -----------------------------------
get_artist_ids <- function(.df) {
  artists <- .df %>%
    filter(!is.na(artist)) %>%
    select(artist) %>%
    distinct()
  
  artist_ids <- artists %>%
    pull(artist) %>%
    map_dfr(~search_top_artist(.x)) %>%
    select(id) %>%
    bind_cols(artists)
  
  .df %>%
    left_join(artist_ids) %>%
    mutate(artist_id = ifelse(is.na(artist) | is.na(id), artist_id, id)) %>%
    select(-id)
}

### Return top Spotify result for artist or NA row if no results found
search_top_artist <- function(.item) {
  row <- search_spotify(.item, type = "artist", limit = 1)
  
  if (nrow(row) == 0) {
    row <- tibble(
      genres = NA,
      href = NA,
      id = NA,
      images = NA,
      name = NA,
      popularity = NA,
      type = NA,
      uri = NA,
      external_urls.spotify = NA,
      followers.href = NA,
      followers.total = NA
    )
  }
  
  row
}

## Assign IDs for matching -----------------------------------------------------
assign_ids <- function(.df, .type) {
  if(.type == "chills") {
    .df <- .df %>% 
      mutate(analysis_id = "all") %>% 
      mutate(match_id = paste0("m", row_number()))
  }
  
  .df %>% 
    mutate(type = .type) %>% 
    select(artist:track_id, analysis_id, match_id, type, duration_ms:last_col())
}
