# Top-level function -----------------------------------------------------------
search_potential_matches <- function(df, n) {
  df %>%
    distinct(artist_id) %>%
    pull() %>%
    map_dfr(~search_artist_potential_matches(.x, .n = n)) %>%
    left_join(distinct(df, artist, artist_id)) %>%
    mutate(
      title = NA,
      album.release_date = ymd(album.release_date, truncated = 2)
      ) %>%
    select(
      artist,
      title,
      track_id = id,
      duration_ms,
      popularity,
      released = album.release_date,
      track_name = name,
      album_name = album.name,
      artist_id,
      album_id = album.id,
      n_tracks = album.total_tracks
    ) %>%
    anti_join(distinct(df, track_id)) %>%
    remove_duplicates(.from = df, .min_char = 4) %>%
    filter(!is.na(track_id))
}

# Helper functions -------------------------------------------------------------
## Return n tracks from n albums for artist or NA row if no results ------------
search_artist_potential_matches <- function(.artist_id, .n) {
  albums <- .artist_id %>% 
    get_artist_albums(include_group = c("album", "single"), limit = .n)
  
  if (length(albums) == 0) {
    tracks <- tibble(
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
      external_urls.spotify = NA, 
      artist_id = .artist_id
    )
  } else {
    tracks <- albums %>% 
      pull(id) %>% 
      map_dfr(~get_album_tracks(.x, limit = .n)) %>% 
      pull(id) %>% 
      map_dfr(~get_tracks(.x)) %>% 
      mutate(artist_id = .artist_id) 
  }
  
  print(paste("Potential matches searched for artist with ID", .artist_id))
  
  tracks
}

## Remove tracks with titles too similar to chills track title -----------------
remove_duplicates <- function(.df, .from, .min_char) {
  words_to_check <- .df %>% 
    mutate(title = track_name) %>% 
    extract_words(.min_char = .min_char) %>% 
    mutate(title = NA) %>% 
    distinct()
  
  words_to_remove <- .from %>% 
    select(artist_id, title) %>% 
    extract_words(.min_char = .min_char) %>% 
    select(-title) %>% 
    distinct()
  
  tracks_to_remove <- words_to_check %>% 
    inner_join(words_to_remove, by = c("artist_id", "words")) %>%
    distinct(track_id)
  
  .df %>% 
    anti_join(tracks_to_remove)
}

### Extract numbers and words of at least `min_char` length from track titles
extract_words <- function(.df, .min_char) {
  .df %>% 
    mutate(words = str_extract_all(title, boundary("word"))) %>% 
    unnest(cols = words) %>%
    filter(nchar(words) >= .min_char | str_detect(words, "\\d")) %>% 
    mutate(words = tolower(words)) %>% 
    filter(!words %in% c("major", "minor"))
}
