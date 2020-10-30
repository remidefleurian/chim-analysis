# Pull Spotify API data if TRUE, load archived data if FALSE
refresh_data <- FALSE

# Spotify access token
# See https://www.rcharlie.com/spotifyr/index.html#authentication
Sys.setenv(SPOTIFY_CLIENT_ID = "YOUR_ID_HERE")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "YOUR_SECRET_HERE")
access_token <- get_spotify_access_token()

# Set seed for reproducible mediation analyses
set.seed(123)