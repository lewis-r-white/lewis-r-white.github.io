#Obtaining access token

library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = '1cc09ac76eb44d609b02f846813431da')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4e85ad97db564c49be2294bf6d667c25')

access_token <- get_spotify_access_token() #takes ID and SECRET, sends to Spotify and receives an access token