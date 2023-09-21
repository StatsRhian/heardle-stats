library("glue")
library("httr")
library("jsonlite")
library("tidyverse")

artist <- "Red Hot Chili Peppers"
get_tags(artist)

artist <- "Sia"
get_top_tag(artist)

get_top_tag = function(artist){

artist <- URLencode(artist)
stub <- "http://ws.audioscrobbler.com/2.0/"

artist_tags <- glue("{stub}?method=artist.getTopTags&artist={artist}&user=RJ&api_key={Sys.getenv('LASTFM_API_KEY')}&format=json")

request <- GET(url = artist_tags)
response <- content(request, as = "text", encoding = "UTF-8")

tags_tibble <- fromJSON(response, flatten = TRUE)$toptags$tag |>
  tibble()

if(nrow(tags_tibble) == 0){
  tag = NA
} else {
tag = tags_tibble$name[[1]]
}
return(tag)
}
