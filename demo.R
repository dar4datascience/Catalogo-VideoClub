# access mdb file in linux is very complicated but found a tool for exporting it
library(dplyr)
library(purrr)

catalogo_peliculas <- readr::read_csv("peliculas_ant.csv") |> 
  janitor::clean_names()


# get clean title and director para desambiguar
zoomed_catalogo_peliculas <- catalogo_peliculas |> 
  select(
    titulo,
    tituloi,
    nom_dir,
    ape_dir
  ) |> 
  mutate(
    nombre_director = paste(nom_dir, ape_dir)
  )

# demo search

movie_name <- "RAIN MAN"

# transform spaces in movie name to '+'
movie_name <- gsub(" ", "+", movie_name)

api_key <- Sys.getenv("OMDB_API_KEY")


# Search for movie --------------------------------------------------------


movie_url <- paste0("http://www.omdbapi.com/?s=", movie_name, "&apikey=", api_key)

#print(movie_url)
library(httr2)

response <- request(movie_url) |>
  req_perform() |>
  resp_body_json() 


clean_response <- response |>
  pluck("Search") |>
  map(
    ~ tibble(
      title = stringr::str_to_upper(.x$Title),
      year = .x$Year,
      poster = .x$Poster,
      imdb_id = .x$imdbID
    )
  ) |>
  list_rbind() |>
  #remove movies with no poster
  filter(poster != 'N/A')

# rain_man_search <-


# use text similarity to determine most lilkely title ---------------------
matched_title <- clean_response |> 
  filter(title == "RAIN MAN")

# nice, final step is adding the imdb_id to the record

imdb_id <- matched_title |> 
  pull(imdb_id)

# use i end point to fetch all the information for a movie ----------------
imdb_url <- paste0("http://www.omdbapi.com/?i=", imdb_id, "&apikey=", api_key)

imdb_response <- request(imdb_url) |>
  req_perform() |>
  resp_body_json() 


imdb_df <- imdb_response |> 
  as.data.frame()


# disambiguate with director. could get expensive ----------------------------------------------


