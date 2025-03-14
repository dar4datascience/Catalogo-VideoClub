library(httr2)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(furrr)


# Read Catalogo -----------------------------------------------------------
read_catalogo <- function(){
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
    ) |> 
    select(
      titulo,
      tituloi
      nombre_director
    )
  
  return(zoomed_catalogo_peliculas)
}

# Search Movies -----------------------------------------------------------
# Sub-function to search for movies based on name
search_movie <- function(movie_name, api_key) {
  
  movie_name <- gsub(" ", "+", movie_name)  # transform spaces to '+'
  
  # Build the search URL
  movie_url <- paste0("http://www.omdbapi.com/?s=", movie_name, "&apikey=", api_key)
  
  # Perform the request and extract relevant data
  response <- request(movie_url) |>
    req_perform() |>
    resp_body_json()
  
  # Clean and filter the response data
  clean_response <- response |>
    pluck("Search") |>
    map(
      ~ tibble(
        title = str_to_upper(.x$Title),
        year = .x$Year,
        poster = .x$Poster,
        imdb_id = .x$imdbID
      )
    ) |>
    list_rbind() |>
    filter(poster != 'N/A')
  
  nested_df <- clean_response |> 
    mutate(
      movie_name = movie_name
    ) |> 
    group_by(movie_name) |> 
    tidyr::nest(.key = "search_results")
  
  return(nested_df)
}

search_movies <- function(zoomed_catalogo, api_key){
  plan(multisession, workers = 6)
  
  searched_movies <- future_map(zoomed_catalogo,
                                ~ search_movie(.x, api_key)
  ) |> 
    list_rbind()
  

  return(searched_movies)  
}


# Search IMDB -------------------------------------------------------------

# Sub-function to fetch metadata for a movie using the IMDB ID
fetch_movie_metadata <- function(imdb_id, api_key) {
  imdb_url <- paste0("http://www.omdbapi.com/?i=", imdb_id, "&apikey=", api_key)
  
  # Perform the request to fetch metadata
  imdb_response <- request(imdb_url) |>
    req_perform() |>
    resp_body_json()
  
  # Convert to a data frame
  imdb_df <- as.data.frame(imdb_response)
  
  return(imdb_df)
}


# Combined Flow -----------------------------------------------------------



# Main function to search and fetch movie metadata
search_n_fetch_movie_metadata <- function(movie_name, api_key) {
  # Search for the movie and get clean response
  clean_response <- search_movie(movie_name, api_key)
  
  # Match the movie title
  matched_title <- clean_response |> 
    filter(title == str_to_upper(movie_name))
  
  if (nrow(matched_title) == 0) {
    stop("No matching movie found.")
  }
  
  # Get the imdb_id from the matched title
  imdb_id <- matched_title |> 
    pull(imdb_id)
  
  # Fetch movie metadata using the imdb_id
  movie_metadata <- fetch_movie_metadata(imdb_id, api_key)
  
  return(movie_metadata)
}

# Example usage:
# api_key <- Sys.getenv("OMDB_API_KEY")
# movie_metadata <- search_n_fetch_movie_metadata("RAIN MAN", api_key)

# catalogo_peliculas <- readr::read_csv("peliculas_ant.csv") |> 
#   janitor::clean_names()
# 
# 
# # get clean title and director para desambiguar
# zoomed_catalogo_peliculas <- catalogo_peliculas |> 
#   select(
#     titulo,
#     tituloi,
#     nom_dir,
#     ape_dir
#   ) |> 
#   mutate(
#     nombre_director = paste(nom_dir, ape_dir)
#   )
# 
# safely_search_n_fetch_movie_metadata <- purrr::safely(search_n_fetch_movie_metadata)
# 
# 
# api_key <- Sys.getenv("OMDB_API_KEY")
# 
# # furrr candidate
# movies_metadata_list <- zoomed_catalogo_peliculas$tituloi |> 
#   map(
#     ~ safely_search_n_fetch_movie_metadata(.x, api_key)
#   ) 



# save result!