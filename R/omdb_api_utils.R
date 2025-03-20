library(httr2)
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(furrr)


# Safely Search -----------------------------------------------------------

tidy_up_search_results <- function(videoclub_catalogo, movies_search_results){
  
  found_movies <- movies_search_results |> 
    map(
    ~pluck(.x, 'result')
    ) |> 
    list_rbind()
  
  # unnest and only keep those with exact match names
    agg_movie_catalogo <- videoclub_catalogo |> 
      left_join(found_movies,
                join_by(titulo_disponible == movie_name ,
                         uuid))
    
    return(agg_movie_catalogo)
}

account_for_movies_not_found <- function(videoclub_catalogo, tidy_movie_search){
  
  found_movies <- tidy_movie_search |> 
    distinct(titulo_disponible,
             uuid)
  
  movies_with_no_search_results <- videoclub_catalogo |> 
    anti_join(found_movies,
              join_by( titulo_disponible,
                       uuid)) 
    
  
  return(movies_with_no_search_results)
  
}

augment_and_clean_one_catalogue <- function(tidy_movie_search, one_catalogue, directors_catalogue) {
  aug_one_catalogue <- one_catalogue |> 
    left_join(directors_catalogue, by = "imdb_id") |> 
    mutate(
      nombre_director = if_else(nombre_director == "NA NA", director, nombre_director),
      director = stringr::str_to_upper(director),
      director_is_same = stringr::str_detect(director, nombre_director),
      director_similitude = stringdist::stringdist(nombre_director, director, method = "lv")  # Levenshtein distance
    ) |> 
    filter(
      director_is_same == TRUE | director_similitude < 5 
    ) |> 
    select(
      uuid,
      year,
      imdb_id,
      titulo_disponible,
      plot,
      poster
    )
  
  video_club_clean_catalogue <- tidy_movie_search |> 
    left_join(aug_one_catalogue, by = c("uuid", "titulo_disponible")) |> 
    select(-search_results) |> 
    filter(!is.na(imdb_id))
  
  return(video_club_clean_catalogue)
}


furry_fetch_director_info <- function(one_catalogue) {
  future::plan(future::multisession, workers = 6)
  
  directors_catalogue <- one_catalogue |> 
    distinct(imdb_id) |>
    pull() |> 
    furrr::future_map(
      ~ fetch_movie_metadata(.x, Sys.getenv("OMDB_API_KEY"))
    ) |> 
    dplyr::bind_rows() |> 
    distinct()
  
  return(directors_catalogue)
}

build_one_catalogue <- function(tidy_movie_search) {
  criterion_catalogue <- tidy_movie_search |> 
    tidyr::unnest(cols = search_results) |> 
    mutate(
      title_is_same = titulo_disponible == title
    )
  
  one_catalogue <- criterion_catalogue |> 
    mutate(
      movie_name_similitude = stringdist::stringdist(titulo_disponible, title, method = "lv") 
    ) |> 
    filter(
      title_is_same == TRUE 
      #| movie_name_similitude < 5 this can cause duplicates
    )
  
  return(one_catalogue)
}

disambiguate_movies_found <- function(tidy_movie_search){
  
  criterion_catalogue <- tidy_movie_search |> 
    tidyr::unnest(cols = search_results) |> 
    mutate(
      title_is_same = titulo_disponible == title
    )
  
  one_catalogue <- criterion_catalogue |> 
    mutate(
      movie_name_similitude = stringdist(titulo_disponible, title, method = "lv") 
    ) |> 
    filter( #stringr dif causes movie 2 
      title_is_same == TRUE | movie_name_similitude < 5
    ) 

  
  plan(multisession, workers = 6)
  

  
  directors_catalogue <- one_catalogue |> 
    distinct(imdb_id) |>
    pull() |> 
    future_map(
      ~ fetch_movie_metadata(.x, Sys.getenv("OMDB_API_KEY"))
      ) |> 
    list_rbind() |> 
    distinct()
  # NEXT IS TO DISAMBIGUAGTE BY FETCHING THE DIRECTOR INFORMATION FOR EACH MOVIE
  
  # update to use stringdist on title because some movies dont have director but their title is obvious
  
  aug_one_catalogue <- one_catalogue |> 
    left_join(directors_catalogue,
              join_by(imdb_id)) |> 
    mutate(
      nombre_director = if_else(nombre_director == "NA NA", director, nombre_director)
    ) |> 
    mutate(
      director = stringr::str_to_upper(director),
      director_is_same = str_detect(director, nombre_director),
      director_similitude = stringdist(nombre_director, director, method = "lv")  # Levenshtein distance
    ) |> 
    filter(
      director_is_same == TRUE | director_similitude < 5 
    ) |> 
    select(
      uuid,
      year,
      imdb_id,
      titulo_disponible,
      plot,
      poster
    )
  
  video_club_clean_catalogue <- tidy_movie_search |> 
    left_join(aug_one_catalogue,
              join_by(uuid,
                      titulo_disponible)) |> 
    select(!search_results) |> 
    filter(!is.na(imdb_id))
  
  return(video_club_clean_catalogue)
}

clean_up_movie_title <- function(title_movie){
  clean_movie_title <- title_movie |> 
    str_remove_all("\\( \\d{1} \\)") |>
    str_remove_all("\\( \\d{2} \\)") |> 
    str_remove_all("\\( \\d{3} \\)") |> 
    str_remove_all("\\( \\d{4} \\)") |> 
    #str_remove_all("'") |> 
    str_squish()
  
  return(clean_movie_title)
}

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
      nombre_director = paste(nom_dir, ape_dir),
      titulo_disponible = coalesce(tituloi, titulo, "Sin datos"), 
      titulo_disponible = map_chr(
        titulo_disponible,
        clean_up_movie_title
      ),
      uuid = uuid::UUIDgenerate(n = nrow(catalogo_peliculas))
    ) |> 
    select(
      titulo_disponible,
      nombre_director,
      uuid
    ) |> 
    filter( # quitar cuando no se tiene titulo disponible
      titulo_disponible != "Sin datos", # segun yo solo quita 1
      stringr::str_length(titulo_disponible) > 1
    ) |> 
    distinct()
  
  # check
  
  # nrow(zoomed_catalogo_peliculas)
  
  return(zoomed_catalogo_peliculas)
}

# Search Movies -----------------------------------------------------------
# Sub-function to search for movies based on name
search_movie <- function(movie_name, uuid, api_key) {
  
  #print(movie_data)
  
  Sys.sleep(1)
  
  # cli::cli_inform(
  #   glue::glue("Searching for {movie_name}")
  # )
  
  
  clean_movie_name <- gsub(" ", "+", movie_name)  # transform spaces to '+'
  
  # Build the search URL
  movie_url <- paste0("http://www.omdbapi.com/?s=", clean_movie_name, "&apikey=", api_key)
  
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
        #poster = .x$Poster,
        imdb_id = .x$imdbID
      )
    ) |>
    list_rbind() 
  
  nested_df <- clean_response |> 
    mutate(
      movie_name = movie_name,
      uuid = uuid
    ) |> 
    group_by(movie_name, uuid) |> 
    tidyr::nest(.key = "search_results")
  
  return(nested_df)
}

search_movies <- function(videoclub_catalogo, api_key){
  plan(multisession, workers = 6)
  
  safely_search_movie <- safely(search_movie)
  
  searched_movies <- videoclub_catalogo |> 
    select(titulo_disponible,
           uuid) |> 
    future_pmap(~ safely_search_movie(.x, .y, api_key)
  ) 
  

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
  imdb_df <- tibble::as_tibble(imdb_response) |> 
    janitor::clean_names() |> 
    select(
      director,
           actors,
           type,
      plot,
      poster
    ) |> 
    mutate(
      imdb_id = imdb_id,
      director = if_else(director == "N/A", "Not found", director),
      director = coalesce(director, "Not found") # might be redudntdat
    ) |> 
    distinct()
  
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


# Table -------------------------------------------------------------------



#' @export
tbl_movies_found <- function(movies_found) {
  movies_found |>
    dplyr::select(titulo_disponible, year, plot, poster) |>
    reactable(
      theme = fivethirtyeight(centered = TRUE),
      selection = "single",
      onClick = "select",
      striped = TRUE,
      pagination = FALSE,
      searchable = TRUE,
      defaultColDef = colDef(align = 'center'),
      columns = list(
        poster = colDef(
          name = 'Poster',
          maxWidth = 200,
          cell = embed_img(movies_found$poster,
                           height = 200,
                           width = 235,
                           horizontal_align = 'center')
        ),
        titulo_disponible = colDef(maxWidth = 150, name = 'Movie'),
        year = colDef(maxWidth = 50,
                      name = 'Released'),
        plot = colDef(minWidth = 150,
                      name = 'Plot')
      )
    )
}
