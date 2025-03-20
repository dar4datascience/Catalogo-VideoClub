# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble",
               "httr2",
               "stringr",
               "stringdist",
               "uuid",
               "stringr",
               "tidyr",
               "purrr",
               "dplyr",
               "furrr"), # Packages that your targets need for their tasks.
  cue = tar_cue("thorough")
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.
api_key <- Sys.getenv("OMDB_API_KEY")

# Replace the target list below with your own:
list(
  tar_target(
    name = videoclub_catalogo,
    command = read_catalogo()
  ),
  tar_target(
    name = movies_search_results,
    command = search_movies(videoclub_catalogo, api_key)
  ),
  tar_target(
    name = tidy_movie_search,
    command = tidy_up_search_results(videoclub_catalogo, movies_search_results)
  ),
  tar_target(
    name = one_catalogue,
    command = build_one_catalogue(tidy_movie_search)
  ),
  tar_target(
    name = directors_catalogue,
    command = furry_fetch_director_info(one_catalogue)
  ),
  tar_target(
    name = video_club_clean_catalogue,
    command = augment_and_clean_one_catalogue(tidy_movie_search, one_catalogue, directors_catalogue)
  ),
  tar_target(
    name = movies_with_no_hits,
    command = account_for_movies_not_found(videoclub_catalogo, video_club_clean_catalogue)
  ),
  # make quarto report to visualize found movies 
  # and not found movies, 
  # both downloadable
  # try to leverage WASM to allow user to link missing movies and download results
  tarchetypes::tar_quarto(
    video_club_catalogue_report
  )
)
