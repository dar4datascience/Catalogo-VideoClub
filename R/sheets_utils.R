write_catalog_results_2_sheets <- function(peliculas_encontradas, peliculas_no_identificadas){

  
  drive_auth(path=gargle::secret_decrypt_json("encrypted-video-club-amores.json", "GARGLE_KEY"))
  
  gs4_auth(path=gargle::secret_decrypt_json("encrypted-video-club-amores.json", "GARGLE_KEY"))
  
  # CLEAN UP PREVIOUS
  gs4_find("catalogo-videoclub-amores-v1") |> 
    drive_trash()
  
  # create a Sheet with some initial, placeholder data
  ss <- gs4_create(
    "catalogo-videoclub-amores-v1",
    sheets = list(identificados = data.frame(x = 1), no_identificados = data.frame(x = 1))
  )
  #> ✔ Creating new Sheet: sheet-write-demo.

  sheet_write(peliculas_encontradas, ss = ss, sheet = "identificados")
  
  sheet_write(peliculas_no_identificadas, ss = ss, sheet = "no_identificados")
  
  gs4_find("catalogo-videoclub-amores-v1") |> 
    drive_share(
      role = "reader",
      type = "user",
      emailAddress = Sys.getenv("EMAIL1"),
      emailMessage = "Would appreciate your feedback on this!"
    )
}