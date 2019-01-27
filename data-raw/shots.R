game_id <- 2010208:2010216
shots <- extract_shots(game_ids)
usethis::use_data(shots, shots, overwrite = TRUE)
