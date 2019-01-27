#' Extract shot data for Spanish FEB basketball leagues
#'
#' @param game_ids Game IDs from Baloncesto en Vivo
#'
#' @importFrom magrittr %>%
#'
#' @return A data frame with x, y coordinates
#' @export
#'
#' @examples
#' ids <- 2010208:2010216
#' week17_2018 <- extract_shots(ids)
extract_shots <- function(game_ids) {

    # Scrape the data from Baloncesto en Vivo ====
    data_list <- scrape_raw_list(game_ids)

    # Wrangle the data ====
    # We only want the shot data.
    shots_list <- lapply(data_list, function(x) x$SHOTCHART$SHOTS)

    # Specify the team names as factor levels of team variable instead of 0s and 1s.
    team_names_levels <- lapply(data_list, function(x) x$SHOTCHART$TEAM$name)
    team_ids_levels <- lapply(data_list, function(x) x$SHOTCHART$TEAM$id)
    # We need to assign the two team factor levels to variable team of
    ## each element (i.e. game) of our list.
    assign_team_names <- function(x, y) {
        factor(as.numeric(x$team), labels = y)
    }
    team_names_list <- mapply(assign_team_names,
                              shots_list, team_names_levels)
    # Convert list into a single vector that we will append to our final data frame.
    team_names_character_list <- lapply(team_names_list, as.character)
    team_names_vector <- do.call("c", team_names_character_list)
    team_names_to_append <- as.factor(team_names_vector)

    # Create variable identifying the game before merging all elements/games of
    ## our list into one big data frame
    rows_per_game <- sapply(shots_list, nrow)
    ids_to_append <- rep(game_ids, rows_per_game)

    shots_raw <- do.call("rbind", shots_list)
    shots_raw$game_id <- ids_to_append
    shots_raw$team_name <- team_names_to_append

    # Raw coordinates are in a different scale than court drawn.
    ## Trial and error gave me 5.45 a good transformation.
    # The x coordinate needs to be flipped accross the vertical line
    ## going through the middle of the baseline (750).
    ## 2 * 750 - x gives us the desired transformation.
    shots <- shots_raw %>%
        dplyr::mutate(made = factor(as.numeric(m), levels = c(0, 1),
                                    labels = c("Missed", "Made")),
                      x = 2 * 750 - 5.45 * as.numeric(x),
                      y = 5.45 * as.numeric(y),
                      quarter = as.factor(as.numeric(quarter)),
                      game_id = game_id,
                      team_name = as.character(team_name),
                      player_no = player) %>%
        dplyr::select(-m, -player, -team)

    # We need to extract the stats from the raw data list
    stats <- shooting_stats(data_list, game_ids)
    final_shots <- dplyr::left_join(shots, stats,
                                    by = c("game_id",
                                           "player_no",
                                           "team_name"))
    final_shots
}

