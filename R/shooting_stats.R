shooting_stats <- function(raw_list, game_ids) {
    stats_dfs <- lapply(raw_list, function(x) x$SHOTCHART$TEAM$PLAYER)
    # We need to create a variable identifying the team
    home_team_names <- lapply(raw_list, function(x) x$SHOTCHART$TEAM$name[1])
    home_team_ids <- lapply(raw_list, function(x) x$SHOTCHART$TEAM$id[1])
    away_team_names <- lapply(raw_list, function(x) x$SHOTCHART$TEAM$name[2])
    away_team_ids <- lapply(raw_list, function(x) x$SHOTCHART$TEAM$id[2])

    home_stats_dfs <- lapply(stats_dfs, function(x) x[[1]])
    away_stats_dfs <- lapply(stats_dfs, function(x) x[[2]])

    home_stats_dfs <- Map(cbind, home_stats_dfs,
                          "team_name" = home_team_names,
                          "team_id" = home_team_ids,
                          "game_id" = game_ids,
                          "home_team" = home_team_names,
                          "away_team" = away_team_names)
    away_stats_dfs <- Map(cbind, away_stats_dfs,
                          "team_name" = away_team_names,
                          "team_id" = away_team_ids,
                          "game_id" = game_ids,
                          "home_team" = home_team_names,
                          "away_team" = away_team_names)
    home_stats <- do.call("rbind", home_stats_dfs)
    away_stats <- do.call("rbind", away_stats_dfs)
    stats <- rbind(home_stats, away_stats) %>%
        dplyr::transmute(player_name = name,
                         player_no = no,
                         player_id = id,
                         fg_made = as.numeric(fgm),
                         fg_atp = as.numeric(fga),
                         fg_pct = fg_made / fg_atp,
                         p2_made = as.numeric(p2m),
                         p2_atp = as.numeric(p2a),
                         p2_pct = p2_made / p2_atp,
                         p3_made = as.numeric(p3m),
                         p3_atp = as.numeric(p3a),
                         p3_pct = p3_made / p3_atp,
                         points = as.numeric(pts),
                         team_name = as.character(team_name),
                         team_id = team_id,
                         game_id = game_id,
                         home_team = home_team,
                         away_team = away_team)
    stats$p2_pct[is.nan(stats$p2_pct)] <- 0
    stats$p3_pct[is.nan(stats$p3_pct)] <- 0
    stats$fg_pct[is.nan(stats$fg_pct)] <- 0

    stats
}

# str(stats)
# game_ids <- 2010208:2010216
# raw_list <- scrape_raw_list(game_ids)
# stats <- shooting_stats(raw_list, game_ids)

# shots <- extract_shots(game_ids)
# final_df <- dplyr::left_join(shots, stats, by = c("game_id", "player_no",
#                                                   "team_name"))

#str(final_df)
