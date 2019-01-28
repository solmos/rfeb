#' Scrape raw list object with all the shooting data from Baloncesto en Vivo
#'
#' @param game_ids Game IDs
#'
#' @return A list object
#' @export
#'
#' @examples
#' bad_ids <- c(2010215, 2010150, 2010216, 2010147)
#' good_ids <- 2010213:2010216
#' good_requests <- scrape_raw_list(good_ids)
#' bad_request <- scrape_raw_list(bad_ids)
scrape_raw_list <- function(game_ids) {
    base_api <- "http://baloncestoenvivo.feb.es/api/ShotChart/"
    game_apis <- paste0(base_api, game_ids)
    api_requests <- lapply(game_apis, httr::GET)

    # Dealing with API request errors
    errors <- unlist(lapply(api_requests, httr::http_error))
    good_game_ids <- game_ids[!errors]
    bad_game_ids <- game_ids[errors]
    error_message <- paste("request game ID", bad_game_ids)
    # Why not working?
    # mapply(httr::warn_for_status, api_requests, task = as.list(error_message))
    bad_requests <- api_requests[errors]
    n_errors <- length(bad_requests)
    for (i in 1:n_errors) {
        warn_for_status(bad_requests[[i]], task = error_message[i])
    }
    good_requests <- api_requests[!errors]

    json_list <- lapply(good_requests, httr::content,
                        as = "text", encoding = "UTF-8")
    raw_list <- lapply(json_list, jsonlite::fromJSON)

    attr(raw_list, "game_ids") <- good_game_ids

    raw_list
}
