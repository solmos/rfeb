#' Plot shot charts
#'
#' @param df data frame with at least two coordinate columns named x and y
#' @param ... arguments specified to geom_point(aes(x, y, ...))
#'
#' @import ggplot2
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ids <- 2010208:2010216
#' week17_2018 <- extract_shots(ids)
#' plot_shotchart(week17_2018)
#' plot_shotchart(week17_2018, color = made) + facet_wrap(~game_id)
plot_shotchart <- function(df, ...) {

    court <- construct_court()

    plt <- ggplot(environment = environment()) +
        geom_point(data = df, aes(x, y, ...), alpha = 0.8) +
        geom_path(aes(x, y), size = 0.2, data = court$outer_lines) +
        geom_path(aes(x, y), size = 0.2, data = court$paint) +
        geom_path(aes(x, y), size = 0.2, data = court$ft_circle) +
        geom_path(aes(x, y), size = 0.2, data = court$arc3) +
        coord_fixed() +
        theme_void() +
        theme(panel.background = element_rect(fill = "whitesmoke"),
              legend.position = "bottom") +
        labs(color = "")
    plt
}
