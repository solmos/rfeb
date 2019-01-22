#' Construct coordinates of basketball court
#'
#' @return A list containing data frames corresponding to different parts of the court
#' @export
#'
#' @examples
#' court <- construct_court()
construct_court <- function() {
    outer_lines <- data.frame(
        x = c(0, 0, 1500, 1500, 0),
        y = c(0, 1400, 1400, 0, 0),
        type = "Outer lines"
    )

    paint <- data.frame(
        x = c(750 - 175, 750 - 175, 750 + 175, 750 + 175),
        y = c(0, 580, 580, 0),
        type = "Paint"
    )

    ft_circle <- data.frame(
        construct_arc(x0 = 750, y0 = 580, r = 175, start = 0, stop = 2 * pi),
        type = "FT circle"
    )

    upper_arc3 <- data.frame(
        construct_arc(x0 = 750, y0 = 157.5, r = 625, start = 0, stop = pi),
        type = "Upper arc"
    )

    left_corner3 <- data.frame(
        x = c(125, 125),
        y = c(0, 157.5),
        type = "Left corner 3"
    )

    right_corner3 <- data.frame(
        x = c(1500 - 125, 1500 - 125),
        y = c(157.5, 0),
        type = "Right corner 3"
    )

    arc3 <- rbind(right_corner3, upper_arc3, left_corner3)

    court <- list(outer_lines = outer_lines, paint = paint,
                  ft_circle = ft_circle, arc3 = arc3)

    court
}
