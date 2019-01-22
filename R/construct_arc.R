
#' Construct the coordinates of an arc
#'
#' @param x0 Center x coordinate
#' @param y0 Center y coordinate
#' @param r Radius
#' @param start In radians from 0 to pi
#' @param stop In radians from 0 to pi
#'
#' @return Data frame with x, y coordinates
#' @export
#'
#' @examples
#' circle <- construct_arc(x0 = 0, y0 = 0, r = 1, start = 0, stop = 2 * pi)
#' plot(circle$x, circle$y, type = "l")
#' arc <- construct_arc(2, 1, 5, start = pi, stop = pi / 2)
#' plot(arc$x, arc$y, type = "l")
construct_arc <- function(x0, y0, r, start, stop) {
    by <- ifelse(start <= stop, 0.01, -0.01)
    theta <- seq(start, stop, by)
    x <- x0 + r * cos(theta)
    y <- y0 + r * sin(theta)
    data.frame(x, y)
}
