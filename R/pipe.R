#' Pipe graphics
#'
#' Like dplyr, drawProteins also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#' From \url{https://github.com/rstudio/ggvis/blob/master/R/pipe.R}
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' # Allows this workflow:
#' five_rel_data %>%
#'     draw_canvas()
NULL
