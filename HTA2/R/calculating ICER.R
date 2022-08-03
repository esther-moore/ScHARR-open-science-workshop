#'This function will calculate an ICER
#' @param delta_e
#' @param delta_c
#'
#' @return
#' @export
#'
#' @examples

calc_ICER <- function(delta_e, delta_c) {
  return(delta_c/delta_e)
}

calc_ICER(2,100)
