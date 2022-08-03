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



delta_ce <- function(e1, c1, e0, c0) {
  delta_e <- e1 - e0
  delta_c <- c1 - c0
  return(c(delta_e, delta_c))
}

delta_ce(0.9, 100, 0.5, 50)


ce_to_ICER <- function(e1, c1, e0, c0) {
  incr_ce <- delta_ce(e1, c1, e0, c0)
  icer <- calc_ICER(incr_ce[1], incr_ce[2])
  return(icer)
}
ce_to_ICER(0.9, 100, 0.5, 50)

calc_ICER(delta_ce(0.9, 100, 0.5, 50))

delta_ce2 <- function(e1, c1, e0, c0) {
  delta_e <- e1 - e0
  delta_c <- c1 - c0
  return(list(delta_e = delta_e,
              delta_c = delta_c))
}

do.call(calc_ICER, args = delta_ce2(0.9, 100, 0.5, 50))

ce_to_INMB <- function(e1, c1, e0, c0) {
  delta_ce(e1, c1, e0, c0) |>
    calc_INMB2()
}
ce_stat <- function(stat) {
  stat_fn <- 
    if (stat == "INMB") {
      calc_INMB2
    } else {
      calc_ICER2}
  
  function(e1, c1, e0, c0) {
    delta_ce(e1, c1, e0, c0) |> 
      stat_fn()
  }
}

