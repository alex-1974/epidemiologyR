require(rlang)
require(zoo)
require(tidyverse)

#' Calculate incidence
#'
#' @param .data Dataframe or Tibble
#' @param col Column to calculate
#' @param pop Population size
#' @param days Timeframe to calculate
#' @param groups Group by
#' @param prefix Prefix (default 'ìnc_')
#' @example \dontrun{incidence(.data = df, cases = a, pop = p, day = 7, groups = c(g, g2))}
#' @export
incidence <- function(.data, cases, pop, days, groups, prefix = "inc_") {
  cases <- enquo(cases)
  name <- quo_name(cases)
  name <- paste0(prefix, name)
  days <- enquo(days)
  pop <- enquo(pop)
  groups <- as.list(enexpr(groups))
  print(groups)
  groups <- if(length(groups) > 1) groups[-1] else groups
  .data |>
    group_by(!!!groups) |>
    mutate(
      !!name := (zoo::rollsum(!!cases, !!days, align = "right", fill = 0) / !!pop) * 100000
      )
}
