
#' Incidence
incidence <- function(days, per, fill) {
  new_function(
    exprs(cases =, pop = ),
    expr({
      (zoo::rollsum(cases, !!days, align = "right", fill = !!fill) / pop) * !!per
    }),
    caller_env()
  )
}
