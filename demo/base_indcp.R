library(indcp)

res_indcp <- base_indcp |>
  indcp(yname = "y", iname = "id", tname = "year", bname = "byear",
        kname = "rel_time")

summary.indcp(res_indcp, agg = "cage", compute_var = TRUE)
