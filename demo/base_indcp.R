library(indcp)

res_indcp <- base_indcp |>
  indcp(yname = "y", iname = "id", tname = "year", bname = "byear",
        kname = "rel_time")

res_indcp$df_indcp
library(tidyverse)
summary(res_indcp, agg = "cage") |>
  ggplot(aes(x = k, y = tau)) +
  geom_line() +
  facet_wrap(~ cage, nrow = 4)

base_indcp |> View()
