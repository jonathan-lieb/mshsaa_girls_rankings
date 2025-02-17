# Jonathan Lieb
# 2/15/25

# This script adds a a quad ranking to the data frame
# based on the values of the columns in the data frame.
# data <- read_parquet("Data/modeling_data.parquet") |> 
#   filter(season >= 2015) |> 
#   weight_history()
# 
# quad_values <- data |> 
#   group_by(g) |> 
#   summarise(across(c(rpi, crpi), list(q25 = ~quantile(.x, 0.25),
#                                  q50 = ~quantile(.x, 0.5),
#                                  q75 = ~quantile(.x, 0.75))))

# g rpi_q25 rpi_q50 rpi_q75 crpi_q25 crpi_q50 crpi_q75
# <dbl>   <dbl>   <dbl>   <dbl>    <dbl>    <dbl>    <dbl>
# 1     0   0.379   0.5     0.617    0.379    0.5      0.617
# 2     1   0.415   0.489   0.581    0.329    0.482    0.663
# 3     2   0.441   0.497   0.557    0.325    0.478    0.671
# 4     3   0.440   0.497   0.553    0.327    0.472    0.669
# 5     4   0.440   0.497   0.554    0.330    0.470    0.666
# 6     5   0.440   0.497   0.553    0.332    0.473    0.662
# 7     6   0.440   0.497   0.554    0.333    0.472    0.661
# 8     7   0.440   0.495   0.551    0.336    0.475    0.658
# 9     8   0.439   0.495   0.551    0.337    0.472    0.659
# 10     9   0.438   0.494   0.550    0.337    0.471    0.658
# 11    10   0.437   0.492   0.549    0.337    0.468    0.656
# 12    11   0.438   0.492   0.548    0.337    0.469    0.658
# 13    12   0.439   0.493   0.548    0.338    0.469    0.656
# 14    13   0.439   0.494   0.549    0.337    0.468    0.658
# 15    14   0.440   0.494   0.550    0.337    0.468    0.655
# 16    15   0.442   0.496   0.550    0.336    0.466    0.654
# 17    16   0.442   0.495   0.551    0.336    0.467    0.654
# 18    17   0.442   0.495   0.552    0.337    0.467    0.653
# 19    18   0.441   0.497   0.552    0.336    0.465    0.653
# 20    19   0.443   0.498   0.553    0.336    0.466    0.657
# 21    20   0.445   0.499   0.553    0.337    0.466    0.656
# 22    21   0.448   0.502   0.555    0.338    0.466    0.657
# 23    22   0.450   0.504   0.557    0.338    0.467    0.660
# 24    23   0.456   0.509   0.559    0.342    0.473    0.666
# 25    24   0.464   0.516   0.565    0.347    0.488    0.672
# 26    25   0.476   0.526   0.573    0.351    0.497    0.679
# 27    26   0.495   0.540   0.585    0.354    0.508    0.687
# 28    27   0.516   0.558   0.599    0.358    0.515    0.694
# 29    28   0.538   0.581   0.614    0.366    0.529    0.713
# 30    29   0.557   0.598   0.623    0.376    0.536    0.714
# 31    30   0.581   0.612   0.632    0.387    0.556    0.718
# 32    31   0.595   0.619   0.637    0.395    0.543    0.704
# 33    32   0.599   0.620   0.639    0.399    0.512    0.639
# 34    33   0.622   0.635   0.640    0.440    0.515    0.583
# 35    34   0.564   0.599   0.615    0.390    0.451    0.516
# 36    35   0.600   0.600   0.600    0.329    0.329    0.329
# 37    36   0.609   0.609   0.609    0.332    0.332    0.332
# print(quad_values, n = 100)
# 
# write_rds(quad_values, "Data/quad_values.rds")

add_quad <- function(df, quad_values) {
  df |> 
    left_join(quad_values, by = "g") |> 
    left_join(quad_values, by = c("g_opp" = "g"), suffix = c("", "_qopp")) |>
    mutate(rpi_q = case_when(
      rpi > rpi_q75 ~ 1,
      rpi > rpi_q50 & rpi <= rpi_q75 ~ 2,
      rpi > rpi_q25 & rpi <= rpi_q50 ~ 3,
      rpi <= rpi_q25 ~ 4
    ),
    crpi_q = case_when(
      crpi > crpi_q75 ~ 1,
      crpi > crpi_q50 & crpi <= crpi_q75 ~ 2,
      crpi > crpi_q25 & crpi <= crpi_q50 ~ 3,
      crpi <= crpi_q25 ~ 4
    ),
    quad = rpi_q * crpi_q,
    rpi_opp_q = case_when(
      rpi_opp > rpi_q75_qopp ~ 1,
      rpi_opp > rpi_q50_qopp & rpi_opp <= rpi_q75_qopp ~ 2,
      rpi_opp > rpi_q25_qopp & rpi_opp <= rpi_q50_qopp ~ 3,
      rpi_opp <= rpi_q25_qopp ~ 4
    ),
    crpi_opp_q = case_when(
      crpi_opp > crpi_q75_qopp ~ 1,
      crpi_opp > crpi_q50_qopp & crpi_opp <= crpi_q75_qopp ~ 2,
      crpi_opp > crpi_q25_qopp & crpi_opp <= crpi_q50_qopp ~ 3,
      crpi_opp <= crpi_q25_qopp ~ 4
    ),
    quad_opp = rpi_opp_q * crpi_opp_q,
    ) |> 
    select(-ends_with("q"), -ends_with("_qopp"))
}
