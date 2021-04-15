ggplot(trade_demo_benchmark2, aes(x = dimensions, y = median, color = expression)) +
  geom_point() +
  geom_line() +
  bench::scale_y_bench_time() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  # coord_fixed() +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "log-log plot of number of observations vs fitting time",
       x = "number of observations",
       y = "fitting time")

ggplot(trade_demo_benchmark2, aes(x = dimensions, y = mem_alloc, color = expression2)) +
  geom_point() +
  geom_line() +
  bench::scale_y_bench_bytes() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "log-log plot of number of observations vs memory allocation",
       x = "number of observations",
       y = "fitting time")
