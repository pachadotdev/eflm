library(eflm)
library(stringr)
library(ggplot2)
library(patchwork)
library(bench)

trade_data_yotov <- readRDS("benchmarks/00-trade-data-yotov.rds")

# PPML

y <- unique(trade_data_yotov$year)

trade_data_yotov_benchmark <- list()

model <- "trade ~ log(dist) + cntg + lang + clny + exp_year + imp_year"

for (i in seq_along(y)) {
  d <- trade_data_yotov[trade_data_yotov$year <= y[i], ]
  
  out <- bench::mark(
    glm(model, data = d,
        family = quasipoisson(link = "log"), y = F, model = F)$coefficients,
    eglm(model, data = d,
         family = quasipoisson(link = "log"), y = F, model = F, reduce = T)$coefficients,
    iterations = 5L
  )
  
  mm <- model.matrix(~log(dist) + cntg + lang + clny + exp_year + imp_year, d)
  
  out$mm_rows <- nrow(mm)
  out$mm_cols <- ncol(mm)
  
  trade_data_yotov_benchmark[[i]] <- out
}

gc()

# saveRDS(trade_data_yotov_benchmark, file = "benchmarks/trade_data_yotov_benchmark.rds", compress = "xz")

benchmark <- do.call("rbind", trade_data_yotov_benchmark)
benchmark$dimensions <- benchmark$mm_rows * benchmark$mm_cols
benchmark$expression2 <- ifelse(grepl("eglm", benchmark$expression), "EGLM", "GLM")

benchmark <- benchmark %>%
  select(expression2, dimensions, median, mem_alloc) %>%
  mutate(
    median = str_trim(as.character(median)),
    median_units = str_extract(median, "[a-z]+"),
    median = as.numeric(str_remove(median, median_units)),
    median_milliseconds = case_when(
      median_units == "ms" ~ median,
      median_units == "s" ~ median * 1000,
      median_units == "m" ~ median * 60000
    ),
    
    mem_alloc = str_trim(as.character(mem_alloc)),
    mem_alloc_units = str_extract(mem_alloc, "[A-Z]+"),
    mem_alloc = as.numeric(str_remove(mem_alloc, mem_alloc_units)),
    mem_alloc_mb = case_when(
      mem_alloc_units == "MB" ~ mem_alloc,
      mem_alloc_units == "GB" ~ mem_alloc * 1024
    )
  ) %>%
  select(expression2, dimensions, median_milliseconds, mem_alloc_mb)

g1 <- ggplot(benchmark, aes(x = dimensions, y = median_milliseconds, color = expression2)) +
  geom_point() +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  labs(title = "R Time Benchmark",
       x = "Entries in the design matrix",
       y = "Fitting Time (Milliseconds)") +
  scale_color_manual(values = c("#3B9AB2", "#E1AF00"))

g2 <- ggplot(benchmark, aes(x = dimensions, y = mem_alloc_mb, color = expression2)) +
  geom_point() +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  labs(title = "R Memory Benchmark",
       x = "Entries in the design matrix",
       y = "Required Memory (MB)") +
  scale_color_manual(values = c("#3B9AB2", "#E1AF00"))

g <- list(g1 = g1, g2 = g2)

saveRDS(g, "benchmarks/05-benchmark-subsets.rds")
