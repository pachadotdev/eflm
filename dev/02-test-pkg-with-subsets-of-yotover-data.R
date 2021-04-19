library(eflm)
if (!require(bench)) install.packages("bench")

# OLS

trade_data_yotov_benchmark1 <- list()

model <- "log(trade) ~ log(dist) + cntg + lang + clny + exp_year + imp_year"

y <- unique(trade_data_yotov$year)

for (i in seq_along(y)) {
  d <- trade_data_yotov[trade_data_yotov$year <= y[i], ]
  d <- d[d$trade > 0, ]

  out <- bench::mark(
    lm(model, data = d)$coefficients,
    elm(model, data = d, bypass = F)$coefficients
  )

  mm <- model.matrix(~log(dist) + cntg + lang + clny + exp_year + imp_year, d)

  out$mm_rows <- nrow(mm)
  out$mm_cols <- ncol(mm)

  trade_data_yotov_benchmark1[[i]] <- out
}

gc()

# PPML

trade_data_yotov_benchmark2 <- list()

model <- "trade ~ log(dist) + cntg + lang + clny + exp_year + imp_year"

for (i in seq_along(y)) {
  d <- trade_data_yotov[trade_data_yotov$year <= y[i], ]

  out <- bench::mark(
    glm(model, data = d,
        family = quasipoisson(link = "log"),
        y = FALSE,
        model = FALSE)$coefficients,
    eglm(model, data = d,
         family = quasipoisson(link = "log"),
         bypass = F,
         y = FALSE,
         model = FALSE)$coefficients
  )

  mm <- model.matrix(~log(dist) + cntg + lang + clny + exp_year + imp_year, d)

  out$mm_rows <- nrow(mm)
  out$mm_cols <- ncol(mm)

  trade_data_yotov_benchmark2[[i]] <- out
}

gc()

trade_data_yotov_benchmark <- list(
  ols = trade_data_yotov_benchmark1,
  ppml = trade_data_yotov_benchmark2
)

saveRDS(trade_data_yotov_benchmark, file = "dev/trade_data_yotov_benchmark.rds", compress = "xz")
