library(eflm)

model <- "trade ~ log_dist + cntg + lang + clny + exp_year + imp_year"
y <- unique(trade_demo_glm$year)
trade_demo_benchmark <- list()

for (i in seq_along(y)) {
  d <- trade_demo_glm[trade_demo_glm$year <= y[i], ]

  out <- bench::mark(
    glm(model, data = d,
        family = quasipoisson(link = "log"))$coefficients,
    eglm(model, data = d,
         family = quasipoisson(link = "log"), bypass = F)$coefficients
  )
  out$dimensions <- nrow(d) * ncol(d)
  out$link <- nrow(d) * ncol(d)

  trade_demo_benchmark[[i]] <- out
}

use_data(trade_demo_benchmark, compress = "xz")
