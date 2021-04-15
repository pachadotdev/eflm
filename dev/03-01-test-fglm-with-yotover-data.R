# NEVER PUT THIS ON TESTS/ FOLDER
# THE DATA IS TOO LARGE AND TAKES +5 MINUTES TO FIT THE MODEL

# the results for each droplet were manually downloaded from each RStudio Server
# instance at http://123.456.789:8787 and then merged in Google Sheets

droplet <- "c2-2vcpu-4gb"

if (!require(dplyr)) install.packages("dplyr")
if (!require(eflm)) install.packages("eflm")
if (!require(microbenchmark)) install.packages("microbenchmark")
if (!require(readr)) install.packages("readr")

library(dplyr)
library(eflm)
library(microbenchmark)
library(readr)

benchmark_times <- microbenchmark(
  glm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,
      family = quasipoisson(link = "log"),
      data = trade_demo_glm,
      y = FALSE,
      model = FALSE
  ),
  eglm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,
       family = quasipoisson(link = "log"),
       data = trade_demo_glm,
       y = FALSE,
       model = FALSE
  ),
  times = 500L
)

benchmark_times_df <- microbenchmark:::print.microbenchmark(benchmark_times)

write_csv(benchmark_times_df, sprintf("dev/benchmarktimes_%s.csv", droplet))
