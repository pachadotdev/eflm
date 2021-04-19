library(bench)
library(dplyr)
library(stringr)

try(dir.create("inst/extdata", recursive = T))

trade_data_yotov_benchmark <- readRDS("dev/trade_data_yotov_benchmark.rds")

benchmark1 <- do.call("rbind", trade_data_yotov_benchmark$ols)
benchmark1$dimensions <- benchmark1$mm_rows * benchmark1$mm_cols
benchmark1$expression2 <- ifelse(grepl("elm", benchmark1$expression), "ELM", "LM")

benchmark1 %>%
  select(expression2, dimensions, median, mem_alloc) %>%
  mutate(
    median = str_trim(as.character(median)),
    median_units = str_extract(median, "[a-z]+"),
    median = as.numeric(str_remove(median, median_units)),
    median_milliseconds = case_when(
      median_units == "ms" ~ median,
      median_units == "s" ~ median * 1000
    ),

    mem_alloc = str_trim(as.character(mem_alloc)),
    mem_alloc_units = str_extract(mem_alloc, "[A-Z]+"),
    mem_alloc = as.numeric(str_remove(mem_alloc, mem_alloc_units)),
    mem_alloc_mb = case_when(
      mem_alloc_units == "MB" ~ mem_alloc
    )
  ) %>%
  select(expression2, dimensions, median_milliseconds, mem_alloc_mb) %>%
  readr::write_csv("inst/extdata/elm-benchmarks.csv")

benchmark2 <- do.call("rbind", trade_data_yotov_benchmark$ppml)
benchmark2$dimensions <- benchmark2$mm_rows * benchmark2$mm_cols
benchmark2$expression2 <- ifelse(grepl("eglm", benchmark2$expression), "EGLM", "GLM")

benchmark2 %>%
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
  select(expression2, dimensions, median_milliseconds, mem_alloc_mb) %>%
  readr::write_csv("inst/extdata/eglm-benchmarks.csv")
