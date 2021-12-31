library(ggplot2)
library(dplyr)
library(bench)
library(ggplot2)
library(stringr)

load("benchmarks/01-chapter1-traditional-gravity-bench.RData")

bench_traditional_gravity <- bind_rows(
  bench_fe %>% mutate(model = "TG, FE", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ols %>% mutate(model = "TG, OLS", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ols_remoteness %>% mutate(model = "TG, OLS REM", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ppml %>% mutate(model = "TG, PPML", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc)
)

rm(list = grep("bench_traditional_gravity", ls(), value = TRUE, invert = TRUE))

load("benchmarks/02-chapter1-distance-puzzle-bench.RData")

bench_distance_puzzle <- bind_rows(
  bench_fe %>% mutate(model = "DP, FE", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ols %>% mutate(model = "DP, OLS", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_home_bias %>% mutate(model = "DP, HB", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_internal_distance %>% mutate(model = "DP, ID", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ppml %>% mutate(model = "DP, PPML", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc)
)

rm(list = grep("bench_traditional_gravity|bench_distance_puzzle", ls(), value = TRUE, invert = TRUE))

load("benchmarks/03-chapter1-regional-trade-agreements-bench.RData")

bench_rtas <- bind_rows(
  bench_ols %>% mutate(model = "RTAs, OLS", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_endogeneity %>% mutate(model = "RTAs, EN", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_globalization %>% mutate(model = "RTAs, GB", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_phasing %>% mutate(model = "RTAs, PH", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_ppml %>% mutate(model = "RTAs, PPML", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_reverse_causality %>% mutate(model = "RTAs, RC", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc),
  bench_trade_diversion %>% mutate(model = "RTAs, TD", fun = gsub("\\(.*", "", expression)) %>% select(model, fun, median, mem_alloc)
)

rm(list = grep("bench_traditional_gravity|bench_distance_puzzle|bench_rtas", ls(), value = TRUE, invert = TRUE))

bench_models <- bind_rows(
  bench_traditional_gravity,
  bench_distance_puzzle,
  bench_rtas
)

bench_models <- bench_models %>%
  # filter(fun %in% c("eglm", "glm")) %>%
  mutate(`Function` = toupper(fun)) %>%
  mutate(
    median = str_trim(as.character(median)),
    median_units = str_extract(median, "[a-z]+"),
    median = as.numeric(str_remove(median, median_units)),
    median_seconds = case_when(
      median_units == "ms" ~ median / 1000,
      median_units == "s" ~ median,
      median_units == "m" ~ median * 60
    ),

    mem_alloc = str_trim(as.character(mem_alloc)),
    mem_alloc_units = str_extract(mem_alloc, "[A-Z]+"),
    mem_alloc = as.numeric(str_remove(mem_alloc, mem_alloc_units)),
    mem_alloc_mb = case_when(
      mem_alloc_units == "MB" ~ mem_alloc,
      mem_alloc_units == "GB" ~ mem_alloc * 1024
    )
  )

# colors <- c("#1c26b3", "#d05555", "#549e95", "#993f7b")

g2_time <- ggplot(bench_models %>% filter(fun %in% c("eglm", "glm"))) +
  geom_col(aes(x = model, y = median_seconds, fill = `Function`), position = "dodge2") +
  scale_fill_manual(values = c("#3B9AB2", "#E1AF00")) +
  theme_minimal() +
  # theme(legend.position = "none") +
  labs(x = "Model", y = "Median Time (in Seconds)",
       title = "GLM vs EGLM - Time Benchmark")

g2_memory <- ggplot(bench_models %>% filter(fun %in% c("eglm", "glm"))) +
  geom_col(aes(x = model, y = mem_alloc_mb, fill = `Function`), position = "dodge2") +
  scale_fill_manual(values = c("#3B9AB2", "#E1AF00")) +
  theme_minimal() +
  # theme(legend.position = "none") +
  labs(x = "Model", y = "Allocated Memory (in MB)",
       title = "GLM vs EGLM - Memory Usage Benchmark")

load("benchmarks/01-chapter1-traditional-gravity.RData")

object_size_traditional_gravity <- tibble(
  model = c(rep("TG, OLS", 2), rep("TG, OLS REM", 2), rep("TG, FE", 2), rep("TG, PPML", 2)),
  fun = c(rep(c("lm", "elm"), 3), c("glm", "eglm")),
  size = c(
    object.size(fit_ols_lm),
    object.size(fit_ols_elm),
    object.size(fit_ols_remoteness_lm),
    object.size(fit_ols_remoteness_elm),
    object.size(fit_fe_lm),
    object.size(fit_fe_elm),
    object.size(fit_ppml_glm),
    object.size(fit_ppml_eglm)
  )
) %>%
  mutate(size = size / 1000^2)

model_matrix_traditional_gravity <- tibble(
  model = c(rep("TG, OLS", 2), rep("TG, OLS REM", 2), rep("TG, FE", 2), rep("TG, PPML", 2)),
  fun = c(rep(c("lm", "elm"), 3), c("glm", "eglm")),
  model_matrix_nrow = c(
    dim(fit_ols_lm$qr$qr)[1],
    fit_ols_elm$qr$original.dimensions[1],
    dim(fit_ols_remoteness_lm$qr$qr)[1],
    fit_ols_remoteness_elm$qr$original.dimensions[1],
    dim(fit_fe_lm$qr$qr)[1],
    fit_fe_elm$qr$original.dimensions[1],
    dim(fit_ppml_glm$qr$qr)[1],
    fit_ppml_eglm$qr$original.dimensions[1]
  ),
  model_matrix_ncol = c(
    dim(fit_ols_lm$qr$qr)[2],
    fit_ols_elm$qr$original.dimensions[2],
    dim(fit_ols_remoteness_lm$qr$qr)[2],
    fit_ols_remoteness_elm$qr$original.dimensions[2],
    dim(fit_fe_lm$qr$qr)[2],
    fit_fe_elm$qr$original.dimensions[2],
    dim(fit_ppml_glm$qr$qr)[2],
    fit_ppml_eglm$qr$original.dimensions[2]
  )
)

rm(fit_fe_elm, fit_fe_lm, fit_ols_elm, fit_ols_lm, fit_ols_remoteness_elm,
   fit_ols_remoteness_lm, fit_ppml_eglm, fit_ppml_glm)

load("benchmarks/02-chapter1-distance-puzzle.RData")

object_size_distance_puzzle <- tibble(
  model = c(rep("DP, OLS", 2), rep("DP, FE", 2), rep("DP, HB", 2), rep("DP, ID", 2), rep("DP, PPML", 2)),
  fun = c("lm", "elm", rep(c("glm", "eglm"), 4)),
  size = c(
    object.size(fit_ols_lm),
    object.size(fit_ols_elm),
    object.size(fit_fe_glm),
    object.size(fit_fe_eglm),
    object.size(fit_home_bias_glm),
    object.size(fit_home_bias_eglm),
    object.size(fit_internal_distance_glm),
    object.size(fit_internal_distance_eglm),
    object.size(fit_ppml_glm),
    object.size(fit_ppml_eglm)
  )
) %>%
  mutate(size = size / 1000^2)

model_matrix_distance_puzzle <- tibble(
  model = c(rep("DP, OLS", 2), rep("DP, FE", 2), rep("DP, HB", 2), rep("DP, ID", 2), rep("DP, PPML", 2)),
  fun = c("lm", "elm", rep(c("glm", "eglm"), 4)),
  model_matrix_nrow = c(
    dim(fit_ols_lm$qr$qr)[1],
    fit_ols_elm$qr$original.dimensions[1],
    dim(fit_fe_glm$qr$qr)[1],
    fit_fe_eglm$qr$original.dimensions[1],
    dim(fit_home_bias_glm$qr$qr)[1],
    fit_home_bias_eglm$qr$original.dimensions[1],
    dim(fit_internal_distance_glm$qr$qr)[1],
    fit_internal_distance_eglm$qr$original.dimensions[1],
    dim(fit_ppml_glm$qr$qr)[1],
    fit_ppml_eglm$qr$original.dimensions[1]
  ),
  model_matrix_ncol = c(
    dim(fit_ols_lm$qr$qr)[2],
    fit_ols_elm$qr$original.dimensions[2],
    dim(fit_fe_glm$qr$qr)[2],
    fit_fe_eglm$qr$original.dimensions[2],
    dim(fit_home_bias_glm$qr$qr)[2],
    fit_home_bias_eglm$qr$original.dimensions[2],
    dim(fit_internal_distance_glm$qr$qr)[2],
    fit_internal_distance_eglm$qr$original.dimensions[2],
    dim(fit_ppml_glm$qr$qr)[2],
    fit_ppml_eglm$qr$original.dimensions[2]
  )
)

rm(fit_fe_eglm, fit_fe_glm, fit_home_bias_eglm, fit_home_bias_glm,
   fit_internal_distance_eglm, fit_internal_distance_glm, fit_ols_elm,
   fit_ols_lm, fit_ppml_eglm, fit_ppml_glm)

load("benchmarks/03-chapter1-regional-trade-agreements.RData")

object_size_rtas <- tibble(
  model = c(rep("RTAs, OLS", 2), rep("RTAs, END", 2), rep("RTAs, GB", 2), rep("RTAs, PHA", 2),
            rep("RTAs, PPML", 2), rep("RTAs, REV", 2), rep("RTAs, DIV", 2)),
  fun = c("lm", "elm", rep(c("glm", "eglm"), 6)),
  size = c(
    object.size(fit_ols_lm),
    object.size(fit_ols_elm),
    object.size(fit_endogeneity_glm),
    object.size(fit_endogeneity_eglm),
    object.size(fit_globalization_glm),
    object.size(fit_globalization_eglm),
    object.size(fit_phasing_glm),
    object.size(fit_phasing_eglm),
    object.size(fit_ppml_glm),
    object.size(fit_ppml_eglm),
    object.size(fit_reverse_causality_glm),
    object.size(fit_reverse_causality_eglm),
    object.size(fit_trade_diversion_glm),
    object.size(fit_trade_diversion_eglm)
  )
) %>%
  mutate(size = size / 1000^2)

model_matrix_rtas <- tibble(
  model = c(rep("RTAs, OLS", 2), rep("RTAs, END", 2), rep("RTAs, GB", 2), rep("RTAs, PHA", 2),
            rep("RTAs, PPML", 2), rep("RTAs, REV", 2), rep("RTAs, DIV", 2)),
  fun = c("lm", "elm", rep(c("glm", "eglm"), 6)),
  model_matrix_nrow = c(
    dim(fit_ols_lm$qr$qr)[1],
    fit_ols_elm$qr$original.dimensions[1],
    dim(fit_endogeneity_glm$qr$qr)[1],
    fit_endogeneity_eglm$qr$original.dimensions[1],
    dim(fit_globalization_glm$qr$qr)[1],
    fit_globalization_eglm$qr$original.dimensions[1],
    dim(fit_phasing_glm$qr$qr)[1],
    fit_phasing_eglm$qr$original.dimensions[1],
    dim(fit_ppml_glm$qr$qr)[1],
    fit_ppml_eglm$qr$original.dimensions[1],
    dim(fit_reverse_causality_glm$qr$qr)[1],
    fit_reverse_causality_eglm$qr$original.dimensions[1],
    dim(fit_trade_diversion_glm$qr$qr)[1],
    fit_trade_diversion_eglm$qr$original.dimensions[1]
  ),
  model_matrix_ncol = c(
    dim(fit_ols_lm$qr$qr)[2],
    fit_ols_elm$qr$original.dimensions[2],
    dim(fit_endogeneity_glm$qr$qr)[2],
    fit_endogeneity_eglm$qr$original.dimensions[2],
    dim(fit_globalization_glm$qr$qr)[2],
    fit_globalization_eglm$qr$original.dimensions[2],
    dim(fit_phasing_glm$qr$qr)[2],
    fit_phasing_eglm$qr$original.dimensions[2],
    dim(fit_ppml_glm$qr$qr)[2],
    fit_ppml_eglm$qr$original.dimensions[2],
    dim(fit_reverse_causality_glm$qr$qr)[2],
    fit_reverse_causality_eglm$qr$original.dimensions[2],
    dim(fit_trade_diversion_glm$qr$qr)[2],
    fit_trade_diversion_eglm$qr$original.dimensions[2]
  )
)

rm(fit_endogeneity_eglm, fit_endogeneity_glm, fit_globalization_eglm,
   fit_globalization_glm, fit_ols_elm, fit_ols_lm, fit_phasing_eglm,
   fit_phasing_glm, fit_ppml_eglm, fit_ppml_glm, fit_reverse_causality_eglm,
   fit_reverse_causality_glm,  fit_trade_diversion_eglm, fit_trade_diversion_glm)

save.image("benchmarks/04-glm-vs-eglm.RData")
