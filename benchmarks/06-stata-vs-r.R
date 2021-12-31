library(tidyverse)
library(readxl)

# tidy_benchmarks <- read_excel("data/tidy benchmarks.xlsx") %>%
#   select(-expr, -neval) %>%
#   gather(Statistic, `Time (Seconds)`, -Hardware, -Function) %>%
#   filter(!grepl("2 dedicated|SSD", Hardware)) %>%
#   mutate(
#     Hardware = gsub(", NVMe disk| dedicated", "", Hardware),
#     Statistic = as_factor(Statistic),
#     Statistic = fct_relevel(Statistic, "Min", "LQ", "Median",
#                             "Mean", "UQ", "Max")
#   )

# tidy_benchmarks <- tidy_benchmarks %>% 
#   filter(Hardware == "8 CPUs, 16 GB RAM", Statistic == "Median") %>%  
#   select(Function, `Time (Seconds)`) %>% 
#   mutate(Software = "R")

trade_data_yotov_benchmark <- readRDS("~/github/tesis-magister/benchmarks/trade_data_yotov_benchmark.rds")
tidy_benchmarks <- do.call("rbind", trade_data_yotov_benchmark)
tidy_benchmarks$`Function` <- ifelse(grepl("eglm", tidy_benchmarks$expression), "EGLM", "GLM")

tidy_benchmarks <- tidy_benchmarks %>% 
  filter(mm_rows == max(mm_rows)) %>% 
  select(`Function`, median) %>% 
  mutate(
    median = str_trim(as.character(median)),
    median_units = str_extract(median, "[a-z]+"),
    median = as.numeric(str_remove(median, median_units)),
    `Fitting Time (Seconds)` = case_when(
      median_units == "s" ~ median,
      median_units == "m" ~ median * 60
    )) %>% 
  mutate(`Software` = "R") %>% 
  select(-c(median, median_units))

stata_bench <- read_excel("data/stata_bench.xlsx") %>% 
  group_by(command) %>% 
  filter(Obs == max(Obs)) %>% 
  mutate(Function = ifelse(grepl("glm", command), "Stata REG", "Stata GLM")) %>% 
  filter(Function != "Stata REG") %>% 
  rename(`Fitting Time (Seconds)` =  Time) %>% 
  ungroup() %>% 
  select(Function, `Fitting Time (Seconds)`) %>% 
  mutate(Software = "Stata")

tidy_benchmarks2 <- bind_rows(tidy_benchmarks, stata_bench)

g <- ggplot(tidy_benchmarks2) + 
  geom_col(aes(x = Function, y = `Fitting Time (Seconds)`, fill = `Function`), position = "dodge2") + 
  scale_fill_manual(values = c("#3B9AB2", "#3B9AB2", "#E1AF00")) +
  labs(title = "R vs Stata Benchmark") + 
  theme_minimal(base_size = 8) +
  theme(legend.position = "none")

saveRDS(g, "benchmarks/06-stata-vs-r.rda")
