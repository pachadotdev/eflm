library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(forcats)

tidy_benchmarks <- read_excel("data/tidy benchmarks.xlsx")

tidy_benchmarks <- tidy_benchmarks %>% 
  select(-expr) %>% 
  gather(Statistic, Time, -Hardware, -Function) %>% 
  filter(
    Statistic %in% c("Min", "Median", "Max"),
    Hardware != "4 dedicated CPUs, 8 GB RAM, SSD disk"
  ) %>% 
  mutate(
    Statistic = fct_relevel(Statistic, "Min", "Median", "Max")
  )

g <- ggplot(tidy_benchmarks) +
  geom_col(aes(x = `Function`, y = Time, fill = Hardware), position = "dodge2") +
  facet_wrap(~Statistic) +
  theme_minimal(base_size = 8) +
  scale_fill_manual(values = c("#3B9AB2", "#E1AF00", "#a8b084", "#d08c62")) +
  # theme(legend.position = "bottom", legend.direction = "vertical") +
  labs(y = "Fitting Time (Seconds)")

saveRDS(g, "benchmarks/07-r-digitalocean.rds")
