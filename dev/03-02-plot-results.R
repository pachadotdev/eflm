library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(wesanderson)
library(forcats)

# the results for each droplet were manually downloaded from each RStudio Server
# instance at http://123.456.789:8787 and then merged in Google Sheets

tidy_benchmarks <- read_excel("tidy benchmarks.xlsx")

tidy_benchmarks %>%
  select(-expr, -neval) %>%
  gather(Statistic, `Time (Seconds)`, -Hardware, -Function) %>%
  mutate(
    Statistic = as_factor(Statistic),
    Statistic = fct_relevel(Statistic, "Min", "LQ", "Median",
                            "Mean", "UQ", "Max")
  ) %>%
  ggplot() +
  geom_col(aes(x = Function, y = `Time (Seconds)`, fill = Hardware), position = "dodge2") +
  facet_wrap(~Statistic) +
  scale_fill_manual(values = wes_palette("Zissou1")) +
  labs(title = "Benchmarking Time on DigitalOcean Droplets") +
  theme_minimal(base_size = 18)
