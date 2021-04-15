library(yotover)
library(dplyr)

ch1_application1_2 <-  yotov_data("ch1_application1") %>%
  filter(year %in% seq(1986, 2006, 4))

ch1_application1_2 <- ch1_application1_2 %>%
  mutate(
    log_trade = log(trade),
    log_dist = log(dist)
  )

ch1_application1_2 <- ch1_application1_2 %>%
  # Create Yit
  group_by(exporter, year) %>%
  mutate(
    y = sum(trade),
    log_y = log(y)
  ) %>%

  # Create Eit
  group_by(importer, year) %>%
  mutate(
    e = sum(trade),
    log_e = log(e)
  )

ch1_application1_2 <- ch1_application1_2 %>%
  # Replicate total_e
  group_by(exporter, year) %>%
  mutate(total_e = sum(e)) %>%
  group_by(year) %>%
  mutate(total_e = max(total_e)) %>%

  # Replicate rem_exp
  group_by(exporter, year) %>%
  mutate(
    remoteness_exp = sum(dist *  total_e / e),
    log_remoteness_exp = log(remoteness_exp)
  ) %>%

  # Replicate total_y
  group_by(importer, year) %>%
  mutate(total_y = sum(y)) %>%
  group_by(year) %>%
  mutate(total_y = max(total_y)) %>%

  # Replicate rem_imp
  group_by(importer, year) %>%
  mutate(
    remoteness_imp = sum(dist / (y / total_y)),
    log_remoteness_imp = log(remoteness_imp)
  )

ch1_application1_2 <- ch1_application1_2 %>%
  # This merges the columns exporter/importer with year
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year)
  )

trade_data_yotov <- ch1_application1_2 %>%
  filter(exporter != importer)

trade_data_yotov <- trade_data_yotov %>%
  ungroup() %>%
  select(year, trade, dist, cntg, lang, clny, exp_year, imp_year)

use_data(trade_data_yotov, compress = "xz", overwrite = T)
