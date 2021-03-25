library(digitalocean)
library(dplyr)
library(purrr)

# oauth needed for r,s!
r <- regions()
s <- sizes()

# get all the droplets in the TOR1 region
s <- s %>%
  filter(
    grepl("tor", region),
    grepl("vcpu", slug),
    memory >= 4096
  ) %>%
  arrange(slug)

droplets <- as.list(s$slug)
names(droplets) <- janitor::make_clean_names(s$slug)

# create the 9 droplets (R + RStudio included)
map(
  seq_along(s$slug),
  function(x) {
    droplets[[x]] <- droplet_create(name = s$slug[x], size = s$slug[x], wait = F)
  }
)
