# Dynamically exported, see zzz.R
# taken from broom::: but adapted to use base (makes tidy.fglm work)
exponentiate <- function (data) {
  data[, "estimate"] <- exp(data[, "estimate"])
  if ("conf.low" %in% colnames(data)) {
    data[, "conf.low"] <- exp(data[, "conf.low"])
    data[, "conf.high"] <- exp(data[, "conf.high"])
  }
  return(data)
}
