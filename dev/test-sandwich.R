library(sandwich)

x = fglm(
  trade ~ log_dist + cntg + lang + clny + log_y + log_e,
  data = filter(ch1_application1_2, trade > 0),
  family = quasipoisson(),
  x = FALSE
)

y = glm(
  trade ~ log_dist + cntg + lang + clny + log_y + log_e,
  data = filter(ch1_application1_2, trade > 0),
  family = quasipoisson()
)

colnames(model.matrix(x))
colnames(model.matrix(y))
all.equal(model.matrix(x), model.matrix(y))

all.equal(estfun(x), estfun(y))

vcov_x <- sandwich::vcovCL(
  x,
  cluster = eval(x$call$data)[,"pair_id"]
)

vcov_y <- sandwich::vcovCL(
  y,
  cluster = y$data[,"pair_id"]
)

all.equal(vcov_x, vcov_y)
