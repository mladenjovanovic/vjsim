lm_mod <- lm(Sepal.Length~., iris)

vip::vip(lm_mod, train = iris, method = "permute", target = "Sepal.Length", metric = "RMSE", pred_wrapper = predict, nsim=10)
vip::vip(lm_mod, method = "firm")


pdp::partial(
  lm_mod,
  train = iris,
  pred.var = "Sepal.Width",
  plot = TRUE,
  rug = FALSE,
  ice = TRUE,
  plot.engine = "ggplot2",
  alpha = 0.1
)
