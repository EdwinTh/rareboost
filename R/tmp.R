n1 = 25; n0 = 975
df <- data.frame(
  y = rep(1:0, c(n1, n0)),
  x1 = c(rnorm(n1), rnorm(n0, 1)),
  x2 = c(rnorm(n1), rnorm(n0, 1))
)

rareboost(x = df, yname = "y")
x = df
