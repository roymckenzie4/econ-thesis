
sids <- c(1:10)

data <- data.frame(SID = sids)

data <- data %>%
  mutate(
    h_0 = runif(10, 0, 10),
    TID = c(rep(1, 5), rep(2, 5)),
    TID_h_effect = 1,
    TID_g_effect = c(rep(1,5), rep(0, 5)),
    g_fresh = h_0 + TID_h_effect + TID_g_effect + rnorm(10, 0, 1)
  )
