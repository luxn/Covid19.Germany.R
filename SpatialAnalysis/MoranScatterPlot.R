dfs <- list(
  read_csv("./data/phase0.complete.csv"),
  read_csv("./data/phase1.complete.csv"),
  read_csv("./data/phase2.complete.csv"),
  read_csv("./data/phase3.complete.csv"),
  read_csv("./data/phase4.complete.csv"),
  read_csv("./data/phase5.complete.csv"),
  read_csv("./data/phase6.complete.csv")
)
i <- 0
for (dframe in dfs) {
  df <- append_landkreis_geometry(dframe)
  NB <- poly2nb(df)
  W <- nb2listw(NB, style="W")
  mp <- moran.plot(df$Inzidenz, W, labels=df$Name)
  mr <- moran.test(df$Inzidenz, W)
  mp$displayLabel <- if_else(mp$is_inf, mp$labels, "")
  p <- ggplot(mp, aes(x=x, y=wx, label=displayLabel)) + geom_point(shape=1) +
    geom_smooth(formula=y ~ x, method="lm", se=F) +
    geom_hline(yintercept=mean(mp$wx), lty=2) +
    geom_vline(xintercept=mean(mp$x), lty=2) + #theme_minimal() +
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    xlab("Inzidenz") + ylab("Spatially lagged Inzidenz") +
    geom_text_repel(size = 3) + ggtitle(sprintf("Phase %s | Moran's I: %f p-Value: %f", i, mr$estimate[1], mr$p.value))

  ggsave(sprintf("./Images/moranscatter.phase%s.png", i), p, dpi="retina", width=7, height=7)
  i <- i + 1
}

