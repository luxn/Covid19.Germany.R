df <- read_csv("./Data/phase.all.daily.csv")

ts <- seq(min(df$Meldedatum), max(df$Meldedatum), by="day")


result.df <- data.frame(
  Date = as.Date(character()),
  MoranI = numeric(),
  MoranPValue = numeric(),
  MoranMCPValue = numeric()
)

for (t in ts) {
  local.df <- filter(df, Meldedatum == t) %>% append_landkreis_geometry()
  NB <- poly2nb(local.df)
  W <- nb2listw(NB, style="W")
  mr <- moran.test(local.df$Inzidenz, W)
  mc <- moran.mc(local.df$Inzidenz, listw=W, nsim=4999)
  result.df <- add_row(result.df, Date = as.Date(t, origin=origin), MoranI=mr$estimate[1], MoranPValue = mr$p.value, MoranMCPValue = mc$p.value)
}

plot.moran <- ggplot(result.df, aes(x=Date, y=MoranI)) +
  geom_spline(size=1.25) +
  scale_x_date("", date_breaks = "months" , date_labels = "%b-%y") +
  scale_y_continuous("Moran's I", limits = c(0, 1)) +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  annotate("text", x = date('2020-01-27') + 0, y = 1, label = "Phase 0") +
  annotate("text", x = date('2020-03-02') + 25, y = 1, label = "Phase 1") +
  annotate("text", x = date('2020-05-18') + 25, y = 1, label = "Phase 2") +
  annotate("text", x = date('2020-09-28') + 25, y = 1, label = "Phase 3") +
  annotate("text", x = date('2021-03-01') + 25, y = 1, label = "Phase 4") +
  annotate("text", x = date('2021-06-07') + 25, y = 1, label = "Phase 5") +
  annotate("text", x = date('2021-08-16') + 25, y = 1, label = "Phase 6") +
  annotate("rect", xmin = c(
    date('2020-01-27'),
    date('2020-03-02'),
    date('2020-05-18'),
    date('2020-09-28'),
    date('2021-03-01'),
    date('2021-06-07'),
    date('2021-08-16')),
    xmax = c(
      date('2020-03-01'),
      date('2020-05-17'),
      date('2020-09-27'),
      date('2021-02-28'),
      date('2021-06-06'),
      date('2021-08-15'),
      date('2022-01-02')),
    ymin = -Inf, ymax = Inf, alpha = .1, fill = c(
      "green",
      "red",
      "green",
      "red",
      "green",
      "red",
      "green")) +
  ggtitle("Verlauf des Moran's I auf Bundesebene")


ggsave("./Images/moranverlauf.phasen.png", plot.moran, dpi="retina", width=9, height=3)
