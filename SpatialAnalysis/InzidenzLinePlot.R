germany.df <- read_csv("./data/phase.all.daily.csv") %>%
  group_by(Meldedatum) %>%
  summarise(Inzidenz = mean(Inzidenz)) %>%
  arrange(Meldedatum)

Phases <- list (
  c('.all', '2020-01-27', '2022-01-02'),
  c(0, '2020-01-27', '2020-03-01'),
  c(1, '2020-03-02', '2020-05-17'),
  c(2, '2020-05-18', '2020-09-27'),
  c(3, '2020-09-28', '2021-02-28'),
  c(4, '2021-03-01', '2021-06-06'),
  c(5, '2021-06-07', '2021-08-15'), # own
  c(6, '2021-08-16', '2022-01-02')) # own

plot.inz <- ggplot(germany.df, aes(x=Meldedatum, y=Inzidenz)) +
  geom_spline(size=1.25) +
  scale_x_date("", date_breaks = "months" , date_labels = "%b-%y") +
  scale_y_continuous("7-Tage-Inzidenz") +
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  annotate("text", x = date('2020-01-27') + 0, y = 500, label = "Phase 0") +
  annotate("text", x = date('2020-03-02') + 25, y = 500, label = "Phase 1") +
  annotate("text", x = date('2020-05-18') + 25, y = 500, label = "Phase 2") +
  annotate("text", x = date('2020-09-28') + 25, y = 500, label = "Phase 3") +
  annotate("text", x = date('2021-03-01') + 25, y = 500, label = "Phase 4") +
  annotate("text", x = date('2021-06-07') + 25, y = 500, label = "Phase 5") +
  annotate("text", x = date('2021-08-16') + 25, y = 500, label = "Phase 6") +
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
  ggtitle("Verlauf der 7-Tage Inzidenz pro 100.000 Einwohner auf Bundesebene")

ggsave("./Images/inzidenzverlauf.phasen.png", plot.inz, dpi="retina", width=9, height=3)

