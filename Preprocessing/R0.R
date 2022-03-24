# Reference https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/R-Wert-Erlaeuterung.pdf?__blob=publicationFile

# Untersuchung mit R0 etwas anzufangen

wave4 <- read_csv("./wave4daily.csv")
waveR0 <- wave4 %>% head(119)

data <- waveR0

# R-Wert Berechnung bei einem seriellen Intervall von 7 Tagen
R_Wert <- rep(NA, nrow(data))
for (t in 11:nrow(data)) {
  R_Wert[t] <- sum(data$Fallzahl[t-0:6]) / sum(data$Fallzahl[t-4:10])
}
data <- data %>% mutate(R_Wert7 = round(R_Wert, digits = 2))


waveR0 <- wave4 %>% group_by(Id) %>% mutate(RWert7 =
                              (Fallzahl + lag(Fallzahl, 1) + lag(Fallzahl, 2) + lag(Fallzahl, 3) + lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)) /
                              (lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)+ lag(Fallzahl, 7) + lag(Fallzahl, 8) + lag(Fallzahl, 9) + lag(Fallzahl, 10)))

plot(as.POSIXct(data$Meldedatum), data$R_Wert7, ylim=c(0, 2.5))
lines(spline(as.POSIXct(data$Meldedatum), data$R_Wert7, n= 10))



waveR0 <- read_csv("./Data/wave4.daily.csv")
waveR0 <- waveR0 |>
  group_by(Meldedatum) |>
  summarize(Fallzahl = sum(Fallzahl), Inzidenz = mean(Inzidenz)) |>
  filter(!is.na(Inzidenz))

#si aus https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7448781/
res <- estimate_R(incid = waveR0$Inzidenz, method="parametric_si",
                  config = make_config(list(
                    mean_si = 5.19,
                    std_si = 0.82)))

plot(res)
