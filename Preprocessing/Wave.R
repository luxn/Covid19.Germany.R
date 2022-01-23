#RKICOVID19 <- read_csv(file.choose(), col_types = cols(Meldedatum = col_date(format = "%Y/%m/%d %H:%M:%S"),
#                                                       Refdatum = col_date(format = "%Y/%m/%d %H:%M:%S")))
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/66876b81065340a4a48710b062319336/about
RKICOVID19 <- read_csv(file.choose(), col_types = cols(Meldedatum = col_date(format = "%Y-%m-%d"),
                                                       Refdatum = col_date(format = "%Y-%m-%d")))


CalcLandkreise <- st_drop_geometry(st_read("./Data/Landkreise.gpkg"))

Waves <- list(
  c(0, '2020-02-03', '2022-01-02'),
  c(1, '2020-02-03', '2020-05-31'),
  c(2, '2020-06-01', '2021-02-14'),
  c(3, '2021-02-15', '2021-08-01'),
  c(4, '2021-08-02', '2021-11-28')
)

# RKI + Own Definition https://www.rki.de/DE/Content/Infekt/EpidBull/Archiv/2021/Ausgaben/37_21.pdf?__blob=publicationFile
Phases <- list (
  c('.all', '2020-01-27', '2022-01-02'),
  c(0, '2020-01-27', '2020-03-01'),
  c(1, '2020-03-02', '2020-05-17'),
  c(2, '2020-05-18', '2020-09-27'),
  c(3, '2020-09-28', '2021-02-28'),
  c(4, '2021-03-01', '2021-06-06'),
  c(5, '2021-06-07', '2021-08-15'), # own
  c(6, '2021-08-16', '2022-01-02')) # own

ts <- seq(min(RKICOVID19$Meldedatum), max(RKICOVID19$Meldedatum), by="day")
landkreisDF <- RKICOVID19 %>% select(IdLandkreis, Bundesland) %>% distinct(IdLandkreis, Bundesland)

dfMissing = data.frame(Meldedatum=ts) %>%
  full_join(landkreisDF, by = character())

wave.daily <- RKICOVID19 %>%
    full_join(dfMissing) %>%
    mutate(AnzahlFall = ifelse(is.na(AnzahlFall), 0, AnzahlFall)) %>%
    mutate(IdLandkreis = ifelse(Bundesland == "Berlin", "11000", IdLandkreis)) %>%
    merge(CalcLandkreise, by.x='IdLandkreis', by.y='Schluesselnummer') %>%
    select(
      Bundesland = Bundesland.x,
      IdLandkreis,
      Name,
      Altersgruppe,
      AnzahlFall,
      Meldedatum,
      Einwohnerzahl) %>%
    group_by(Id = IdLandkreis, Name, Bundesland, Meldedatum, Einwohnerzahl) %>%
    summarize(Fallzahl = sum(AnzahlFall)) %>%
    arrange(Id, Meldedatum) %>%
    group_by(Id) %>%
  mutate(#RWert7 = (Fallzahl + lag(Fallzahl, 1) + lag(Fallzahl, 2) + lag(Fallzahl, 3) + lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)) /
         #         (lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)+ lag(Fallzahl, 7) + lag(Fallzahl, 8) + lag(Fallzahl, 9) + lag(Fallzahl, 10)),
         Inzidenz = ((Fallzahl + lag(Fallzahl, 1) + lag(Fallzahl, 2) + lag(Fallzahl, 3) + lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)) / Einwohnerzahl * 100000),
         Fallzahl7 = (Fallzahl + lag(Fallzahl, 1) + lag(Fallzahl, 2) + lag(Fallzahl, 3) + lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6))
         )

wave.weekly <- wave.daily %>%
  mutate(Kalenderwoche = floor_date(Meldedatum, unit="week", week_start = 1)) %>%
  group_by(Id, Name, Bundesland, Kalenderwoche) %>%
  summarize(Inzidenz = mean(Inzidenz),
            Fallzahl = sum(Fallzahl))
            #RWert7 = mean(RWert7, na.rm = TRUE))

wave.weekly[is.na(wave.weekly)] <- 0 # currently



wave.daily <- wave.daily %>% select(-Einwohnerzahl)

for (Wave in Phases) {
  daily <- filter(wave.daily, Meldedatum >= Wave[2] & Meldedatum <= Wave[3])
  weekly <- filter(wave.weekly, Kalenderwoche >= Wave[2] & Kalenderwoche <= Wave[3])
  complete <- weekly %>%
    group_by(Id, Name, Bundesland) %>%
    summarize(
      Inzidenz = mean(Inzidenz),
      Fallzahl = sum(Fallzahl),
      #RWert7 = mean(RWert7, na.rm = TRUE)
    )
  write_csv(daily, sprintf("./Data/phase%s.daily.csv", Wave[1]))
  write_csv(weekly, sprintf("./Data/phase%s.weekly.csv", Wave[1]))
  write_csv(complete, sprintf("./Data/phase%s.complete.csv", Wave[1]))
}

for (Wave in Waves) {
  daily <- filter(wave.daily, Meldedatum >= Wave[2] & Meldedatum <= Wave[3])
  weekly <- filter(wave.weekly, Kalenderwoche >= Wave[2] & Kalenderwoche <= Wave[3])
  complete <- weekly %>%
    group_by(Id, Name, Bundesland) %>%
    summarize(
      Inzidenz = mean(Inzidenz),
      Fallzahl = sum(Fallzahl),
      #RWert7 = mean(RWert7, na.rm = TRUE)
    )
  write_csv(daily, sprintf("./Data/wave%s.daily.csv", Wave[1]))
  write_csv(weekly, sprintf("./Data/wave%s.weekly.csv", Wave[1]))
  write_csv(complete, sprintf("./Data/wave%s.complete.csv", Wave[1]))
}

