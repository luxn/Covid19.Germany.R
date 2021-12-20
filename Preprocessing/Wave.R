RKICOVID19 <- read_csv(file.choose())
Landkreise <- st_read("./Data/Landkreise.gpkg")
Landkreise <- st_drop_geometry(Landkreise)

#Wave <- c(1, '2020-02-03', '2020-05-31')
Wave <- c(2, '2020-06-01', '2021-02-14')
#Wave <- c(3, '2021-02-15', '2021-08-01')
#Wave <- c(4, '2021-08-02', '2021-11-28')

RKICOVID19$Meldedatum <- as.POSIXct(RKICOVID19$Meldedatum) # Formatieren des Datums von character zu datetimelocal wie in dfmissing, sonst join nicht möglich

ts <- seq(ymd(Wave[2]), ymd(Wave[3]), by="day")
landkreisDF <- RKICOVID19 %>% select(IdLandkreis, Bundesland) %>% distinct(IdLandkreis, Bundesland)

dfMissing = data.frame(Meldedatum=ts) %>%
  mutate(Meldedatum = as.POSIXct(Meldedatum)) %>%
  full_join(landkreisDF, by = character())

wave.daily <- RKICOVID19 %>%
    full_join(dfMissing) %>%
    mutate(AnzahlFall = ifelse(is.na(AnzahlFall), 0, AnzahlFall)) %>%
    mutate(IdLandkreis = ifelse(Bundesland == "Berlin", "11000", IdLandkreis)) %>%
    merge(Landkreise, by.x='IdLandkreis', by.y='Schluesselnummer') %>%
    select(
      Bundesland = Bundesland.x,
      IdLandkreis,
      Name,
      Altersgruppe,
      AnzahlFall,
      Meldedatum,
      Einwohnerzahl) %>%
    filter(Meldedatum >= Wave[2] & Meldedatum <= Wave[3]) %>%
    group_by(Id = IdLandkreis, Name, Bundesland, Meldedatum, Einwohnerzahl) %>%
    summarize(Fallzahl = sum(AnzahlFall)) %>%
    arrange(Id, Meldedatum) %>%
    group_by(Id) %>%
  mutate(RWert7 = (Fallzahl + lag(Fallzahl, 1) + lag(Fallzahl, 2) + lag(Fallzahl, 3) + lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)) /
                  (lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)+ lag(Fallzahl, 7) + lag(Fallzahl, 8) + lag(Fallzahl, 9) + lag(Fallzahl, 10)),
         Inzidenz = ((Fallzahl + lag(Fallzahl, 1) + lag(Fallzahl, 2) + lag(Fallzahl, 3) + lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)) / Einwohnerzahl * 100000)
         )

wave.weekly <- wave.daily %>%
  mutate(Kalenderwoche = sprintf("%s/%s",isoyear(Meldedatum), isoweek(Meldedatum))) %>%
  group_by(Id, Name, Bundesland, Kalenderwoche) %>%
  summarize(Inzidenz = first(sum(Fallzahl) / Einwohnerzahl * 100000), Fallzahl = sum(Fallzahl), RWert7 = mean(RWert7, na.rm = TRUE))


wave.complete <- wave.weekly %>%
  group_by(Id, Name, Bundesland) %>%
  summarize(Inzidenz = mean(Inzidenz),  Fallzahl = sum(Fallzahl), RWert7 = mean(RWert7, na.rm = TRUE))


wave.daily <- wave.daily %>% select(-Einwohnerzahl)

write_csv(wave.daily, sprintf("./Data/wave%s.daily.csv", Wave[1]))
write_csv(wave.weekly, sprintf("./Data/wave%s.weekly.csv", Wave[1]))
write_csv(wave.complete, sprintf("./Data/wave%s.complete.csv", Wave[1]))
