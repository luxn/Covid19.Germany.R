RKICOVID19 <- read_csv(file.choose())
Landkreise <- st_read("./Data/Landkreise.gpkg")
Landkreise <- st_drop_geometry(Landkreise)

#Wave <- c(1, '2020-02-03', '2020-05-31')
#Wave <- c(2, '2020-06-01', '2021-02-14')
#Wave <- c(3, '2021-02-15', '2021-08-01')
Wave <- c(4, '2021-08-02', '2021-11-28')

wave.daily <- RKICOVID19 %>%
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
    mutate(Einwohnerzahl = as.integer(Einwohnerzahl)) %>%
    group_by(Id = IdLandkreis, Name, Bundesland, Meldedatum, Einwohnerzahl) %>%
    summarize(Fallzahl = sum(AnzahlFall)) %>%
    group_by(Id) %>% mutate(RWert7 =
                            (Fallzahl + lag(Fallzahl, 1) + lag(Fallzahl, 2) + lag(Fallzahl, 3) + lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)) /
                            (lag(Fallzahl, 4) + lag(Fallzahl, 5) + lag(Fallzahl, 6)+ lag(Fallzahl, 7) + lag(Fallzahl, 8) + lag(Fallzahl, 9) + lag(Fallzahl, 10)))

wave.weekly <- wave.daily %>%
  mutate(Kalenderwoche = isoweek(Meldedatum)) %>%
  group_by(Id, Name, Bundesland, Kalenderwoche) %>%
  summarize(Inzidenz = first(sum(Fallzahl) / Einwohnerzahl * 100000), Fallzahl = sum(Fallzahl), RWert7 = mean(RWert7, na.rm = TRUE))

wave.daily <- wave.daily %>% select(-Einwohnerzahl)

st_write(wave.daily, dsn="./wave.daily.csv", delete_dsn=T)
st_write(wave.weekly, dsn="./wave.weekly.csv", delete_dsn=T)
