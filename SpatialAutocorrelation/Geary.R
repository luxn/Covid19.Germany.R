geary.c <- function(df, wavetype, wavefilter, bundesland=F) {
  if (wavetype == "weekly") {
    wave <- df %>% filter(Kalenderwoche == wavefilter) # KW: 2021/43
  } else if (wavetype == "daily") {
    from <- as.POSIXct(wavefilter)
    to <- from + days(1)
    wave <- df %>% filter(Meldedatum >= from & Meldedatum < to) # Datum: '2021-10-22'
  } else {
    wave <- df
  }
  if (bundesland) {
    wave <- wave %>% append_bundesland_geometry

  } else {
    wave <- wave %>% append_landkreis_geometry
  }

  nb <- poly2nb(wave, queen=T)
  lw <- nb2listw(nb, style="W")

  # Geary's C measure of spatial autocorrelation
  geary.test(wave$Inzidenz, listw = lw)
}
