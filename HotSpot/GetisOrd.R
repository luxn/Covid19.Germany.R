hotspot.gstat <- function(df, wavetype, wavefilter) {
  if (wavetype == "weekly") {
    wave <- df %>% filter(Kalenderwoche == wavefilter) # KW: 43
  } else if (wavetype == "daily") {
    wave <- df %>% filter(Meldedatum == wavefilter) # Datum: '2021-10-22'
  } else {
    wave <- df
  }
  wave <- wave %>% append_landkreis_geometry

  nb <- dnearneigh(st_centroid(wave), 0, 60000) # 60km
  nb_lw <- nb2listw(nb, style = 'W')
  local_g <- localG(wave$Inzidenz, nb_lw)
  local_g <- cbind(wave, as.matrix(local_g))
  local_g <- rename(local_g, gstat = as.matrix.local_g.)

  pal <-  c("blue",rgb(0,0,1,alpha=0.4),"white", rgb(1,0,0,alpha=0.4),"red")

  tm_shape(local_g) + tm_fill("gstat", n=5,
                              palette = pal,
                              style = "fixed", breaks = c(-Inf, -2, -1, 1, 2, +Inf)) + tm_borders(alpha=.4)
}


wv <- read_csv("./wave.weekly.csv")
kw <- 16
wave <- wave %>% filter(Kalenderwoche == kw) %>% append_landkreis_geometry()

nb <- dnearneigh(st_centroid(wave), 0, 60000) # 60km

nb_lw <- nb2listw(nb, style = 'W')

local_g <- localG(wave$Inzidenz, nb_lw)
local_g <- cbind(wave, as.matrix(local_g))
local_g <- mutate(local_g, gstat = as.matrix(local_g))

pal <-  c("blue",rgb(0,0,1,alpha=0.4),"white", rgb(1,0,0,alpha=0.4),"red")

tm_shape(local_g) + tm_fill("gstat", n=5,
                            palette = pal,
                            style = "fixed", breaks = c(-Inf, -2, -1, 1, 2, +Inf)) + tm_borders(alpha=.4)

