globalmoran.plot <- function(df, wavetype, wavefilter, bundesland=F) {
  if (wavetype == "weekly") {
    wave <- df %>% filter(Kalenderwoche == wavefilter) # KW: 2021/43
  } else if (wavetype == "daily") {
    wave <- df %>% filter(Meldedatum == wavefilter) # Datum: '2021-10-22'
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

  t <- moran.test(wave$Inzidenz, lw, alternative="greater")

  if (bundesland) {
    res <- moran.plot(wave$Inzidenz, labels = wave$Bundesland, listw = lw)
  } else {
    res <- moran.plot(wave$Inzidenz, labels = wave$Name, listw = lw)
  }

  title(sprintf("Global Moran I: %.3f | pValue: %.10f", t$estimate[1], t$p.value))
}

# https://mgimond.github.io/simple_moransI_example/#another_example:_florida_1980_homicide_rate_example

#
# glowave4 <- read_csv("./wave.weekly.csv") %>% group_by(Bundesland) %>% summarize()
#
# df <- data.frame(KW=integer(), Moran=double(), pValue = double())
#
# wave4 <- append_bundesland_geometry(wave4)
# #op <- par(mfrow = c(4, 5), pty = "s")
# for (kw in min(wave4$Kalenderwoche):(max(wave4$Kalenderwoche))) {
#   wave <- wv %>% filter(Kalenderwoche == 17)
#
#   nb <- poly2nb(wave, queen=F)
#   print(nb)
#   lw <- nb2listw(nb, style="W", zero.policy=TRUE)
#
#   inzidenz.lag <- lag.listw(lw, wave$Inzidenz)
#   M1 <- lm(inzidenz.lag ~ wave$Inzidenz)
#   #I <- moran(wave$Inzidenz, lw, length(nb), Szero(lw))[1]
#   #plot(inzidenz.lag ~ wave$Inzidenz,
#   #     main = sprintf("KW %s", kw),
#   #     sub = sprintf("Moran's I: %f", coef(M1)[2]),
#   #     xlab = "Inzidenz", ylab = "Inzidenz Lag",
#   #     cex = 0.2
#   #)
#   #abline(M1, col="red")
#
#   # moran is also -> coef(M1)[2]
#
#   t <- moran.test(wave$Inzidenz, lw, alternative="greater")
#   #print(t)
#   df <- df %>% add_row(KW = kw, Moran = coef(M1)[2])#, pValue = t$p.value)
# }
#
# #par(mfrow = c(1, 1))
# #plot(df$KW, df$Moran, main = "Moran's I", ylim = c(0, 1),  xlab = "Kalenderwoche", ylab="Moran's I Value")
#
#
# moran <- moran.plot(wave$Inzidenz, labels = wave$Name, listw = nb2listw(nb, style="W"))
# t <- moran.test(wave$Inzidenz, nb2listw(nb, style="W"), alternative="greater")
# t$estimate[1]
