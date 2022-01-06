hotspot.lisa <- function(df, wavetype = "complete", wavefilter = FALSE) {
  if (wavetype == "weekly") {
    wave <- df %>% filter(Kalenderwoche == wavefilter) # KW: '2021-09-27'
  } else if (wavetype == "daily") {
    wave <- df %>% filter(Meldedatum == wavefilter) # Datum: '2021-10-22'
  } else {
    wave <- df
  }
  wave <- wave %>% append_landkreis_geometry

  neighbours <- poly2nb(wave, queen = TRUE)
  local <- localmoran(x = wave$Inzidenz, listw = nb2listw(neighbours, style = "W"))
  quadrant <- vector(mode="numeric",length=nrow(local))

  # centers the variable of interest around its mean
  m.inzidenz <- wave$Inzidenz - mean(wave$Inzidenz)

  # centers the local Moran's around the mean
  m.local <- local[,1] - mean(local[,1])

  # significance threshold
  signif <- 0.1

  # builds a data quadrant
  quadrant[m.inzidenz >0 & m.local>0] <- 4
  quadrant[m.inzidenz <0 & m.local<0] <- 1
  quadrant[m.inzidenz <0 & m.local>0] <- 2
  quadrant[m.inzidenz >0 & m.local<0] <- 3
  quadrant[local[,5]>signif] <- 0

  # plot in r
  brks <- c(0,1,2,3,4)
  colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")

  plot(as_Spatial(wave) ,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
  title(sprintf("hotspot.lisa: %s %s", wavetype, wavefilter))
  box()
  legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),fill=colors,bty="n")

}







# htwave <- read_csv("./wave.weekly.csv")
# kw <- 35
#
# wave <- wave %>% filter(Kalenderwoche == kw) %>% append_landkreis_geometry
#
# neighbours <- poly2nb(wave, queen = TRUE)
# local <- localmoran(x = wave$Inzidenz, listw = nb2listw(neighbours, style = "W"))
#
# moran.map <- cbind(wave, local)
# tm_shape(moran.map) + tm_fill(col = "Ii",
#                               style = "quantile",
#                               title = "local moran statistic")
#
# quadrant <- vector(mode="numeric",length=nrow(local))
#
# # centers the variable of interest around its mean
# m.inzidenz <- wave$Inzidenz - mean(wave$Inzidenz)
#
# # centers the local Moran's around the mean
# m.local <- local[,1] - mean(local[,1])
#
# # significance threshold
# signif <- 0.1
#
# # builds a data quadrant
# quadrant[m.inzidenz >0 & m.local>0] <- 4
# quadrant[m.inzidenz <0 & m.local<0] <- 1
# quadrant[m.inzidenz <0 & m.local>0] <- 2
# quadrant[m.inzidenz >0 & m.local<0] <- 3
# quadrant[local[,5]>signif] <- 0
#
# # plot in r
# brks <- c(0,1,2,3,4)
# colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
#
# plot(as_Spatial(wave) ,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
# box()
# legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),fill=colors,bty="n")
