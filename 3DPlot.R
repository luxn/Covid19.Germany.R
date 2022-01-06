library(rgl)
library(tidyverse)
library("htmltools", "rglwidget")

df <- read_csv("./Data/wave4.weekly.csv") |>
  group_by(Id, Name, Kalenderwoche) |>
  summarize(Inzidenz = mean(Inzidenz)) |>
  append_landkreis_geometry()

df$centroid <-  st_centroid(df)

df <- cbind(df, st_coordinates(df$centroid))

myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

df <- df |> filter(Kalenderwoche > '2021-11-01' & Kalenderwoche <= '2021-11-29')
cols <- myColorRamp(c("white", "red"), df$Inzidenz)
df$Week <- isoweek(df$Kalenderwoche)

plot3d(df$X,df$Y, df$Week,
       type="s",
       col=cols,
       radius=4500,
       box=F,
       xlab = "", ylab="", zlab="",
       axes=F, lit=T, specular="black")
show2d({
  par(mar=c(0,0,0,0))
  plot(st_geometry(df$geom), axes=FALSE, asp=0.79)
}, expand=1.175, width=1440, height=1440)
aspect3d(x=0.8,y=1, z=0.05)

rglwidget() # to integrated rstudio plot
