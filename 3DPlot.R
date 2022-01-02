library(rgl)
library(tidyverse)
library("htmltools", "rglwidget")

df <- read.csv("./Data/wave4.weekly.csv") |>
  group_by(Id, Name, Kalenderwoche) |>
  summarize(Inzidenz = mean(Inzidenz)) |>
  append_landkreis_geometry()

df$centroid <-  st_centroid(df)

df <- cbind(df, st_coordinates(df$centroid))

df <- cbind(df, str_split_fixed(df$Kalenderwoche, "/", 2))

myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

df <- df |> mutate(Week = as.integer(X2)) |> filter(Week > 37 & Week <= 45)
cols <- myColorRamp(c("white", "red"), df$Inzidenz)


plot3d(df$X,df$Y, df$`X2`,
       type="s",
       col=cols,
       radius=4500, # 4500
       box=F,
       xlab = "", ylab="", zlab="",
       axes=F, lit=T, specular="black")
show2d({
  par(mar=c(0,0,0,0))
  plot(st_geometry(df$geom), axes=FALSE, asp=0.79)
}, expand=1.175, width=1440, height=1440)
aspect3d(x=0.8,y=1, z=0.1)

rglwidget() # to integrated rstudio plot
