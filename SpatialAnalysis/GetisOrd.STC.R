source("./Preprocessing/AppendGeometry.R")
df <- read_csv("./data/phase6.weekly.csv")

#cube.df <- df %>% group_by(Id, Name) %>% summarize(Id = first(Id), Name=first(Name)) %>% arrange(Id)

df <- append_landkreis_geometry(df)

result.df <- data.frame(
  Id = numeric(),
  Name = character(),
  Kalenderwoche = as.Date(character()),
  gstat = numeric(),
  class = character()

)

weeks <- unique(date(df$Kalenderwoche))
for (week in weeks) {
  week <- as.Date(week, origin=origin)
  week.df <- filter(df, Kalenderwoche == week) %>% arrange(Id)
  NB <- poly2nb(week.df)
  nb_lw <- nb2listw(include.self(NB), style = 'W')
  local_g <- localG(week.df$Inzidenz, nb_lw)

  column <- cut(as.matrix(local_g), c(-Inf, -2.576, -1.960, -1.645, 1.645, 1.960, 2.576, +Inf),
                labels=c(">99% ColdSpot", ">95% ColdSpot", ">90% ColdSpot", "Uncertain", ">90% HotSpot", ">95% HotSpot", ">99% HotSpot"))

  week.df <- cbind(week.df, column)
  week.df <- rename(week.df, class = column)

  week.df <- cbind(week.df, as.matrix(local_g))
  week.df <- rename(week.df, gstat = as.matrix.local_g.)

  result.df <- rbind(result.df, select(week.df, Id, Name, Kalenderwoche, gstat, class))



  #ggsave(sprintf("./Images/lisa.getisord.phase6.%s.png", week), p, dpi="retina", width=7.5, height=6)

  #cube.df <- cbind(cube.df, as.matrix(local_g))
  #names(cube.df)[length(names(cube.df))] <- as.character(week)
}

week_labeller <- function(variable,value) {
  return(sprintf("%s/2021", week(value)))
}

p <- ggplot(result.df) +
  geom_sf(aes(fill=class),
          lwd =0.25) +
  scale_fill_manual(values = rev(c('#b2182b','#ef8a62','#fddbc7','#f7f7f7','#d1e5f0','#67a9cf','#2166ac')), name= "Gi* values",) +
  facet_wrap(vars(Kalenderwoche), ncol = 4, labeller=week_labeller) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
  #ggtitle(sprintf("Gi* - Phase 6 - Week of %s", week))

ggsave("./Images/plot3.pdf", p, dpi="retina", width=9, height=11)


result.df$centroid <- st_centroid(result.df)
result.df <- cbind(result.df, st_coordinates(result.df$centroid))

myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

cols <- myColorRamp(c("blue", "white", "red"), result.df$gstat)
#df$Week <- isoweek(df$Kalenderwoche)

plot3d(result.df$X, result.df$Y, result.df$Kalenderwoche,
       type="s",
       col=cut(result.df$gstat, c(-Inf, -2.576, -1.960, -1.645, 1.645, 1.960, 2.576, +Inf),
               labels=rev(c('#b2182b','#ef8a62','#fddbc7','#f7f7f7','#d1e5f0','#67a9cf','#2166ac'))),
       radius=4500,
       box=T,
       xlab = "", ylab="", zlab="",
       axes=F, lit=T, specular="black")
show2d({
  par(mar=c(0,0,0,0))
  plot(st_geometry(result.df$geom), axes=FALSE, asp=0.79)
}, expand=1.175, width=1440, height=1440)
aspect3d(x=0.8,y=1, z=0.15)

rglwidget() # to integrated rstudio plot

