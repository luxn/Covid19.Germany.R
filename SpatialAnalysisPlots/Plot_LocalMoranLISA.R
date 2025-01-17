source("./libraries.R")
source("./Preprocessing/AppendGeometry.R")

# Load Data
dfs <- list(
  read_csv("./Data/phase0.complete.csv"),
  read_csv("./Data/phase1.complete.csv"),
  read_csv("./Data/phase2.complete.csv"),
  read_csv("./Data/phase3.complete.csv"),
  read_csv("./Data/phase4.complete.csv"),
  read_csv("./Data/phase5.complete.csv"),
  read_csv("./Data/phase6.complete.csv")
)

# own colours
lisa_colors = c("#eeeeee", "#d7191c", "#0868ac", "#a7adf9", "#f4ada8", "#464646", "#999999")
desc = c("sporadische Fälle", "1. Welle", "Sommerplateau 2020", "2. Welle", "3. Welle", "Sommerplateau 2021", "4. Welle")
lisa_factors <- c(0,1,2,3,4)

i <- 0
plots.list <- list()

for (dframe in dfs) {
  local_i <- i
  df <- append_landkreis_geometry(dframe)
  NB <- poly2nb(df)
  W <- nb2listw(NB, style="W")

  # scatter plot
  mp <- moran.plot(as.vector(scale(df$Inzidenz)), W, labels=df$Name)

  #global moran's I
  mr <- moran.test(df$Inzidenz, W)


  mp$displayLabel <- if_else(mp$is_inf, mp$labels, "")
  plot.scatter <- ggplot(mp, aes(x=x, y=wx, label=displayLabel)) + geom_point(shape=1) +
    geom_smooth(formula=y ~ x, method="lm", se=F) +
    geom_hline(yintercept=mean(mp$wx), lty=2) +
    geom_vline(xintercept=mean(mp$x), lty=2) + #theme_minimal() +
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    xlab("Inzidenz") + ylab("Spatially lagged Inzidenz") +
    geom_text_repel(size = 3) + ggtitle(sprintf("Phase %s  | %s" , i, desc[i+1]), sprintf("Moran's I: %f (p < 0.05)", mr$estimate[1]))

  ggsave(sprintf("./Images/moranscatter.phase%s.png", i), plot.scatter, dpi="retina", width=5, height=5)

  W <- queen_weights(df)

  # Local Moran's I
  lisa <- local_moran(W, df['Inzidenz'])

  #lisa_colors <- lisa_colors(lisa)
  lisa_labels <- lisa_labels(lisa)
  lisa_clusters <- lisa_clusters(lisa)
  lisa_clusters <- as.factor(lisa_clusters)
  levels(lisa_clusters) <- lisa_factors
  lisa_pvalues <- lisa_pvalues(lisa)

  plot.lisa <- ggplot(df) +
    geom_sf(aes(fill=lisa_clusters), lwd =0.25) +
    scale_fill_manual(values = lisa_colors, name= "LISA Cluster", labels=lisa_labels, drop=FALSE) +
    ggtitle("LISA Cluster", sprintf("Phase %s  | %s | Moran's I: %f (p < 0.05)" , i, desc[i+1], mr$estimate[1])) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())

  plot.pvalue <- ggplot(df) +
    geom_sf(aes(fill=cut(lisa_pvalues, c(0, 0.001, 0.01, 0.05, 1), labels=c("< 0.001", "< 0.01", "< 0.05", "Not significant"))), lwd =0.25) +
    scale_fill_manual(values = c('#006d2c', '#31a354', '#bae4b3', "#eeeeee"), name= "p-value") +
    ggtitle("LISA p-values") +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())

  ggsave(sprintf("./Images/lisa.localmoran.phase%s.cluster.png", i), plot.lisa, dpi="retina", width=7.5, height=6)
  ggsave(sprintf("./Images/lisa.localmoran.phase%s.pvalues.png", i), plot.pvalue, dpi="retina", width=7.5, height=6)
  #print(plot.scatter + plot.lisa + plot.pvalue + plot_layout(widths = c(1, 1, 1)))

  i <- i + 1
}

# Multiplot bauen
multiplot1 <- plots.list[[1]] /
  plots.list[[2]] /
  plots.list[[3]] /
  plots.list[[4]]

multiplot2 <- plots.list[[5]] /
  plots.list[[6]] /
  plots.list[[7]]

# Speichern
ggsave("./Images/lisa01.pdf", multiplot1, dpi="retina", width=12, height = 14)
ggsave("./Images/lisa02.pdf", multiplot2, dpi="retina", width=12, height = 11)
