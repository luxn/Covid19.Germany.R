dfs <- list(
  read_csv("./data/phase0.complete.csv"),
  read_csv("./data/phase1.complete.csv"),
  read_csv("./data/phase2.complete.csv"),
  read_csv("./data/phase3.complete.csv"),
  read_csv("./data/phase4.complete.csv"),
  read_csv("./data/phase5.complete.csv"),
  read_csv("./data/phase6.complete.csv")
)

lisa_colors = c("#eeeeee", "#d7191c", "#0868ac", "#a7adf9", "#f4ada8", "#464646", "#999999")
#'#b2182b','#ef8a62','#fddbc7','#f7f7f7','#d1e5f0','#67a9cf','#2166ac'

i <- 0
for (dframe in dfs) {
  df <- append_landkreis_geometry(dframe)

  W <- queen_weights(df)
  lisa <- local_moran(W, df['Inzidenz'])

  #lisa_colors <- lisa_colors(lisa)
  lisa_labels <- lisa_labels(lisa)
  lisa_clusters <- lisa_clusters(lisa)
  lisa_pvalues <- lisa_pvalues(lisa)

  ggplot(df) +
    geom_sf(aes(fill=factor(lisa_clusters)), lwd =0.25) +
    scale_fill_manual(values = lisa_colors, name= "LISA Cluster", labels=lisa_labels, drop=F, guide="none") +
    theme_minimal() +
    ggtitle(sprintf("Local Moran LISA Cluster - Phase %s | p <0.05", i)) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())

  p2 <- ggplot(df) +
    geom_sf(aes(fill=cut(lisa_pvalues, c(0, 0.001, 0.01, 0.05, 1), labels=c("less than 0.001", "less than 0.01", "less than 0.05", "not significant"))), lwd =0.25) +
    scale_fill_manual(values = c('#006d2c', '#31a354', '#bae4b3', "grey"), name= "LISA pvalues",) +
    ggtitle(sprintf("Local Moran LISA pvalues - Phase %s", i))

  ggsave(sprintf("./Images/lisa.localmoran.phase%s.cluster.png", i), p, dpi="retina", width=7.5, height=6)
  ggsave(sprintf("./Images/lisa.localmoran.phase%s.pvalues.png", i), p2, dpi="retina", width=7.5, height=6)
  i <- i + 1
}
