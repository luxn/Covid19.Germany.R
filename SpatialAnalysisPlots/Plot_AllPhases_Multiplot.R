source("./libraries.R")
source("./Preprocessing/AppendGeometry.R")

# 7-Tage-Inzidenz Plot
source("./SpatialAnalysisPlots/InzidenzLinePlot.R")

# Moran's I Plot
source("./SpatialAnalysisPlots/MoranPlot.R")

# Tile Plot - Bundesländer
source("./SpatialAnalysisPlots/TilePlotBundesland.R")

# MULTIPLOT
plot1 <- plot.inz / plot.moran / plot.bundesland + plot_annotation(tag_levels = 'A') + theme(plot.tag = element_text(size = 16))
ggsave("./Images/plot1_multiplot.pdf", plot1, dpi="retina", width=13, height=13)
