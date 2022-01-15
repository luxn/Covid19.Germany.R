Landkreise <- st_read("./Data/Landkreise.gpkg")

append_landkreis_geometry <- function(df, merge.key = "Id") {
  to.be.merged <- Landkreise %>%
    select(geom, Schluesselnummer)
  return(df %>% merge(to.be.merged, by.x = merge.key, by.y = "Schluesselnummer") %>% st_as_sf)
}

append_bundesland_geometry <- function(df, merge.key = "Bundesland") {
  to.be.merged <- Landkreise %>% select(geom, Bundesland) %>% group_by(Bundesland) %>% summarize(geom = st_union(geom))
  return(df %>% merge(to.be.merged, by.x = merge.key, by.y = "Bundesland") %>% st_as_sf)
}
