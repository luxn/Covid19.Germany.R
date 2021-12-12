append_landkreis_geometry <- function(df, merge.key = "Id") {
  Landkreise <- st_read("./Data/Landkreise.gpkg") %>% select(geom, Schluesselnummer)
  return(df %>% merge(Landkreise, by.x = merge.key, by.y = "Schluesselnummer") %>% st_as_sf)
}

append_bundesland_geometry <- function(df, merge.key = "Bundesland") {
  Bundeslaender <- st_read("./Data/Landkreise.gpkg") %>% select(geom, Bundesland) %>% group_by(Bundesland) %>% summarize(geom = st_union(geom))
  return(df %>% merge(Bundeslaender, by.x = merge.key, by.y = "Bundesland") %>% st_as_sf)
}
