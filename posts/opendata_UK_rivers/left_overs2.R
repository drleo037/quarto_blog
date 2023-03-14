I like the leaflet package for interactive maps.

{r label: sample leaflet plots}
library(leaflet)
source(here::here("lib/maps_lib.R"))

wk_ll <- os_or_watercourse_link %>%
  mutate(geom = st_make_valid(geom)) %>%
  filter(str_detect(str_to_lower(watercourseName), "thames")
         |
           str_detect(str_to_lower(watercourseNameAlternative), "thames")
  ) %>%
  sf::st_transform(crs = sf::st_crs(4326))
#  select(length) %>%
#  na.omit()

ggplot(data = wk_ll) +
  geom_sf(aes(colour = length))


uk_rivers <- leaflet() %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addPolylines(data = wk_ll,
               popup = ~makeHTML(paste0('Details:<br>',
                                        'name:', watercourseName, '<br>',
                                        'alt:', watercourseNameAlternative, '<br>',
                                        buildGoogleMapsURLFromStGeom(st_centroid(geom))))
  )

uk_rivers

length(wk_ll)

Introducing sfnetwork…

{r label: introducing sfnetworks}
library(sfnetworks)
if(T) {
  # this takes about 20 seconds
  tictoc::tic()
  sfnet_rivers <- as_sfnetwork(os_or_watercourse_link)
  tictoc::toc()
  sfnet_rivers %>% saveRDS("sfnet_rivers.Rds")
  #  beepr::beep()
} else {
  # https://luukvdmeer.github.io/sfnetworks/articles/sfn01_structure.html
  sfnet_rivers <- sfnetwork(nodes = os_or_hydro_node,
                            edges = os_or_watercourse_link,
                            node_key = "id")
  sfnet_rivers %>% readRDS("sfnet_rivers.Rds")
}


# plot(sfnet_rivers)


# part of the thames thats missing
# https://www.google.co.uk/maps/@51.4923909,0.1937954,14z
# E 552395,  N 179331
if(F) {
  this_df <- "POINT(179331 552395)" %>%
    enframe(value = "geom") %>%
    st_as_sf(wkt = "geom") %>%
    sf::st_transform(crs = sf::st_crs(27700))
  the_centroid <- this_df[,1]$geom
} else {
  the_centroid <- st_sfc(st_point(c(552395, 179331)), crs = 27700)
}

# find a part of the river thames:
sfnet_rivers %>%
  activate("nodes") %>%
  head(10)

sfnet_rivers %>%
  activate("edges") %>%
  head(10)

# Subset neighborhood.
neigh_net1000 <- sfnet_rivers %>%
  activate("edges") %>%
  tidygraph::convert(to_spatial_neighborhood, the_centroid, threshold = 100000, weights = edge_length())


if(F) {
  plot(neigh_net1000, col = watercourseName)
  
  neigh_net1000 %>%
    activate(links) %>%
    st_as_sf() %>%
    mutate(geom = st_make_valid(geom)) %>%
    sf::st_transform(crs = sf::st_crs(4326)) %>%
    #  mutate(geom = st_make_valid(geom)) %>%
    leaflet() %>%
    addTiles(group = "OSM (default)") %>%
    addPolylines(popup = ~makeHTML(paste0('Details:<br>',
                                          'name:', watercourseName, '<br>',
                                          'alt:', watercourseNameAlternative, '<br>',
                                          buildGoogleMapsURLFromStGeom(st_centroid(geom))))
    )  
}


{r label: a graph based view of the river network}
# https://stackoverflow.com/questions/75011724/using-r-package-sfnetworks-to-subset-river-network-that-is-upstream-of-a-given-n
#' visualize directness of network
library(ggraph)
neigh_net1000 %>%#
  ggraph(layout = "tree") +
  geom_node_point(size = 1) +
  geom_edge_link(arrow = arrow(length = unit(2, 'mm'), ends = "first"),
                 end_cap = circle(2, 'mm'),
                 start_cap = circle(2, 'mm')) +
  theme_graph()

and now looking at "communities" (sections of river that are connected somehow to each other.
                                  
                                  {r label: separating the entire rive network into catchments}
                                  # make an igraph to make the associations easier
                                  
                                  # id84B36155-D67B-4C00-A625-9473EB60F828
                                  
                                  sfnet_rivers %>%
                                    activate("edges") %>%
                                    st_as_sf() %>% # names()
                                    as_tibble() %>%
                                    count(id, sort = T)
                                  
                                  rg <- igraph::clusters(sfnet_rivers %>% 
                                                           activate("edges"))
                                  if(F) {
                                    igraph::groups(rg)
                                  }
                                  sfnet_rivers 
                                  # extract the sub-graphs from the full graph (i.e the groupings)
                                  river_grouping <- rg$membership %>% enframe(name = "node_id", value = "RIVER_GROUP_ID")
                                  
                                  sfnet_rivers_grouped <- sfnet_rivers %>%
                                    activate("nodes") %>%
                                    mutate(rg = river_grouping$RIVER_GROUP_ID)
                                  
                                  
                                  
                                  extract the Thames…
                                  
                                  {r label: extracting a specific river}
                                  
                                  target_river <- "yare"
                                  target_river <- "waveney"
                                  target_river <- "thames"
                                  sample_nodeid <- sfnet_rivers_grouped %>%
                                    activate("edges") %>%
                                    filter(str_detect(str_to_lower(watercourseName), target_river)
                                           |
                                             str_detect(str_to_lower(watercourseNameAlternative), target_river)) %>%
                                    as_tibble() %>%
                                    select(to) %>%
                                    as_tibble() %>%
                                    select(-geom) %>%
                                    head(1) %>%
                                    as.integer()
                                  # node 135244 is part of the thames
                                  
                                  # so see which RG this is...
                                  this_rg <- sfnet_rivers_grouped %>%
                                    activate("nodes") %>%
                                    filter(row_number() == sample_nodeid) %>%
                                    as_tibble() %>%
                                    select(rg) %>%
                                    as_tibble() %>%
                                    select(-geom) %>%
                                    head(1) %>%
                                    as.integer()
                                  
                                  
                                  # rg 3544 is the associated group
                                  # so subset the full rivers set to those in the same group
                                  t <- sfnet_rivers %>%
                                    activate("nodes") %>%
                                    mutate(rg = river_grouping$RIVER_GROUP_ID) %>%
                                    filter(rg == this_rg) 
                                  plot(t)
                                  
                                  
                                  
                                  {r label: visualising sfnetworks as sf objects}
                                  basin_rivers_sf <- t %>%
                                    activate("edges") %>%
                                    st_as_sf() %>%
                                    #    select(geom, watercourseName, watercourseNameAlternative) %>%
                                    mutate(geom = st_make_valid(geom)) %>%
                                    sf::st_transform(crs = sf::st_crs(4326))
                                  
                                  basin_leaflet <- basin_rivers_sf %>%
                                    leaflet() %>%
                                    # Base groups
                                    addTiles(group = "OSM (default)") %>%
                                    addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
                                    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
                                    addPolylines(popup = ~makeHTML(paste0('Details:<br>',
                                                                          'form:', form, '<br>',
                                                                          'name:', watercourseName, '<br>',
                                                                          'alt:', watercourseNameAlternative, '<br>',
                                                                          buildGoogleMapsURLFromStGeom(st_centroid(geom)))),
                                                 group = "riverBasin"
                                    ) %>%
                                    addPolylines(data = basin_rivers_sf %>%
                                                   filter(str_detect(str_to_lower(watercourseName), target_river)
                                                          |
                                                            str_detect(str_to_lower(watercourseNameAlternative), target_river)),
                                                 color = "green",
                                                 popup = ~makeHTML(paste0('Details:<br>',
                                                                          'form:', form, '<br>',
                                                                          'name:', watercourseName, '<br>',
                                                                          'alt:', watercourseNameAlternative, '<br>',
                                                                          buildGoogleMapsURLFromStGeom(st_centroid(geom)))),
                                                 group = "river"
                                    )
                                  
                                  basin_leaflet
                                  
                                  {r label: connecting to TW EDM API}
                                  if(F) {
                                    # regiser for an API here: https://data.thameswater.co.uk/s/apis
                                    # then get credentials from: https://data.thameswater.co.uk/s/application-listing
                                    keyring::key_set_with_value(service = "thames_api", 
                                                                username = "client_id",
                                                                password = "REDACTED")
                                    keyring::key_set_with_value(service = "thames_api", 
                                                                username = "client_secret",
                                                                password = "REDACTED")
                                  } else {
                                    client_id <- keyring::key_get(service = "thames_api", "client_id")
                                    client_secret <- keyring::key_get(service = "thames_api", "client_secret")
                                  }
                                  
                                  
                                  {r label: collecting data from TW EDM API}
                                  library(httr)
                                  
                                  # modify this url as desired to access the different end points.
                                  url <- "https://prod-tw-opendata-app.uk-e1.cloudhub.io/data/STE/v1/DischargeCurrentStatus"
                                  
                                  # add here any query parameters if using them e.g. date filters, leave as '' for none.
                                  params <- ""
                                  
                                  # send the request
                                  res <- httr::GET(url = url, 
                                                   add_headers(client_id = client_id, client_secret = client_secret),
                                                   query = params)
                                  
                                  # check status and return only valid requests
                                  if (httr::status_code(res) != 200){
                                    warning(paste("Request failed with status code: ", httr::status_code(res)))
                                  } else {
                                    # parse the response
                                    content <- httr::content(res)
                                    
                                    # APi meta data - ignored
                                    # content$meta
                                    
                                    # data
                                    data <- dplyr::bind_rows(content$items)
                                  }
                                  
                                  edm_df <- data %>%
                                    janitor::clean_names() %>% 
                                    st_as_sf(coords = c("x", "y"), crs = 27700) %>%
                                    rename(geom = geometry) %>%
                                    sf::st_transform(crs = sf::st_crs(4326))
                                  
                                  # you can then manipulate the tibble as you wish
                                  
                                  
                                  {r label: adding TW EDM data to a leaflet plot}
                                  
                                  
                                  getColor <- function(alert_status) {
                                    sapply(alert_status, function(alert_status) {
                                      if(alert_status == "Offline") {
                                        "grey"
                                      } else if(alert_status == "Not discharging") {
                                        "green"
                                      } else if(alert_status == "Discharging") {
                                        "red"
                                      } else {
                                        "yellow"
                                      } })
                                  }
                                  
                                  
                                  offIcon <- awesomeIcons(
                                    icon = 'ios-close',
                                    iconColor = 'black',
                                    library = 'ion',
                                    markerColor = "lightgray"
                                  )
                                  
                                  disIcon <- awesomeIcons(
                                    icon = 'ios-close',
                                    iconColor = 'black',
                                    library = 'ion',
                                    markerColor = "red"
                                  )
                                  
                                  notIcon <- awesomeIcons(
                                    icon = 'ios-close',
                                    iconColor = 'black',
                                    library = 'ion',
                                    markerColor = "darkgreen"
                                  )
                                  
                                  
                                  basin_leaflet %>%
                                    addAwesomeMarkers(data = edm_df %>%
                                                        filter(alert_status == "Offline"),
                                                      clusterOptions = markerClusterOptions(),
                                                      popup = ~makeHTML(paste0('Details:<br>',
                                                                               'name:', location_name, '<br>',
                                                                               'river:', receiving_water_course, '<br>',
                                                                               'alert_status:', alert_status, '<br>',
                                                                               buildGoogleMapsURLFromStGeom(st_centroid(geom)))),
                                                      icon = offIcon,
                                                      clusterId = "Offline EDM",
                                                      group = "Offline EDM"
                                    ) %>%
                                    addAwesomeMarkers(data = edm_df %>%
                                                        filter(alert_status == "Discharging"),
                                                      clusterOptions = markerClusterOptions(),
                                                      popup = ~makeHTML(paste0('Details:<br>',
                                                                               'name:', location_name, '<br>',
                                                                               'river:', receiving_water_course, '<br>',
                                                                               'alert_status:', alert_status, '<br>',
                                                                               buildGoogleMapsURLFromStGeom(st_centroid(geom)))),
                                                      icon = disIcon,
                                                      clusterId = "Discharging EDM",
                                                      group = "Discharging EDM"
                                    ) %>%
                                    addAwesomeMarkers(data = edm_df %>%
                                                        filter(alert_status == "Not discharging"),
                                                      clusterOptions = markerClusterOptions(),
                                                      popup = ~makeHTML(paste0('Details:<br>',
                                                                               'name:', location_name, '<br>',
                                                                               'river:', receiving_water_course, '<br>',
                                                                               'alert_status:', alert_status, '<br>',
                                                                               buildGoogleMapsURLFromStGeom(st_centroid(geom)))),
                                                      icon = notIcon,
                                                      clusterId = "Not discharging EDM",
                                                      group = "Not discharging EDM"
                                    ) %>%
                                    addLayersControl(
                                      baseGroups = c("OSM (default)", "Terrain", "Toner Lite"),
                                      overlayGroups = c("river", "riverBasin", "Discharging EDM", "Not discharging EDM", "Offline EDM"),
                                      options = layersControlOptions(collapsed = FALSE))
                                  
                                  
                                  
                                  
                                  {r label: tracing upstream and downstream}
                                  #' this node is on the lower main stem of the network
                                  node <- 4030 # node near the River Isis part of the thamnes
                                  
                                  node <- t %>%
                                    activate("edges") %>%
                                    filter(str_detect(watercourseNameAlternative, "Isis")) %>%
                                    select(to) %>%
                                    as_tibble() %>%
                                    select(to) %>%
                                    as_tibble() %>%
                                    select(-geom) %>%
                                    head(1) %>%
                                    as.integer()
                                  
                                  node
                                  
                                  
                                  #' query the nodes/vertices with a path to the node (warning issued)
                                  tictoc::tic()
                                  ups <- unique(unlist(igraph::shortest_paths(t, from = node, to = igraph::V(t), mode = "in")[[1]]))
                                  tictoc::toc()
                                  tictoc::tic()
                                  downs <- unique(unlist(igraph::shortest_paths(t, from = node, to = igraph::V(t), mode = "out")[[1]]))
                                  tictoc::toc()
                                  
                                  #' select upstream nodes from spatial network
                                  up.ss <- rep(FALSE, nrow(st_as_sf(t, "nodes")))
                                  up.ss[ups] <- TRUE
                                  down.ss <- rep(FALSE, nrow(st_as_sf(t, "nodes")))
                                  down.ss[downs] <- TRUE
                                  
                                  
                                  if(T) {
                                    n.ss.up <- t |>
                                      activate("nodes") |>
                                      filter(row_number() %in% ups)
                                    
                                    n.ss.down <- t |>
                                      activate("nodes") |>
                                      filter(row_number() %in% downs)
                                  } else {
                                    n.ss.up <- t |>
                                      activate("nodes") |>
                                      filter(up.ss)
                                    n.ss.down <- t |>
                                      activate("nodes") |>
                                      filter(down.ss)
                                  }
                                  
                                  #' plot upstream nodes
                                  cowplot::plot_grid(autoplot(t), autoplot(n.ss.up), autoplot(n.ss.down))
                                  
                                  
                                  
                                  I've downloaded it (sorry, you'll have to follow the link and download it too). We can have a quick look at the contents of a geodatabase without loading it as follows:
  
  