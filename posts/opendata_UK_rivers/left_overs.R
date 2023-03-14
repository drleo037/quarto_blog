# get some stats on the stage levels for the EA stations -----
if(T) {
  get_ea_stage_summary <- function(url) {
    message("collecting on: ", url)
    res <- httr::GET(url = url)
    
    content <- httr::content(res)
    
    # there's a few stats on a few measurements inside the content
    # for this post I'm only interested in "stage" (not flow or downstreamStage etc.)
    
    content$items$stageScale$typicalRangeHigh
    if(!is.null(content$items$stageScale$highestRecent)) {
      part1 <- dplyr::bind_rows(content$items$stageScale) %>%
        select(-c(highestRecent, maxOnRecord, minOnRecord)) %>% #  we'll hald thise in part 2
        janitor::clean_names() %>%
        select(-id) %>%
        distinct()
      
      part2 <- dplyr::bind_rows(content$items$stageScale$highestRecent) %>% mutate(stat = 'highestRecent') %>%
        dplyr::bind_rows(dplyr::bind_rows(content$items$stageScale$maxOnRecord) %>% mutate(stat = 'maxOnRecord')) %>%
        dplyr::bind_rows(dplyr::bind_rows(content$items$stageScale$minOnRecord) %>% mutate(stat = 'minOnRecord')) %>%
        janitor::clean_names() %>%
        select(-id) %>%
        pivot_wider(names_from = stat, values_from = c(date_time, value)) 
      
      
      result = part1 %>% bind_cols(part2) %>%
        distinct()
      
    } else {
      result = NA      
    }
    return(result)
  }
  
  ea_rage_cachefile <- "C:/Users/leoki/DATA/EA/ea_historic_ranges.Rds"
  if(file.exists(ea_rage_cachefile)) {
    ea_historic_ranges <- readRDS(file = ea_rage_cachefile)
  } else {
    tictoc::tic()
    ea_historic_ranges <- ea_stations_sf %>%
      st_filter(catchment_boundary_sf) %>%
      as_tibble() %>%
      # filter(
      #   station_id == 'http://environment.data.gov.uk/flood-monitoring/id/stations/1029TH'
      #   |
      #   station_id == 'http://environment.data.gov.uk/flood-monitoring/id/stations/0006'
      # ) %>%
      select(station_id) %>%
      #      head(100) %>%
      #      mutate(response = furrr::future_map(station_id, get_ea_stage_summary)) %>%
      mutate(response = map(station_id, get_ea_stage_summary)) %>%
      unnest(response) %>%
      na.omit()
    tictoc::tic()
    beepr::beep()
    Sys.sleep(3)
    saveRDS(object = ea_historic_ranges, file = ea_rage_cachefile)
  }
  
  ea_stats <- ea_stations_sf %>%
    select(station_id, label, river_name) %>%
    mutate(compound_id = paste0(river_name, "-", label), .before = 1) %>%
    #      st_filter(catchment_boundary_sf) %>%
    inner_join(
      # get the latest measurements for the stages
      ea_station_measurements %>%
        filter(qualifier == "Stage") %>%
        select(station_id, measure_id) %>%
        inner_join(ea_latest) %>%
        select(station_id, date_time, value, unit_name)
    ) %>%
    # join the historic data so we can compare against max values
    inner_join(ea_historic_ranges) %>%
    st_join(edm_df, join = st_nearest_feature) %>%
    mutate(pct_typical_high = value / typical_range_high) %>%
    filter(pct_typical_high > 0)
  
  
  p<- ea_stats %>%
    as_tibble() %>%
    mutate(compound_id = fct_reorder(compound_id, pct_typical_high)) %>%
    ggplot() +
    geom_point(aes(x = pct_typical_high, y = compound_id, colour = display_status, alpha = display_status)) +
    scale_color_manual("EDM status", values = c( "orange", "red", "green", "grey")) +
    scale_alpha_manual("EDM status", values = c( 0.5, 1, 0.1, 0.01)) +
    scale_x_continuous(labels = scales::percent) +
    labs(title = "EDM status linked to the loading of the nearest EA stage",
         x = "Current gauge level as percvent of typical level",
         y = "River & town")
  
  p
  plotly::ggplotly(p)
  ea_stats %>%
    group_by(alert_status) %>%
    summarise(pct_typical_high = mean(pct_typical_high))
}

if(T){
  library(leaflet)
  leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = catchment_boundary_sf %>% sf::st_transform(crs = sf::st_crs(4326)),
                fill = FALSE) %>%
    addCircles(data = net %>% activate(nodes) %>% st_as_sf() %>% sf::st_transform(crs = sf::st_crs(4326)),
               col = "grey", opacity = 0.1) %>%
    addCircles(data = ea_stations_sf %>% st_filter(catchment_boundary_sf) %>% sf::st_transform(crs = sf::st_crs(4326)),
               col = "green", opacity = 0.1) %>%
    addCircles(data = edm_df %>% sf::st_transform(crs = sf::st_crs(4326)),
               col = "blue", opacity = 0.1)
}


# add thames water's data points to net
# snap the EDM data to the River Thames ----
if(T) {
  # https://luukvdmeer.github.io/sfnetworks/articles/sfn03_join_filter.html
  
  # Find indices of nearest nodes to the edms
  nearest_nodes_to_edm = st_nearest_feature(edm_df, net)
  # Snap geometries of POIs to the network.
  snapped_edms = edm_df %>%
    st_set_geometry(st_geometry(net)[nearest_nodes_to_edm])
  # this is one way to add the edm info to the river network
  net_edm <- st_join(net, snapped_edms %>% select(edm_id))
  
  # Find indices of nearest nodes to the edms
  nearest_nodes_to_ea = st_nearest_feature(ea_stations_sf, net_edm)
  # Snap geometries of POIs to the network.
  snapped_ea = ea_stations_sf %>%
    st_set_geometry(st_geometry(net_edm)[nearest_nodes_to_ea])
  net_edm_ea <- st_join(net_edm, snapped_ea %>% select(station_id))
  
  net_edm_ea %>%
    activate(nodes) %>%
    filter(!is.na(edm_id)) %>%
    filter(!is.na(station_id)) %>%
    as_tibble() %>% as_tibble() %>%
    left_join(edm_df %>% as_tibble() %>% select(edm_id, alert_status, discharge_duration)) %>%
    left_join(ea_station_measurements %>% select(station_id, measure_id, parameter_name)) %>%
    left_join(ea_latest %>% select(measure_id, value, unit_name)) %>%
    as_tibble() %>% as_tibble() %>%
    filter(alert_status == "Discharging") %>%
    filter(str_detect(parameter_name, "Water")) %>%
    ggplot() +
    geom_point(aes(discharge_duration, value))
  
  
  if(F) {
    # or this (with tolerance) using blended
    # DOESNT WORK,  runs out of memory on my laptop
    blended = st_network_blend(net, edm_df %>% head(2)) #, tolerance = 10)
    blended
  }
  
  if(F) {
    plot_connections = function(pois) {
      for (i in seq_len(nrow(pois))) {
        connection = st_nearest_points(pois[i, ], activate(net, "edges"))
        plot(connection, col = "grey", lty = 2, lwd = 2, add = TRUE)
      }
    }
    
    plot(net, cex = 2, lwd = 4)
    plot_connections(snapped_pois)
    plot(snapped_pois, pch = 8, cex = 2, lwd = 2, add = TRUE)
    plot(blended, cex = 2, lwd = 4)
  }
}

worst_dicharge_id <- augmented_net %>%
  activate(nodes) %>%
  mutate(row_num = row_number()) %>%
  filter(location_name == snapped_pois[which.max(snapped_pois$discharge_duration),]$location_name) %>%
  as_tibble() %>% as_tibble() %>%
  select(row_num) %>%
  as.integer()

x <- unique(unlist(    igraph::shortest_paths(augmented_net, from = worst_dicharge_id, to = igraph::V(augmented_net), mode = "out")[[1]]))

x

# I like the leaflet package for interactive maps.
# 
# {r label: sample leaflet plots}
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

# Introducing sfnetwork…
# 
# {r label: introducing sfnetworks}
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
  
  os_or_hydro_node %>%
    filter(id == "idD45C4203-CD27-43D4-81B4-39613012C9D2")
  
  # this looks like no links are reversed,  so we can load up the network 
  os_or_watercourse_link %>% as_tibble() %>% select(-geom) %>% count(flowDirection)
  
  
  sfnet_rivers <- sfnetwork(nodes = os_or_hydro_node,
                            edges = os_or_watercourse_link,
                            node_key = "id")
  sfnet_rivers %>% readRDS("sfnet_rivers.Rds")
}


# plot(sfnet_rivers)

# Define the origin location.
the_centroid <- sfnet_rivers %>%
  st_geometry() %>%
  st_combine() %>%
  st_centroid()

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
neigh_net500  <- sfnet_rivers %>%
  activate("edges") %>%
  tidygraph::convert(to_spatial_neighborhood, the_centroid, threshold = 5000, weights = edge_length())
# Subset neighborhood.
neigh_net1000 <- sfnet_rivers %>%
  activate("edges") %>%
  tidygraph::convert(to_spatial_neighborhood, the_centroid, threshold = 100000, weights = edge_length())


if(F) {
  plot(neigh_net1000, col = "grey")
  plot(neigh_net500, col = "grey")
  # plot(neigh_net500, col = "red", add = TRUE)
  
  neigh_net1000 %>% 
    st_as_sf() %>% View()
  #  mutate(geom = st_make_valid(geom)) %>%
  leaflet() %>%
    addTiles(group = "OSM (default)") %>%
    addPolylines(popup = ~makeHTML(paste0('Details:<br>',
                                          'name:', watercourseName, '<br>',
                                          'alt:', watercourseNameAlternative, '<br>',
                                          buildGoogleMapsURLFromStGeom(st_centroid(geom))))
    )  
}

# and now looking at "communities" (sections of river that are connected somehow to eachother.
#                                   




#                                  {r label: visualising sfnetworks as sf objects}
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


