library(sf)
library(tidyverse)
library(lubridate)
library(sfnetworks)
# library(tidygraph)


#| Stack exchange once again makes it easy to make reproducible code
#| https://stackoverflow.com/questions/64850643/why-cant-i-get-st-read-to-open-a-shapefile-from-a-compressed-file-map-is-read

read_shape_URL <- function(URL){
  cur_tempfile <- tempfile()
  download.file(url = URL, destfile = cur_tempfile)
  out_directory <- tempfile()
  unzip(cur_tempfile, exdir = out_directory)
  
  sf::read_sf(dsn = out_directory) #read_sf also works here st_read
}
wc_bundaries_sf <- read_shape_URL("https://data.parliament.uk/resources/constituencystatistics/water/SewerageServicesAreas_incNAVsv1_4.zip") %>%
  rename(geom = geometry) %>%
  # we don't need the full resolution here, save a few Mb
  # by simplifying to a tollerance of 10 meters because we're in CRS 27700
  st_simplify(dTolerance = 10)


read_open_rivers <- function(URL){
  cur_tempfile <- tempfile()
  download.file(url = URL, destfile = cur_tempfile)
  out_directory <- tempfile()
  unzip(cur_tempfile, exdir = out_directory)
  
  sf::read_sf(dsn = out_directory) #read_sf also works here st_read
  # note: I did try to automate this task based on
  # https://stackoverflow.com/questions/64850643/why-cant-i-get-st-read-to-open-a-shapefile-from-a-compressed-file-map-is-read
  result$os_or_hydro_node <- sf::st_read(dsn = out_directory, layer = "HydroNode")
  result$os_or_watercourse_link <- sf::st_read(dsn = out_directory, layer = "WatercourseLink")
  
  return(result)
}

  
sf::st_layers("C:/Users/leoki/DATA/OS/oprvrs_gpkg_gb/data/oprvrs_gb.gpkg")
#| note: I did try to automate this task, pulling the data from
#| https://api.os.uk/downloads/v1/products/OpenRivers/downloads?area=GB&format=GeoPackage&redirect
#| based on:
#| https://stackoverflow.com/questions/64850643/why-cant-i-get-st-read-to-open-a-shapefile-from-a-compressed-file-map-is-read
#| but it didnt work :-/
os_or_hydro_node <- sf::st_read("C:/Users/leoki/DATA/OS/oprvrs_gpkg_gb/data/oprvrs_gb.gpkg", layer = "HydroNode")
os_or_watercourse_link <- sf::st_read("C:/Users/leoki/DATA/OS/oprvrs_gpkg_gb/data/oprvrs_gb.gpkg", layer = "WatercourseLink")


target_river <- "river thames" # exercise for the reader: what's odd about these two catchments: "yare", "waveney"
# notice there are sections of the river Thames that are missing
# (e.g. just east of 0' as it passes through central london)
if(T) {
  os_or_watercourse_link %>%
    filter(str_detect(str_to_lower(watercourseName), target_river)
           | str_detect(str_to_lower(watercourseNameAlternative), target_river)) %>%
    plot()
  
  if(F) {
    os_or_watercourse_link %>%
      filter(str_detect(str_to_lower(watercourseName), target_river)
             | str_detect(str_to_lower(watercourseNameAlternative), target_river)) %>%
      ggplot() +
      geom_sf(aes(colour = form)) +
      geom_sf(data = tribble(~lat, ~lon, 
                             51.35, -2.1,
                             51.8,0.8) %>% 
                st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
                st_bbox() %>% 
                st_as_sfc(),
              colour = "red", alpha = 0.01, linewidth = 1) +
      labs(title = "The River Thames is only partially labelled")
    
  }
  
  bbox_thames <-tribble(~lat, ~lon, 
                        51.4, 0.1,
                        51.55,0.35) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  os_or_watercourse_link %>%
    filter(str_detect(str_to_lower(watercourseName), target_river)
           | str_detect(str_to_lower(watercourseNameAlternative), target_river)) %>%
    ggplot() +
    geom_sf(aes(colour = form)) +
    geom_sf(data = bbox_thames, colour = "red", alpha = 0.01, linewidth = 1) +
    labs(title = "The River Thames is only partially labelled",
         subtitle = paste0("This plots shows all links with name '", str_to_title(target_river), "'"))
  
  library(leaflet)
  os_or_watercourse_link %>%
    st_transform(crs = st_crs(4326)) %>%
    # mutate(namesThames = ifelse((str_detect(str_to_lower(watercourseName), target_river)
    #        | str_detect(str_to_lower(watercourseNameAlternative), target_river)), "Yes", "No")) %>%
    st_filter(tribble(~lat, ~lon, 
                      51.35, -2.1,
                      51.8,0.8) %>% 
                st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
                st_bbox() %>% 
                st_as_sfc()) %>%
    st_filter(bbox_thames) %>%
    mutate(geom = st_make_valid(geom)) %>%
#    as_tibble() %>%
#    mutate(namesThames = coalesce(namesThames, "No")) %>%
  leaflet() %>%  
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addPolygons(data = bbox_thames) %>%
    addPolylines(
      label = ~paste0(watercourseName, ' - ',
                             watercourseNameAlternative)
    ) %>% 
    addLayersControl(
      baseGroups = c("OSM (default)", "Terrain", "Toner Lite"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  
  os_or_hydro_node %>%
    as_tibble() %>%
    count(hydroNodeCategory)
  
  # there's one outlet for the Thames
  river_outlet_link <- os_or_watercourse_link %>%
    filter(str_detect(str_to_lower(watercourseName), target_river)) %>%
    inner_join(os_or_hydro_node %>%
                 as_tibble() %>%
                 filter(hydroNodeCategory == "outlet") %>%
                 select(id),
               by = c("endNode" = "id")
    ) # id F00735F4-5883-4681-80B3-F7DB33BF036C
  
  river_outlet_node <- river_outlet_link$endNode
  # but loads of sources for the river (at weirs etc.)
  river_source <- os_or_watercourse_link %>%
    filter(str_detect(str_to_lower(watercourseName), target_river)) %>%
    inner_join(os_or_hydro_node %>%
                 as_tibble() %>%
                 filter(hydroNodeCategory == "source") %>%
                 select(id),
               by = c("startNode" = "id")
    )
  
}

#| we're going up a gear... use sfnetworks to do both geospatial and network analysis
#| sf for geospatial manipulation like slipping and linking other spatial datasets
#| igraph to do the network type analysis like
#|     splitting the river network up into catchments according to connectivity
#|     or tracing along sections of rivers from point to point
library(sfnetworks)
# create a sf network for the whole of the UK ----
if(T) {
  if(F) {
    #| note:  you're supposed to be able to create sfnetwork from nodes & links
    #| as described here:
    #| https://luukvdmeer.github.io/sfnetworks/articles/sfn01_structure.html
    #| e.g. as disabled below,
    #| but for some reason it eats all my memory & crashes
    sfnet_rivers <- sfnetwork(nodes = os_or_hydro_node,
                              edges = os_or_watercourse_link %>% rename(from = startNode, to = endNode),
                              node_key = "id")
  }
  
  #| SO I'm doing it a slightly different way using the links
  tictoc::tic()
  sfnet_rivers <- as_sfnetwork(os_or_watercourse_link)
  tictoc::toc()

  #| ...then... adding some info back onto the nodes by:
  #| get the original IDs for the nodes from the link start/ends
  #| so that we can then link in the information from the os_or_hydro_node
  sfnet_rivers <- sfnet_rivers %>%
    # here
    activate(links) %>%
    # look up the hydroNodeCategory of the startNode
    left_join(os_or_hydro_node %>% as_tibble() %>% as_tibble() %>% select(-geom) %>%
                rename(startNode = id, start_cat = hydroNodeCategory)) %>%
    # look up the hydroNodeCategory of the endNode
    left_join(os_or_hydro_node %>% as_tibble() %>% as_tibble() %>% select(-geom) %>%
                rename(endNode = id, end_cat = hydroNodeCategory)) %>%
    # here
    activate(nodes) %>%
    # reunite the nodes with the startNode & endNode ids
    # look up the startNode of links connected FROM it
    mutate(id = row_number()) %>%
    left_join(sfnet_rivers %>%
                activate(links) %>%
                as_tibble() %>%
                transmute(id = from, startNode) %>% # FROM
#                transmute(id = from, startNode, start_watercourse = coalesce(watercourseName, watercourseNameAlternative)) %>% # FROM
                as_tibble() %>%
                select(-geom) %>%
                distinct()
              ) %>%
    # look up the end of links connected TO it
    left_join(sfnet_rivers %>%
                activate(links) %>%
                as_tibble() %>%
                transmute(id = to, endNode) %>% # TO
                #                transmute(id = to, endNode, end_watercourse = coalesce(watercourseName, watercourseNameAlternative)) %>% # TO
                as_tibble() %>%
                select(-geom) %>%
                distinct()
              ) %>%
    # both TO & FROM should hold the same chr ID
    # sometimes one or the other will be blank (at the ends of the network)
    # so lets collapse the IDs from either by coalescing
    mutate(node_id = coalesce(startNode, endNode)) %>%
    rename(startNodeFromLinks = startNode, endNodeFromLinks = endNode) %>%
    left_join(os_or_hydro_node %>% as_tibble() %>% as_tibble() %>% select(-geom),
              by = c("node_id" = "id"))
}

# general investigations 
if(F) {
  
  sfnet_rivers %>%
    activate(links) %>%
    as_tibble() %>% as_tibble() %>%
    count(start_cat)
    
  sfnet_rivers %>%
    activate(links) %>%
    as_tibble() %>% as_tibble() %>%
    transmute(id = from, startNode) %>% # FROM
    distinct() %>% group_by(startNode) %>% mutate(n_times = n()) %>% ungroup() %>% filter(n_times>1)
    filter(startNode == "id002BF4B2-AF2F-4EED-9AFA-18AB2069EA97")
    
    # there are many sources for some rivers
    sfnet_rivers %>%
      activate(links) %>%
      as_tibble() %>% as_tibble() %>%
      mutate(watercourse = coalesce(watercourseName, watercourseNameAlternative)) %>%
      filter(start_cat == "source") %>%
      count(watercourseName, watercourseNameAlternative, start_cat, sort = T)
    
    sfnet_rivers %>%
      activate(links) %>%
      as_tibble() %>% as_tibble() %>%
      mutate(watercourse = coalesce(watercourseName, watercourseNameAlternative)) %>%
      filter(watercourse == "River Waveney") %>% View()
    
    sfnet_rivers %>%
      activate(links) %>%
      as_tibble() %>% as_tibble() %>%
      mutate(watercourse = coalesce(watercourseName, watercourseNameAlternative)) %>%
      filter(watercourse == "River Yare") %>% View()
      filter(end_cat == "outlet") %>%
      count(watercourseName, watercourseNameAlternative, end_cat, sort = T) 
    filter(watercourseName == "River Thames")
}

# define catchments (bits of the river network that are not connected)
# as sub=graphs (igraph::clusters)
# so that we can then subset to the Thames catchment
# (note we cant just filter for river_id = "Thames") ----
if(T) {
  #                                   {r label: separating the entire rivre network in to catchments}
  # make an igraph to make the associations easier
  
  # id84B36155-D67B-4C00-A625-9473EB60F828

  
  rg <- igraph::components(sfnet_rivers)
  
  # add the groups to the nodes
  sfnet_rivers_grouped <- sfnet_rivers %>%
    # push the river group onto the nodes
    activate("nodes") %>%
    mutate(rg = as_factor(rg$membership)) %>%
    group_by(rg) %>%
    mutate(n_nodes = n()) %>%
    ungroup()
    

  # add the groups (of the end_node) to the links
  sfnet_rivers_grouped <- sfnet_rivers_grouped %>%
    activate(links) %>%
    left_join(
      sfnet_rivers_grouped %>%
        activate(nodes) %>%
        as_tibble() %>%
        as_tibble() %>%
        transmute(endNode = node_id, rg, n_nodes)
    )
}

save_sf_network_as_geopkg <- function(this_sfnet, target_gpkg) {
  message(paste0("Exporting sf_network links to: ", target_gpkg))
  this_sfnet %>%
    activate(links) %>%
    st_as_sf() %>%
    st_write(dsn = target_gpkg, layer= "links", append=FALSE)
  
  message(paste0("Exporting sf_network nodes to: ", target_gpkg))
  this_sfnet %>%
    activate(nodes) %>%
    st_as_sf() %>%
    st_write(dsn = target_gpkg, layer= "nodes", append=FALSE)
  message("done")
}

if(F) {
  sfnet_rivers_grouped %>% save_sf_network_as_geopkg("C:/Users/leoki/DATA/LAK/os_river.gpkg")
  saveRDS(object = sfnet_rivers_grouped, file = "C:/Users/leoki/DATA/LAK/os_river.Rds")
  saveRDS(object = wc_bundaries_sf, file = "C:/Users/leoki/DATA/LAK/wc_boundaries_sf.Rds")
}

if(F) {
  sfnet_rivers %>%
    activate(nodes) %>%
    filter(hydroNodeCategory == "source") %>%
    as_tibble() %>%
    as_tibble() %>%
    select(node_id) 
  
  sfnet_rivers %>%
    activate(edges) %>%
    inner_join(sfnet_rivers %>%
                 activate(nodes) %>%
                 filter(hydroNodeCategory == "source") %>%
                 as_tibble() %>%
                 as_tibble() %>%
                 select(node_id), by = c("startNode" = "node_id")) %>%
    activate(edges) %>%
    filter(str_detect(str_to_lower(watercourseName), target_river)
           |
             str_detect(str_to_lower(watercourseNameAlternative), target_river)) %>%
    activate("nodes") %>%
    filter(!tidygraph::node_is_isolated()) %>%
    activate(edges) %>%
    as_tibble() %>% as_tibble() %>% View()
  
  
  
  sfnet_rivers %>%
    activate(edges) %>%
    st_as_sf() %>% # names()
    as_tibble() %>%
    count(id, sort = T)
}  


# load the water company boundaries ----
if(T) {
  # zip file:
  # https://data.parliament.uk/resources/constituencystatistics/water/SewerageServicesAreas_incNAVsv1_4.zip
  tw_boundary_sf <-wc_bundaries_sf %>%
    filter(str_detect(COMPANY, "Thames")) %>%
    st_simplify(dTolerance = 100)
  
  tw_boundary_sf %>%
    ggplot() +
    geom_sf() +
    labs(title = "the Thames Water boundary")

  # colour the rivers nodes up by their group
  if(F) {
    sfnet_rivers_grouped %>%
      activate(nodes) %>%
      st_as_sf() %>%
      sample_n(1000) %>%
      ggplot() +
      geom_sf(aes(colour = rg, size = n_nodes), alpha = 0.2) +
      labs(title = "British waterways",
           x = "long",
           y = "lat",
           caption = paste0("Data source: https://www.data.gov.uk/")) +
      theme_dark() +
      theme(line = element_blank()) +
      theme(text = element_blank()) +
      #    theme(title = element_blank()) +
      theme(legend.position="none")
  }
}

# tmap,  faster than ggplot but the default colour scheme isnt as nice
if(F) {
  library(tmap)
  
  tmap_tw <- sfnet_rivers_grouped %>%
    activate(links) %>%
    st_as_sf() %>%
    st_filter(tw_boundary_sf) %>%
    #      group_by(watercourseName, rg) %>% summarise(length = sum(length)) %>% # simplify https://stackoverflow.com/questions/69175360/is-there-a-way-to-merge-contiguous-multilinestrings-sf-object-in-r
    group_by(watercourseName) %>%
    mutate(river_length = sum(length)) %>%
    ungroup() %>%
    mutate(rg = fct_drop(rg)) %>%
    mutate(all_length = sum(length)) %>%
    mutate(norm_length = river_length / all_length) %>%
    mutate(norm_length = ifelse(is.na(watercourseName), 0.01, norm_length)) %>%
    mutate(norm_length = 0.05 + 0.95*(norm_length - min(norm_length))/(max(norm_length) - min(norm_length))) %>%
    #    mutate(norm_length = sqrt(norm_length)) %>%
    select(watercourseName, rg, length, river_length, norm_length, all_length) %>%
    #      group_by(watercourseName) %>% summarise(norm_length = mean(norm_length)) %>% arrange(desc(norm_length)) %>% View()
    # as_tibble() %>% View()
#    sample_n(1000) %>%
    tm_shape() + 
#    tm_lines(col = "rg") +
    tmap_options(max.categories = 100) +
    tm_lines(col = "rg", lwd = "norm_length", palette = "viridis") +
    tm_layout(bg.color = "black", legend.show = FALSE)
  
  tmap_save(tm = tmap_tw, filename = "tmap_thames.png", width=300, height=300, asp=0, dpi = 600, units = "mm")
}

# hi res plotting of Thames
if(F) {
  
  if(T) {
    library(hrbrthemes)
    tw_p <- sfnet_rivers_grouped %>%
      activate(links) %>%
      st_as_sf() %>%
      st_filter(tw_boundary_sf) %>%
#      group_by(watercourseName, rg) %>% summarise(length = sum(length)) %>% # simplify https://stackoverflow.com/questions/69175360/is-there-a-way-to-merge-contiguous-multilinestrings-sf-object-in-r
      group_by(watercourseName) %>%
      mutate(river_length = sum(length)) %>%
      ungroup() %>%
      mutate(all_length = sum(length)) %>%
      mutate(norm_length = river_length / all_length) %>%
      mutate(norm_length = ifelse(is.na(watercourseName), 0.01, norm_length)) %>%
      mutate(norm_length = 0.05 + 0.95*(norm_length - min(norm_length))/(max(norm_length) - min(norm_length))) %>%
      #    mutate(norm_length = sqrt(norm_length)) %>%
      select(watercourseName, rg, length, river_length, norm_length, all_length) %>%
#      group_by(watercourseName) %>% summarise(norm_length = mean(norm_length)) %>% arrange(desc(norm_length)) %>% View()
      # as_tibble() %>% View()
#      sample_n(1000) %>%
      ggplot() +
      geom_sf(aes(colour = rg, alpha = norm_length)) +
      theme_ft_rc(grid = F,axis_text_size = F) +
      theme(legend.position = "none") +
      labs(
        # title = "Thames Water",
        subtitle = glue::glue("Thames Water river network"),
        # x = "", y = "",
        caption = "Rivers: https://www.data.gov.uk/\nBoundary: https://data.parliament.uk/"
      ) 

  }
  
  if(F) {

    if(F) {
      ggsave(filename = "thames_catchment_ggsave_600.png", plot = tw_p, width = 14, height = 10, dpi = 600,  units = "cm")
      beepr::beep()
    } else {
      tictoc::tic()
      Cairo::Cairo(
        height = 30, width = 30, # high res,  lines are better but still dark
        #        height = 75, width = 100, # high res,  lines are thin so it look s a bit dark
        # height = 50, width = 50, # high res,  lines are better but still dark
        file = "thames_catchment_cairo_600.png",
        type = "png", #tiff
        bg = "transparent", #white or transparent depending on your requirement 
        dpi = 600,
        units = "cm" #you can change to pixels etc 
      )
      plot(tw_p) #p is your graph object 
      dev.off()
      tictoc::toc()
      img <- png::readPNG(here::here("thames_catchment_cairo_600.png"))
      grid::grid.raster(img)
    }
  }

}

# have a look at the numbers of start & end nodes per river
sfnet_rivers_grouped %>%
  activate(nodes) %>%
  as_tibble() %>% as_tibble() %>%
  count( sort= T)


# black map of UK
if(F) {
  uk_p <- sfnet_rivers_grouped %>%
    activate(links) %>%
    as_tibble() %>%
    as_tibble() %>%
#    filter(form == "lake") %>%
    #| I want to find the length of each river (so I can emphasise long rivers)
    #| but sometimes rivers have the same name (e.g. River Avon)
    #| so I'm grouping by both watercourseName AND rg here
    group_by(watercourseName, rg) %>%
    mutate(river_length = sum(length)) %>%
    ungroup() %>%
    #| I want to normalise against the overall length of *all* rivers...
    mutate(all_length = sum(length)) %>%
    #| because I'm not grouped by watercourseName & rg any more
    #| each segment of any given river(in a rg) is assigned the same norm_length
    mutate(norm_length = river_length / all_length) %>%
    mutate(norm_length = ifelse(is.na(watercourseName), 0.0001, norm_length)) %>%
    mutate(norm_length = 0.1 + 0.9*(norm_length - min(norm_length))/(max(norm_length) - min(norm_length))) %>%
    #    mutate(norm_length = sqrt(norm_length)) %>%
    select(watercourseName, rg, length, river_length, norm_length, all_length, geom) %>%
    # group_by(watercourseName) %>% summarise(norm_length = mean(norm_length)) %>% arrange(desc(norm_length)) %>% View()
    st_as_sf() %>%
    # as_tibble() %>% View()
    #    sample_n(1000) %>%
    ggplot(aes(colour = rg)) +
    geom_sf() +
    geom_sf(aes(alpha = norm_length)) +
    hrbrthemes::theme_ft_rc(grid = F,axis_text_size = F) +
    theme(legend.position = "none") +
    labs(
      # title = "Thames Water",
      subtitle = glue::glue("UK river network"),
      # x = "", y = "",
      caption = "Rivers: https://www.data.gov.uk/"
    ) 
  
  library(Cairo)
  img_width <- 20 # cm
  img_height <- 40
  img_units <- "cm" # you can change to pixels etc 
  img_dpi <- 600 
  img_type <- "jpg" # png
  tictoc::tic()
  Cairo::Cairo(
    height = img_height, width = img_width, # high res,  lines are better but still dark
#    height = 200, width = 75, # high res,  lines are better but still dark
    file = paste0("uk_rivers_cairo_", img_dpi, "_", img_width, "x",img_height , ".", img_type),
    type = img_type, #tiff jpg png
    bg = "transparent", #white or transparent depending on your requirement 
    dpi = img_dpi,
    units = img_units 
  )
  plot(uk_p) #p is your graph object 
  dev.off()
  tictoc::toc()
  beepr::beep()
  
  if(F) {
    ggsave(filename = "uk_rivers_ggsave.png", plot = uk_p, width = 5, height = 15,  units = "cm", dpi = 300)
    beepr::beep()
  }
}

# extract the River Thames catchment -----
if(T) {
  # extract the Thames…
  # {r label: extracting a specific river}
  
  if(F) { # 2023-02-14
    # get a nodes on the river thames...
    sample_nodeid <- sfnet_rivers_grouped %>%
      activate(edges) %>%
      # find all links named target_river...
      filter(str_detect(str_to_lower(watercourseName), target_river)
             |
               str_detect(str_to_lower(watercourseNameAlternative), target_river)) %>%
      as_tibble() %>%
      select(to) %>%
      as_tibble() %>%
      select(-geom) %>%
      head(1) %>%
      as.integer()
  }
  
  # get the outlet of the river thames
  last_link <- sfnet_rivers_grouped %>%
    activate(edges) %>%
    as_tibble() %>%
    as_tibble() %>%
    #    filter(id == "idF00735F4-5883-4681-80B3-F7DB33BF036C") %>%
    # startNode: idB486A0E1-04FF-4791-8F78-747C96C47660
    #   endNode: id7A436517-D046-48E5-AD56-0B33753EAA4F
    #    View()
    # remove all edges except those on the target_river
    filter(str_detect(str_to_lower(watercourseName), target_river)) %>%
    # this is my way to then sub-filter to nodes that are outlets
    # i.e. the outlet of the River Thames
    inner_join(sfnet_rivers_grouped %>%
                 activate(nodes) %>%
                 # remove nodes that arent outlets
                 filter(hydroNodeCategory == "outlet") %>%
                 as_tibble() %>%
                 as_tibble() %>%
                 transmute(endNode = node_id)# %>% View()
    )
  
  asHTML <- function(text){
    attr(text, "html") <- TRUE
    class(text) <- c("html", "character")
    return(text)
  }
  
  # plot the very last node on the river thames
  library(leaflet)
  sfnet_rivers_grouped %>%
    activate(nodes) %>%
    filter(node_id == last_link$endNode) %>%
    st_as_sf() %>%
    st_transform(crs = st_crs(4326)) %>%
    leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addAwesomeMarkers(
      label = ~asHTML(node_id)
    )
  
  # then see which RG the node is part of...
  this_rg <- sfnet_rivers_grouped %>%
    activate("nodes") %>%
    filter(node_id == last_link$endNode) %>%
#    filter(row_number() == sample_nodeid) %>%
    as_tibble() %>%
    select(rg) %>%
    as_tibble() %>%
    select(-geom) %>%
    head(1) %>%
    as.integer()
  

  sfnet_rivers_thames_basin <- sfnet_rivers_grouped %>%
    activate(links) %>%
    filter(rg == this_rg) %>%
    activate(nodes) %>%
    filter(!tidygraph::node_is_isolated()) %>%
    # the row number is out of date now (maybe drop entirely?)
    # it needs refreshing (or ignoring entirely!)
    mutate(id = row_number())
  
  plot(sfnet_rivers_thames_basin)
  
  sfnet_rivers_grouped  %>%
    activate("nodes") %>%
    filter(node_id == last_link$endNode)
  sfnet_rivers_thames_basin  %>%
    activate("nodes") %>%
    filter(node_id == last_link$endNode)
}

# I'd like to plot a ggraph plot but it's not quite woring
if(F) {
  # {r label: a graph based view of the river network}
  # https://stackoverflow.com/questions/75011724/using-r-package-sfnetworks-to-subset-river-network-that-is-upstream-of-a-given-n
  #' visualize directness of network
  library(ggraph)
  sfnet_rivers_thames_basin %>%
    activate(links) %>%
    as.igraph() %>%
    igraph::reverse_edges() %>%
#    ggraph() +
    ggraph(layout = "dendrogram") +
    geom_node_point(size = 1) +
    geom_edge_link(arrow = arrow(length = unit(2, 'mm'), ends = "first"),
                   end_cap = circle(2, 'mm'),
                   start_cap = circle(2, 'mm')) +
    theme_graph()
  
}

#| we might want to restrict data that is in the catchment...
#| so lets make a concave polygon surrounding the catchment
if(T){
  #| https://community.rstudio.com/t/adding-manual-legend-to-ggplot2/41651/3
  #| to label the layers
  colors <- c("Thames tributaries" = "grey",
              "TW Region" = "green",
              "Catchment boundary" = "blue",
              "Water Company Boundaries" = "black"
  )
  
  # boundary of the catchment
  # install.packages("concaveman")
  catchment_boundary_sf <- sfnet_rivers_thames_basin %>%
    activate(nodes) %>%
    st_as_sf() %>%
    summarise() %>% 
    concaveman::concaveman() %>%
    st_zm()

  
    ggplot() +
    geom_sf(data = sfnet_rivers_thames_basin %>% activate(links) %>% st_as_sf(), aes(colour = "Thames tributaries"), alpha = 0.5) +
    geom_sf(data = wc_bundaries_sf %>% filter(COMPANY == "Thames Water"), aes(colour = "TW Region"), colour = "green", alpha = 0.5) +
    geom_sf(data = catchment_boundary_sf, aes(colour = "Catchment boundary"), alpha = 0.1) +
#    geom_sf(data = wc_bundaries_sf, aes(colour = "Water Company Boundaries"), colour = "green", alpha = 0.5) +
    labs(title = "Locations of EA stations andThame Water's EDMs",
         x = "long",
         y = "lat",
         color = "Legend") +
    scale_color_manual(values = colors)
}

if(F) {
  save.image("willcontainworkspace.RData")
  load("willcontainworkspace.RData")
  beepr::beep()
  library(sf)
  library(tidyverse)
  library(lubridate)
  library(sfnetworks)
  library(tidygraph)
}

#| get some points on the river network that we might like to route-find between ----
if(T) {
  # rivers in this dataset have a lot of sources
  westernmost_target_node <- sfnet_rivers_thames_basin %>%
    activate(nodes) %>%
    mutate(id = row_number()) %>%
    st_as_sf() %>%
    mutate(lon = sf::st_coordinates(.)[,1],
           lat = sf::st_coordinates(.)[,2]) %>%
    arrange(lon) %>% # longitude is smallest in the west
    filter(hydroNodeCategory == "source")  %>%
    # make sure it's on the thames
    inner_join(
      sfnet_rivers_thames_basin %>%
        activate(links) %>%
        as_tibble() %>% as_tibble() %>%
        filter(str_detect(str_to_lower(watercourseName), target_river)) %>%
        #        filter(end_cat == "outlet")  %>%
        select(node_id = startNode)
    ) %>%
    # select the (single) easternmost node
    slice(1) 
  
  source_kennet_nodes <- sfnet_rivers_thames_basin %>%
    activate(nodes) %>%
    mutate(id = row_number()) %>%
    st_as_sf() %>%
    mutate(lon = sf::st_coordinates(.)[,1],
           lat = sf::st_coordinates(.)[,2]) %>%
    arrange(lon) %>% # longitude is smallest in the west
    filter(hydroNodeCategory == "source")  %>%
    #    arrange(desc(lon)) %>% # longitude is smallest in the west
    # make sure it's on the right river
    inner_join(
      sfnet_rivers_thames_basin %>%
        activate(links) %>%
        as_tibble() %>% as_tibble() %>%
        filter(str_detect(str_to_lower(watercourseName), "kennet")) %>%
        select(node_id = startNode)
    ) # %>%
    # select the (single) easternmost node
    # slice(1) 
  
  river_outlet_node <- sfnet_rivers_thames_basin %>%
    activate(nodes) %>%
    mutate(id = row_number()) %>%
    as_tibble() %>% as_tibble() %>%
    filter(hydroNodeCategory == "outlet") %>%
    inner_join(
      sfnet_rivers_thames_basin %>%
        activate(links) %>%
        as_tibble() %>% as_tibble() %>%
        filter(str_detect(str_to_lower(watercourseName), target_river)) %>%
#        filter(end_cat == "outlet")  %>%
        select(node_id = endNode)
    )
  
  
  sfnet_rivers_thames_basin %>%
    activate(nodes) %>%
    as_tibble() %>% as_tibble() %>%
    filter(row_number() == river_outlet_node$id)
  
  sfnet_rivers_thames_basin %>%
    activate(nodes) %>%
    as_tibble() %>% as_tibble() %>%
    filter(row_number() == westernmost_target_node$id)
}

# generate the route (shortest paths) between oms of the selected nodes ----
if(T) {
  if(F) {
    # without a "to" parameter we will find a paths to all nodes
    # nodes that are unreachable will have zero length paths
    # other nodes will have a list of nodes on the route to them
    paths_downstream_target <- st_network_paths(sfnet_rivers_thames_basin,
                                                from = as.integer(westernmost_target_node$id),
                                                # to hasnt been defined,  so it's to EVERYTHING
                                                #  this function defaults to type = 'shortest'
                                                type = "shortest", # 'shortest', 'all_shortest', 'all_simple'
                                                weights = "length") %>%
      # this next bit returms the longest path
      # (ie the one going all the way to the end)
      mutate(n_nodes = lengths(node_paths)) %>%
      arrange(desc(n_nodes)) %>%
      slice(1)
  }
  # this just gets the path to the outlet
  paths_downstream_target <- st_network_paths(sfnet_rivers_thames_basin,
                            from = westernmost_target_node$id,
                            to = river_outlet_node$id,
                            weights = "length") %>%
    mutate(n_nodes = lengths(node_paths))
  
  # the hard way--- transposing the graph
  # (dont do this, use mode = "in") instead
  if(F) {
    # there's a "node" function in st_network_paths
    # which can search in either direction (or both)
    # so we dont have to reverse the links to search upstream
    reversed_net_as_igraph <- sfnet_rivers_thames_basin %>%
      activate(links) %>%
      igraph::reverse_edges()
    
    paths_reversed <- igraph::shortest_paths(reversed_net_as_igraph,
                                             from =  river_outlet_node$id,
                                             to = westernmost_target_node$id)
    unlist(paths_downstream_target[[1]]) == rev(unlist(paths_reversed$vpath))
    unlist(paths_downstream_target[[1]])
    rev(unlist(paths_reversed$vpath))
    
    kennet_paths_igraph <- igraph::shortest_paths(reversed_net_as_igraph,
                                                  from =  river_outlet_node$id,
                                                  to = source_kennet_nodes$id)
  }
  
  paths_upstream_target <- st_network_paths(sfnet_rivers_thames_basin,
                             from = river_outlet_node$id,
                             to = westernmost_target_node$id,
                             mode = "in") %>% #c("out", "all", "in")
    mutate(n_nodes = lengths(node_paths))
  
  paths_upstream_kennet <- st_network_paths(sfnet_rivers_thames_basin,
                             from = river_outlet_node$id,
                             to = source_kennet_nodes$id,
                             mode = "in") %>% #c("out", "all", "in")
    mutate(n_nodes = lengths(node_paths))
   
  paths_upstream_both <- st_network_paths(sfnet_rivers_thames_basin,
                                            from = river_outlet_node$id,
                                            to = c(source_kennet_nodes$id, westernmost_target_node$id),
                                            mode = "in") %>% #c("out", "all", "in")
    mutate(n_nodes = lengths(node_paths))
  
  plot_path = function(node_path) {
    sfnet_rivers_thames_basin %>%
      activate("nodes") %>%
      slice(node_path) %>%
      plot(cex = 1.5, lwd = 1.5, col = "green", add = TRUE)
  }
  
  plot_path_along = function(node_path, river_network) {
    river_network %>%
      activate("nodes") %>%
      slice(node_path) %>%
      plot(cex = 1.5, lwd = 1.5, col = "green", add = TRUE)
  }

  
  colors = sf.colors(3, categorical = TRUE)
  
  plot(sfnet_rivers_thames_basin, col = "grey")
  paths_downstream_target %>%
    pull(node_paths) %>%
    walk(plot_path)
  
  plot(sfnet_rivers_thames_basin, col = "grey")
  sfnet_rivers_thames_basin %>%
    activate("nodes") %>%
    slice(unlist(paths_downstream_target$node_paths)) %>%
    plot(cex = 1.5, lwd = 1.5, col = "green", add = TRUE)
  
  plot(sfnet_rivers_thames_basin, col = "grey")
  paths_upstream_kennet %>%
    pull(node_paths) %>%
    walk(plot_path)
  
  plot(sfnet_rivers_thames_basin, col = "grey")
  paths_upstream_both %>%
    pull(node_paths) %>%
    walk(plot_path)

  unique(unlist(paths_upstream_kennet$node_paths))
  
  sfnet_rivers_thames_basin %>%
    activate("nodes") %>%
#    slice(unlist(kennet_paths$vpath)) %>%
    slice(unique(unlist(paths_upstream_kennet$node_paths))) %>%
    activate(links) %>%
    st_transform(crs = st_crs(4326)) %>%
    mutate(geom = st_make_valid(geom)) %>%
    st_as_sf() %>%
    #    as_tibble() %>%
    #    mutate(namesThames = coalesce(namesThames, "No")) %>%
    leaflet() %>%  
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    addPolylines(
      label = ~paste0(watercourseName, ' - ',
                      watercourseNameAlternative)
    ) %>% 
    addLayersControl(
      baseGroups = c("OSM (default)", "Terrain", "Toner Lite"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  
  if(F) {
    sfnet_rivers_thames_basin %>%
      activate(links) %>%
      st_transform(crs = st_crs(4326)) %>%
      mutate(geom = st_make_valid(geom)) %>%
      #    as_tibble() %>%
      #    mutate(namesThames = coalesce(namesThames, "No")) %>%
      leaflet() %>%  
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addPolylines(
        label = ~paste0(watercourseName, ' - ',
                        watercourseNameAlternative)
      ) %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Terrain", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    }
}

