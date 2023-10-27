library('amt')
library('move2')
library('lubridate')
library('sf')
library('RColorBrewer')
library('leaflet')
library('purrr')
library('ggplot2')
library('mapview')

## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

# Showcase injecting app setting (parameter `year`)
rFunction = function(data, days_prior, ...) {
  
  ####----Function to make movement track----####
  move2_TO_track.xyt <- function(mv2){
    if(mt_is_move2(mv2)){
      warning("!!INFO!!: only coordinates, timestamps and track IDs are retained")
      track(
        x=sf::st_coordinates(mv2)[,1],
        y=sf::st_coordinates(mv2)[,2],
        t=mt_time(mv2),
        id=mt_track_id(mv2),
        crs = sf::st_crs(mv2)
      )
    }
  }
  
  #' Make movement track
  data_track <- move2_TO_track.xyt(data)
  
  ####----Filter according to user defined 'days_prior'----####
  #' create a target date defined by days prior
  target_date <- (max(data_track$t_) - days(days_prior))
  
  #' filter accordingly
  data_track_filtered <- data_track %>% 
    filter(t_ >=  target_date &
             t_ <= max(t_))
  
  ####----KDE estimation----####
  track_list <- data_track_filtered %>% 
    nest(info = -c(id))
  
  #' Setting requirement for minimum number of points for each interval
  track_list <- track_list %>% 
    mutate(
      row = sapply(track_list$info, nrow)
    ) %>%
    filter(row >= 10)
  
  #' KDE estimates
  sf_use_s2(FALSE) 
  
  hr <- list()
  hr <-  track_list %>%
    mutate( 
      hr_kde = (map(track_list$info, ~hr_kde(., levels = c(0.50)))), #probabilistic
    )
  
  #' KDE maps
  #' Create spatial object for points 
  plot_pts <- sf::st_as_sf(x = data_track_filtered,                         
                           coords = c("x_", "y_"),
                           crs = sf::st_crs(data))
  
  #' Extract isopleths (polygons)
  kde_values <- hr %>% 
    mutate(isopleth = map(hr_kde, possibly(hr_isopleths, otherwise = "NA"))) %>%
    filter(isopleth != "NA")
  
  #' Add columns back
  isopleths <- unique(do.call(rbind, kde_values$isopleth))
  isopleths$id <- kde_values$id
  
  #' Set colours
  col <- brewer.pal(8, "Spectral") 
  pal <- colorNumeric(
    palette = col,
    domain = as.numeric(as.factor(isopleths$id)))
  
  #' Core plots 
  m1 <- leaflet() %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
    addPolygons(data = isopleths, weight = 4, 
                color = ~pal(as.numeric(as.factor(isopleths$id))),
                fillColor = ~pal(as.numeric(as.factor(isopleths$id))),
                fillOpacity = 0.3,
                opacity = 1.0,
                popup = ~ paste0(id),
                group = "Core area") %>%
    addCircleMarkers(data = data_track_filtered,
                     lng = ~x_, lat = ~y_, 
                     radius = 5,
                     popup = ~paste0(id," ",t_),
                     color = "black",
                     fillColor = ~pal(as.numeric(as.factor(data_track_filtered$id))),
                     stroke = TRUE, fillOpacity = 1,
                     weight = 1,
                     group = "Indv. points") %>%
    addLayersControl(
      baseGroups = c("OSM (default)", "World Imagery"),
      overlayGroups = c("Core area", "Indv. points"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  #' Output 1: Export maps as html
  #also exporting these plots into the temporary directory
  dir.create(targetDirHtmlFiles <- tempdir())
  
  mapshot(m1, url = file.path(targetDirHtmlFiles, paste0("map_core_plot.html")))
  
  zip_file <- appArtifactPath(paste0("map_html_files.zip"))
  zip::zip(zip_file, 
           files = list.files(targetDirHtmlFiles, full.names = TRUE,
                              pattern="^map.*html"),
           mode = "cherry-pick")
  
  ####----Time in core area----####
  #intersecting last week core area with data points for respective individuals
  intersect_dat <- plot_pts %>% mutate(
    intersection = as.character(st_intersects(geometry, isopleths$geometry)),
    intersect = as.numeric(intersection),
    location = dplyr::if_else(is.na(intersect), "0", paste0("1"))) 
  
  intersect_df <- as.data.frame(intersect_dat)
  
  in_core <- intersect_df %>% 
    filter(location == 1) %>%
    mutate(time = as.numeric(format(t_, "%H")))
  
  time_in_core <- in_core %>%
    mutate(id = as.factor(id),
           date = as.Date(t_),
           hour = format(t_, "%H"),
           time_frame = paste(date, hour)) %>%
      droplevels() %>%
    split(.$id) %>%
    map(~ggplot(data = .x, 
                mapping = aes(x = hour, fill = hour)) + 
          geom_histogram(stat = "count")+
          theme_minimal()+
          labs(x = "Time", y = "Number of fixes in core area") +
          theme(legend.position = "none",
                panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))+
          facet_grid(date~id)+ theme(strip.text.y = element_text(angle = 0)))
  
  #' Output 2: export KDE by interval
  pdf(appArtifactPath("core_time_plots.pdf"),onefile = TRUE)
  walk(time_in_core, print)
  dev.off()
  
  # provide my result to the next app in the MoveApps workflow
  return(data)
}
