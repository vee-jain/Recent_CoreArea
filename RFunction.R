library('amt')
library('move2')
library('lubridate')
library('sf')
library('s2')
library('dplyr')
library('RColorBrewer')
library('leaflet')
library('purrr')
library('ggplot2')
library('mapview')
library('webshot')
library('lwgeom')
library('utils')

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
  
  #' Create spatial object for points 
  unnested_track_list <- track_list %>% 
    unnest(info)
  
  #' Convert unnested_track_list to a data frame
  unnested_track_list_df <- as.data.frame(unnested_track_list)
  
  plot_pts <- st_as_sf(x = unnested_track_list_df,                         
                       coords = c("x_", "y_"),
                       crs = st_crs(data))
  
  plot_pts$id <- as.factor(plot_pts$id)
  levels(plot_pts$id)
  
  #' KDE estimates
  sf_use_s2(FALSE) 
  
  hr <- list()
  hr <-  track_list %>%
    mutate( 
      hr_kde = (map(track_list$info, ~hr_kde(., levels = c(0.50)))), #probabilistic
    )
  
  #' Extract isopleths (polygons)
  kde_values <- hr %>% 
    mutate(isopleth = map(hr_kde, hr_isopleths)) %>%
    filter(isopleth != "NA")
  
  #' Add columns back
  isopleths <- unique(do.call(rbind, kde_values$isopleth))
  isopleths$id <- kde_values$id
  
  sf_use_s2(TRUE) 
  
  ####----Intersecting points with core area----####
  #intersecting last week core area with data points for respective individuals
  plot_pts$geometry_s2_pts <- st_as_s2(plot_pts$geometry)
  isopleths$geometry_s2_iso <- st_as_s2(st_make_valid(isopleths$geometry))

  #' points by individual
  indv_pts <- plot_pts %>% group_by(id) %>% nest()
  
  #' creating list structure for storing data
  intersect_dat <- indv_pts
  
  # Define a function to perform the intersection for a pair of data frame and geometry
  process_intersection <- function(data_df, geometry) {
    # Assuming 'geometry_s2_pts' is the column name in 'data_df'
    intersections <- s2_intersects(data_df$geometry_s2_pts, geometry)
    # Add the result as a new column
    data_df$intersection <- (intersections)
    return(data_df)
  }
  
  # Use nested map2 to iterate over both lists
  intersect_dat <- map2(indv_pts$data, isopleths$geometry_s2_iso, process_intersection)
  intersect_dat <- setNames(intersect_dat, indv_pts$id)
  
  combined_df <- dplyr::bind_rows(intersect_dat, .id = "id")
  
  #' if a point intersects, it gets 0, else 1 
  combined_df_final <- combined_df %>%
    mutate(location = ifelse(intersection == 'FALSE', 0, 1))
  combined_df_final <- as.data.frame(combined_df_final)

  ####----Output 1: HTML map----####
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
  
  ####----Output 2: Time in core area----####
  in_core <- combined_df_final %>% 
    mutate(time = as.numeric(format(t_, "%H")))
  
  time_in_core <- in_core %>%
    mutate(id = as.factor(id),
           date = as.Date(t_),
           hour = format(t_, "%H"),
           time_frame = paste(date, hour)) %>%
    droplevels() %>%
    group_by(id, date, hour) %>% summarise(count = sum(as.numeric(location))) %>%
    split(.$id) %>%
    map(~ggplot(data = .x, 
                mapping = aes(x = as.numeric(hour), y = date, fill = count )) + 
          geom_tile()+
          scale_fill_gradient(low = "white", high = "red")+
          geom_text(aes(label = count), color = "black", size = 1.5) +
          theme_classic()+
          labs(x = "Hour", y = "Date",
               title="No. of points in core area",
               subtitle= .$id) +
          theme(legend.position = "none",
                panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))+
          coord_fixed()+
          scale_y_date(date_breaks = "1 day")+
          scale_x_continuous(breaks = seq(0, 23, 1), labels = paste(seq(0, 23, 1), "00", sep = ":"))+
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
                axis.text.y = element_text(size = 6))
    )
  
  #' Output 2: export KDE by interval
  pdf(appArtifactPath("core_time_plots.pdf"),onefile = TRUE)
  walk(time_in_core, print)
  dev.off()
  
  # provide my result to the next app in the MoveApps workflow
  return(data)
}
