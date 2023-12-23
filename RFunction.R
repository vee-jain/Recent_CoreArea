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
  unnested_track_list <- track_list %>% 
    unnest(info)
  
  plot_pts <- st_as_sf(x = unnested_track_list,                         
                       coords = c("x_", "y_"),
                       crs = st_crs(data))
  plot_pts$id <- as.factor(plot_pts$id)

  #' Extract isopleths (polygons)
  kde_values <- hr %>% 
    mutate(isopleth = map(hr_kde, possibly(hr_isopleths, otherwise = "NA"))) %>%
    filter(isopleth != "NA")
  
  #' Add columns back
  isopleths <- unique(do.call(rbind, kde_values$isopleth))
  isopleths$id <- kde_values$id
  isopleths_sf <- sf::st_as_sf(isopleths)
  isopleths_sf$id <- as.factor(isopleths_sf$id)
  
  #' Set colours
  all_levels <- union(levels(isopleths_sf$id), levels(plot_pts$id))
  nb.cols <- length(unique(all_levels))
  mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)
  
  # Plotting with mapview
  m1 <- mapview(isopleths_sf, zcol = "id", 
          alpha.regions = 0.5, col.regions = mycolors, layer.name = "Core areas")+
    mapview(plot_pts, zcol = "id", col.regions = mycolors,
            burst = TRUE,
            alpha.regions = 0.3, layer.name = "Individual points")
  
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
    group_by(id, date, hour) %>% summarise(count = sum(as.numeric(location))) %>%
    split(.$id) %>%
    map(~ggplot(data = .x, 
                mapping = aes(x = as.numeric(hour), y = date, fill = count )) + 
          geom_tile()+
          scale_fill_gradient(low = "#ffcccc", high = "red")+
          geom_text(aes(label = count), color = "black", size = 2) +
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
