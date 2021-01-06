# Observer for ends / point toggle
observe({
  app_env$ends <- input$point_id == "Ends"
})

# Reactive for end click type
end_click_event <- eventReactive(input$trackmap_geojson_click, {
  app_env$trackmap_geojson_click <-
    if(app_env$ends) {
      input$trackmap_geojson_click
    } else {
      NULL
    }
})

# Observer for click on end point
observe({

  if(is.null(click <- end_click_event())) return()

  in_id <- click$properties$track_seg_point_id

  # If the last event is provided again, do nothing.
  last_event <- app_env$le
  if(last_event == in_id) {
    return()
  }
  # Reset last event for next time
  app_env$le <- in_id

  # If both control point ID slots are full, clear them.
  if(!is.null(app_env$cp$start_id) &&
     !is.null(app_env$cp$end_id)) {
    app_env$cp$start_id <- app_env$cp$end_id <- NULL
  }

  # If in_id is a repeat of the start_id, don't set end.
  # Is this used given last id check above?
  if(!is.null(app_env$cp$start_id) &&
     in_id == app_env$cp$start_id &&
     is.null(app_env$cp$end_id)) {
    return()
  }

  if(is.null(app_env$cp$start_id)) {

    app_env$cp$start_id <- in_id

  } else if(app_env$cp$start_id != in_id) {

    app_env$cp$end_id <- in_id

  }

})

# reactive for point click type
point_click_event <- eventReactive(input$trackmap_click, {
  app_env$trackmap_click <-
    if(!app_env$ends) {
      input$trackmap_click
    } else {
      NULL
    }
})

# Observer for click in space
observe({

  if(is.null(click <- point_click_event())) return()

  trackmap_proxy = leafletProxy("trackmap") %>%
    clearMarkers() %>%
    addCircleMarkers(click$lng, click$lat, radius = 2)

  app_env$cp$point <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)),
                                 crs = 4326)

})

# Observer for controlpoint$cp reactive value
observe({
  output$controlpoint <- renderPrint(app_env$cp)
})

# Reactive triggered by save point button.
# cp_df <- eventReactive(input$savebutton, {
#   p <- app_env$cp
#
#   new_row <- st_sf(start_id = p$start_id, end_id = p$end_id, point = p$point)
#
#   (app_env$df <- rbind(app_env$df, new_row))
# }, ignoreNULL = FALSE)
new_track <- eventReactive(input$savebutton, {
  p <- app_env$cp

  if(length(p) > 0) {
    new_row <- st_sf(start_id = p$start_id, end_id = p$end_id, point = p$point)

    app_env$df <- rbind(app_env$df, new_row)

    app_env$zoom <- input$trackmap_bounds

    (app_env$track <- gpxr:::bez_smooth(app_env$track,
                                        p$start_id, p$end_id,
                                        sf::st_coordinates(
                                          sf::st_transform(p$point, 5070))))
  } else {
    (app_env$track)
  }

}, ignoreNULL = FALSE)

# Observer for cd_df() reactive data table
# observe({
#   output$cpdf <- renderDataTable(cp_df())
# })
