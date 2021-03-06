### Observer for redrawing the map
observe({

  track <- redraw_listener()

  if(!is.null(track)) {

    sf::write_sf(sf::st_transform(track, 4326),  gt)

    gj <- jsonlite::fromJSON(gt, simplifyVector = FALSE)

    unlink(gt)

  } else {
    gj <- NULL
  }

  bb <- app_env$zoom

  # Base map
  output$trackmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      leaflet.extras::addGeoJSONv2(gj, markerType = "circleMarker",
                 stroke = FALSE, fillColor = "black", fillOpacity = 0.7,
                 markerOptions = markerOptions(radius = 2)) %>%
      fitBounds(bb$west, bb$north, bb$east, bb$south)
  })

})

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

    trackmap_proxy = leafletProxy("trackmap") %>%
      removeMarker(layerId = "start") %>%
      removeMarker(layerId = "end")

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

    trackmap_proxy = leafletProxy("trackmap") %>%
      removeMarker(layerId = "start") %>%
      addCircleMarkers(click$lng, click$lat, radius = 2, color = "green", layerId = "start")

  } else if(app_env$cp$start_id != in_id) {

    app_env$cp$end_id <- in_id

    trackmap_proxy = leafletProxy("trackmap") %>%
      removeMarker(layerId = "end") %>%
      addCircleMarkers(click$lng, click$lat, radius = 2, color = "red", layerId = "end")
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
    removeMarker(layerId = "point") %>%
    addCircleMarkers(click$lng, click$lat, radius = 2, layerId = "point")

  app_env$cp$point <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)),
                                 crs = 4326)

})

# Observer for controlpoint$cp reactive value
observe({
  output$controlpoint <- renderPrint(app_env$cp)
})

# Reactive triggered by undo button.
last_track <- eventReactive(input$undobutton | input$undobutton_ele, {

  if(length(app_env$history) == 1) {

    (app_env$history[[1]])

  } else {

    app_env$history <- app_env$history[1:(length(app_env$history) - 1)]

  }

  length(app_env$history)

}, ignoreNULL = FALSE)

# Reactive triggered by save point button.
new_track <- eventReactive(input$savebutton, {
  p <- app_env$cp

  if(length(p) > 0 | input$mode == "Resample Track" | input$mode == "Simplify Track") {

    app_env$zoom <- input$trackmap_bounds

    track <- (tail(app_env$history, n = 1)[[1]])

    if(input$mode == "Modify Horizontal Curvature") {

      if(is.null(p$point)) {
        showNotification("Control Point Required",
                         type = "error", duration = NULL)
        return()
      }

      track <- bez_smooth(track,
                          p$start_id, p$end_id,
                          sf::st_coordinates(
                            sf::st_transform(p$point, app_env$proj)),
                          n_points = 10, reset_ids = TRUE)

    } else if(input$mode == "Make Loop") {

      if(p$start_id > p$end_id) {
        temp <- p$start_id
        p$start_id <- p$end_id
        p$end_id <- temp
      }

      cp <- NULL

      if(input$loop_curve_option) {

        if(is.null(p$point)) {
          showNotification("Control Point Required",
                           type = "error", duration = NULL)
        }

        cp <- sf::st_coordinates(
          sf::st_transform(p$point, app_env$proj))

      }

      track <- make_loop(track, p$start_id, p$end_id,
                         lap_start = input$loop_start_id,
                         control = cp,
                         correct_elevation = input$correct_elevation_option)

    } else if(input$mode == "Resample Track") {

      if(is.na(input$resample_tolerance)) {
        showNotification("Enter a tolerance.",
                         type = "error", duration = NULL)
        return()
      }

      track <- resample_track(track, input$resample_tolerance)

    } else {

      track <- simplify_track(track, input$simplify_tolerance)

    }

    track <- add_distance(track, app_env$proj)

    app_env$history <- c(app_env$history, list(track))

  }

  length(app_env$history)

}, ignoreNULL = FALSE)
