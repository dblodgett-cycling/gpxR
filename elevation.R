redraw_ele_listener <- reactive({
  watch <- c(recalc_elevation(), new_track(), last_track())
  (tail(app_env$history, n = 1)[[1]])
})

output$click_info <- renderUI({
  o <- tail(app_env$history, n = 1)
  if(is.list(o)) {
    out <- nearPoints(o[[1]], input$ele_click, addDist = TRUE)

    if(nrow(out) == 0) return("Click Points for Info")

    out$ele <- round(out$ele, 1)

    if(nrow(out) == 1) {
      return(paste("ele:", out$ele, "id:", out$track_seg_point_id))
    } else {
      m <- out[out$ele == min(out$ele), ]
      n <- out[out$ele == max(out$ele), ]

      return(HTML(paste("max ele:", m$ele[1], "id:", m$track_seg_point_id[1], "<br/>",
                   "min ele:", n$ele[1], "id:", n$track_seg_point_id[1])))
    }

  } else {
    ""
  }

})

# Observer for redrawing the elevation plot
observe({

  track <- redraw_ele_listener()

  if(!is.null(track)) {

    gg <- ggplot(track, aes(distance, ele)) + geom_point(size = 1)

    if("orig_ele" %in% names(track)) {
      gg <- gg + geom_point(data = track, mapping = aes(distance, orig_ele),
                            colour = "red", size = 0.5)
    }

    if(!is.null(app_env$ele_control)) {
      rows <- c(app_env$ele_control$start, nrow(app_env$ele_control$start))

      dist <- c(track$distance[app_env$ele_control$start],
                track$distance[nrow(track)])

      gg <- gg + geom_vline(xintercept = dist)
    }

    output$elevation <- renderPlot(gg)

  } else {

    output$elevation <- renderPlot(ggplot())

  }

})

recalc_elevation <- eventReactive(input$elevation_button, {

  tryCatch({
    dat <- read.table(text = input$elevation_config, header = TRUE, sep = ",")

    dat <- arrange(dat, start)

    app_env$ele_control <- dat

    track <- (tail(app_env$history, n = 1)[[1]])

    if(!is.null(track)) {
      track$orig_ele <- track$ele

      track$ele <- smooth_elev(track$ele, c(dat$start, nrow(track)), dat$size)

      app_env$history <- app_env$history <- c(app_env$history, list(track))
    }

  }, error = function(e) {

    showNotification(paste("Error parsing elevation control:", e))

  })

  length(app_env$history)

}, ignoreNULL = FALSE)
