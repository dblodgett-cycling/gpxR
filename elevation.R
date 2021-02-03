redraw_ele_listener <- reactive({
  watch <- c(recalc_elevation(), new_track(), last_track())
  (tail(app_env$history, n = 1)[[1]])
})

# Observer for redrawing the elevation plot
observe({

  track <- redraw_ele_listener()

  if(!is.null(track)) {

    gg <- ggplot(track, aes(distance, z)) + geom_point()

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

  }, error = function(e) {

    showNotification(paste("Error parsing elevation control:", e))

  })

  return(1)

}, ignoreNULL = FALSE)
