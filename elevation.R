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
      gg <- gg + geom_vline(xintercept = app_env$ele_control)
    }

    output$elevation <- renderPlot(gg)

  } else {

    output$elevation <- renderPlot(ggplot())

  }

})

recalc_elevation <- eventReactive(input$elevation_button, {
  message("yep")
}, ignoreNULL = FALSE)
