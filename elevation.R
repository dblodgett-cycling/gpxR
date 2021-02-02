# Observer for redrawing the elevation plot
observe({

  track <- redraw_listener()

  if(!is.null(track)) {

    output$elevation <- renderPlot(ggplot(track, aes(distance, z)) + geom_point())

  } else {

    output$elevation <- renderPlot(ggplot())

  }

})
