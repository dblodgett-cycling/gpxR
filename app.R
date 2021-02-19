library(gpxr)
library(dplyr)
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(DT)
# library(Cairo)

gt <- tempfile(fileext = ".geojson")

app_env <- reactiveValues(cp = list(),
                          df = data.frame(),
                          le = 0,
                          ends = TRUE,
                          trackmap_geojson_click = NULL,
                          track_temp = NULL,
                          history = NULL,
                          zoom = list(west = -180, south = -180,
                                      east = 180, north = 180),
                          f = NULL,
                          ele_control = NULL,
                          proj = NULL)

source("ui.R")

server <- function(input, output, session) {

  # Contains observers and reactives for track corner smoothing.
  source("smoothing.R", local = TRUE)

  # contains observers and reactives for elevation cleaning
  source("elevation.R", local = TRUE)

  # Show the model on start up ...
  showModal(upload_modal)

  redraw_listener <- reactive({
    watch <- list(last_track(), new_track())
    (tail(app_env$history, n = 1)[[1]])
  })

  ### Initial action to dismiss the modal upload
  observeEvent(input$dismiss_modal, {

    if(is.null(input$f)) {
      showNotification("Upload gpx file first.",
                       duration = NULL, type = "warning")
      return()
    }

    removeModal()

    app_env$f <- input$f

    track <- load_track_points(input$f$datapath)

    app_env$proj <- get_utm(sf::st_coordinates(track[1, ]))

    track <- simplify_track(track, 0)

    bb <- as.list(sf::st_bbox(track))
    names(bb) <- c("west", "south", "east", "north")

    app_env$zoom <- bb

    track <- sf::st_transform(track, app_env$proj)

    track <- add_distance(track, app_env$proj)

    app_env$history <- list(track)

  }, ignoreNULL = TRUE)

  write_out <- function(f) {

    track <- tail(app_env$history, n = 1)[[1]]

    write_track_points(track, f)

  }

  # Reactive triggered by final save button
  output$globalsave <- downloadHandler(filename = "gpxr_out.gpx",
                                       content = write_out,
                                       contentType = "text/xml")

}

shinyApp(ui, server)
