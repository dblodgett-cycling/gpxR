library(gpxr)
library(dplyr)
library(shiny)
library(leaflet)
library(sf)

gt <- tempfile(fileext = ".geojson")

app_env <- reactiveValues(cp = list(),
                          df = data.frame(),
                          le = 0,
                          ends = TRUE,
                          trackmap_geojson_click = NULL,
                          track = NULL,
                          history = NULL,
                          zoom = list(west = -180, south = -180,
                                      east = 180, north = 180),
                          f = NULL)

##### UI #####
ui <- fluidPage(
  leafletOutput("trackmap"),
  p(),
  shiny::actionButton("undobutton", "Undo"),
  shiny::radioButtons("point_id", "Ends or Point",
                      choices = c("Ends", "Point"),
                      inline = TRUE),
  actionButton("savebutton", "Save Point"),
  shiny::verbatimTextOutput("controlpoint"),
  # shiny::dataTableOutput("cpdf"),
  shiny::downloadButton("globalsave", "Save Track")
)
##############

# the modal dialog where the user can enter the query details.
upload_modal <- modalDialog(
  title = "Upload GPX",
  fileInput("f", "upload", multiple = FALSE, accept = ".gpx"),
  easyClose = F,
  footer = actionButton("dismiss_modal", label = "Dismiss")
)

server <- function(input, output, session) {

  # Show the model on start up ...
  showModal(upload_modal)

  # I think this will work?
  redraw_listener <- reactive({
    watch <- list(last_track(), new_track())
    (tail(app_env$history, n = 1)[[1]])
  })


  first_track <- reactive({
    f <- app_env$f

    if(is.null(f)) return()

    length(f)
  })

  observeEvent(input$dismiss_modal, {
    removeModal()

    app_env$f <- input$f

    track <- load_track_points(input$f$datapath)

    track <- simplify_track(track, 0)

    bb <- as.list(sf::st_bbox(track))
    names(bb) <- c("west", "south", "east", "north")

    app_env$zoom <- bb

    track <- sf::st_transform(track, 5070)

    app_env$history <- list(track)

    app_env$track <- track
  }, ignoreNULL = TRUE)

  # Observer for redrawing the map
  observe({

    track <- redraw_listener()

    if(!is.null(track)) {

      sf::write_sf(buffer_track(track), gt)

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
        addGeoJSON(gj,
                   weight = 1.5, color = "black") %>%
        fitBounds(bb$west, bb$north, bb$east, bb$south)
    })

  })

  # Contains observers and reactives for track corner smoothing.
  source("smoothing.R", local = TRUE)

  # contains observers and reactives for elevation cleaning
  source("elevation.R", local = TRUE)

  write_out <- function(f) {

    track <- tail(app_env$history, n = 1)[[1]] %>%
      mutate(ele = track$ele, track_seg_id = 0, track_fid = 0, time = "")%>%
      sf::st_transform(4326)

    unlink(f)

    sf::write_sf(track, f, "track_points", driver = "GPX")
  }

  # Reactive triggered by final save button
  output$globalsave <- downloadHandler(filename = "gpxr_out.gpx",
                                       content = write_out,
                                       contentType = "text/xml")

}

shinyApp(ui, server)
