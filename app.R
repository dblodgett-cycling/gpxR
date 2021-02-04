source("R/gpx_edit.R")
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
                          ele_control = NULL)

##### UI #####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Modify Horizontal Curvature"),
      p("1) Zoom to the corner to edit (hint: hold shift to drag a box)"),
      p(HTML("2) Click the desired start <strong>then</strong> end point for the new curve.")),
      p("3) Click the desired control point of the new curve. (hint: use undo to experiment)"),
      p("4) Click 'Save Point' to recalculate the track with the new curve."),
      p("5) Repeat 1 through 4 until you are happy."),
      p("6) Click 'Save track' to download your modified gpx file."),
      br(),
      p("Inputs for this workflow can be created with tools such as ride with gps."),
      p("Elevation data must be supplied from elsewhere and all added points will have linear interpolated elevations."),
      p("To densify the points, add curves with little to no curvature.")
    ),
    mainPanel(
      leafletOutput("trackmap"),
      p(),
      shiny::actionButton("undobutton", "Undo"),
      shiny::radioButtons("point_id", "Ends or Point",
                          choices = c("Ends", "Point"),
                          inline = TRUE),
      actionButton("savebutton", "Save Point"),
      shiny::verbatimTextOutput("controlpoint")
    )
  ),
  sidebarLayout(sidebarPanel(
    h4("Modify Elevation"),
    p("This section allows selective smoothing of the elevation profile."),
    p("1) Control points will not be modified. Specify their location with a 'start' ID. Specify the degree of smoothing with 'size'."),
    p("2) 'Preview Smooth Elevation' executes the windowed rolling mean and shows the result on the plot."),
    p("3) The complete elevation table can be seen and edited with the 'Edit Elevation Table' button."),
    p("Experiment with different control points and levels of smoothing. Undo goes back the same as above."),
    p("NOTE: Control points are parsed as a two column csv. The first column is a point id which can be found by clicking on the plot. The second column is a number of points to include in the rolling mean."),
    p("NOTE: The rolling mean window size changes from size 1 at control points to a maximum of the size specified.")),
    mainPanel(
    plotOutput("elevation", click = "ele_click"),
    fluidRow(
      column(width = 6,
             p("Clicked Point"),
             htmlOutput("click_info")),
      column(width = 6,
             textAreaInput("elevation_config", "Elevation Control Points",
                           value = "start, size\n1, 1", width = "100%",
                           height = "200px"),
             shiny::actionButton("elevation_button", "Preview Smooth Elevation"),
             shiny::actionButton("undobutton_ele", "Undo"),
             actionButton("ele_table_edit", "Edit Elevation in Table"))
    ))
  ),
  shiny::downloadButton("globalsave", "Save Track")
)
##############

# the modal dialog where the user can enter the query details.
upload_modal <- modalDialog(
  title = "Upload GPX",
  p("This app is designed to reflow corners of a gpx to ensure they are not too sharp for RGT."),
  p("A second module of the app allows editing and smoothing of elevation data."),
  p("This application is free and open source and is being provided as a service to the community."),
  p(HTML('See <a href="https://github.com/dblodgett-cycling/gpxR" target="_blank">the github repository</a> for more.')),
  br(),
  p("The GPX should contain no overlaps and be point to point or form a loop"),
  fileInput("f", "upload", multiple = FALSE, accept = ".gpx"),
  easyClose = F,
  footer = actionButton("dismiss_modal", label = "Done")
)

server <- function(input, output, session) {

  # Contains observers and reactives for track corner smoothing.
  source("smoothing.R", local = TRUE)

  # contains observers and reactives for elevation cleaning
  source("elevation.R", local = TRUE)

  # Show the model on start up ...
  showModal(upload_modal)

  # I think this will work?
  redraw_listener <- reactive({
    watch <- list(last_track(), new_track())
    (tail(app_env$history, n = 1)[[1]])
  })

  observeEvent(input$dismiss_modal, {
    if(is.null(input$f)) {
      showNotification("Upload gpx file first.")
      return()
    }

    removeModal()

    app_env$f <- input$f

    track <- load_track_points(input$f$datapath)

    track <- simplify_track(track, 0)

    bb <- as.list(sf::st_bbox(track))
    names(bb) <- c("west", "south", "east", "north")

    app_env$zoom <- bb

    track <- sf::st_transform(track, 5070)

    track <- add_distance(track)

    app_env$history <- list(track)

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

  write_out <- function(f) {

    track <- tail(app_env$history, n = 1)[[1]] %>%
      select(track_seg_point_id, ele) %>%
      mutate(track_seg_id = 0, track_fid = 0, time = "") %>%
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
