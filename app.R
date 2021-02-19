library(dplyr)
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(DT)
library(gpxr)
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

##### UI #####
ui <- fluidPage(
  h2("gpxR - GPX editing tools for RGT Magic Roads"),
  p("This application is broken into two main parts: geospatial modifications and elevation modifications."),
  p("Each action can be executed and undone in a trial and error mode."),
  p("Saving your work by periodically downloading the track is encouraged."),
  p("Inputs can be drawn with a route building tool or pulled from raw GPS."),
  shiny::selectInput("mode", "Geospatial Modification Mode", c("Modify Horizontal Curvature",
                                                               "Make Loop",
                                                               "Resample Track",
                                                               "Simplify Track"),
                     selected = "Modify Horizontal Curvature"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.mode=='Modify Horizontal Curvature'",
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
      conditionalPanel(condition = "input.mode=='Make Loop'",
                       h4("Make Loop"),
                       p("1) Zoom to location you want to insert a loop."),
                       p("2) Click the start and end points of the loop, order isn't important."),
                       p("3) Optionally, enter a point ID to use as the loop start. Find this by clicking the map or elevation profile. Leaving this blank will use the start point of the loop."),
                       p("4) If the ends should not be connected with a curve, uncheck the box.")
      ),
      conditionalPanel(condition = "input.mode=='Resample Track'",
                       h4("Resample Track"),
                       p("Enter a desired distance between points in meters."),
                       p("Linear interpolation is used to resample the track.")),
      conditionalPanel(condition = "input.mode=='Simplify Track'",
                       h4("Simplify Track"),
                       p("Enter a desired simplification tolerance in meters."),
                       p("This tolerance is the maximum deviation of the route for a point to be removed."))
    ),
    mainPanel(
      leafletOutput("trackmap"),
      p(),
      conditionalPanel(condition = "input.mode=='Modify Horizontal Curvature' || input.mode=='Make Loop'",
                       shiny::radioButtons("point_id", "Ends or Point",
                                           choices = c("Ends", "Point"),
                                           inline = TRUE),
                       shiny::verbatimTextOutput("controlpoint")
      ),
      conditionalPanel(condition = "input.mode=='Make Loop'",
                       numericInput("loop_start_id", "Loop Start ID",
                                    value = NULL, min = 1, step = 1),
                       checkboxInput("loop_curve_option", "Add Connecting Curve",
                                     value = TRUE),
                       checkboxInput("correct_elevation_option", "Correct start - end elevation.",
                                     value = TRUE)
      ),
      conditionalPanel(condition = "input.mode=='Resample Track'",
                       p("Resample Track"),
                       numericInput("resample_tolerance",
                                    label = "Resample Tolerance",
                                    value = NULL, min = 1, max = 10, step = 1)
      ),
      conditionalPanel(condition = "input.mode=='Simplify Track'",
                       p("Simplify Track"),
                       numericInput("simplify_tolerance", "Simplification Tolerance",
                                    value = 0.1, min = 0.001, max = .5)),
      actionButton("savebutton", "Go Do It!"),
      shiny::actionButton("undobutton", "Undo")
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
  title = h2("gpxR - GPX editing tools for RGT Magic Roads"),
  p("This app is designed to reflow corners, create looping GPX routes, and generally clean up geospatial tracks to ensure they are suitable for RGT Magic Roads."),
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
