library(gpxr)
library(dplyr)
library(shiny)
library(leaflet)

f <- "gpx/RGT_Benton_Park_Classic_(copy).gpx"

track <- load_track_points(f)

track <- simplify_track(track, 0)

bb <- as.list(sf::st_bbox(track))
names(bb) <- c("west", "south", "east", "north")

track <- sf::st_transform(track, 5070)

gt <- tempfile(fileext = ".geojson")

app_env <- reactiveValues(cp = list(),
                          df = data.frame(),
                          le = 0,
                          ends = TRUE,
                          trackmap_geojson_click = NULL,
                          track = track,
                          zoom = bb)

##### UI #####
ui <- fluidPage(
  leafletOutput("trackmap"),
  p(),
  shiny::radioButtons("point_id", "Ends or Point",
                      choices = c("Ends", "Point"),
                      inline = TRUE),
  actionButton("savebutton", "Save Point"),
  shiny::verbatimTextOutput("controlpoint"),
  # shiny::dataTableOutput("cpdf"),
  shiny::actionButton("globalsave", "Save Table")
)
##############


server <- function(input, output, session) {

  # Observer for redrawing the map
  observe({

    sf::write_sf(buffer_track(new_track()), gt)

    gj <- jsonlite::fromJSON(gt, simplifyVector = FALSE)

    unlink(gt)

    bb <- app_env$zoom

    message(str(bb))

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
  source("smoothing.R")

  # contains observers and reactives for elevation cleaning
  source("elevation.R")

  # Reactive triggered by final save button
  save_df <- eventReactive(input$globalsave, {
    app_env$df
  })

  # observer to write reactive function back to global env.
  observe({
    output_control_point <<- save_df()
    stopApp()
  })
}

shinyApp(ui, server)
