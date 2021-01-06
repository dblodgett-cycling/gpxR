#' load track points
#' @param track_f character path to track gpx file
#' @importFrom sf read_sf
#' @export
load_track_points <- function(track_f) {
  read_sf(track_f, "track_points")
}

#' make loop
#' @param track sf data.frame gpx track
#' @param start_id numeric track_seg_point_id of start
#' @param end_id numeric track_seg_point_id of end
#' @param lap_start track_seg_point_id of desired start --
#' NULL for no adjustment.
#' @param control numeric c(lon, lat)
#' @param correct_elevation logical should elevation of start be forced
#' to match elevation of end?
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom sf st_coordinates st_crs
#' @importFrom bezier bezier
#' @export
make_loop <- function(track, start_id, end_id,
                      lap_start = NULL,
                      control = NULL,
                      correct_elevation = FALSE) {

  sub <- filter(track, track_seg_point_id > start_id &
                  track_seg_point_id < end_id)

  if(correct_elevation) {
    correction <- (sub$ele[1] - sub$ele[nrow(sub)]) / nrow(sub)

    correction <- cumsum(rep(correction, nrow(sub)))

    sub$ele <- sub$ele + correction
  }

  if(!is.null(control)) {
    start_point <- st_coordinates(sub[1, ])
    end_point <- st_coordinates(sub[nrow(sub), ])

    control <- matrix(control, nrow = 1)

    new_points <- bezier(t = seq(0, 1, length = 10), p = control,
                         start = end_point, end = start_point)

    new_points <- as.data.frame(new_points)
    names(new_points) <- c("X", "Y")

    new_points <- st_as_sf(new_points, coords = c("X", "Y"), crs = st_crs(sub))

    new_points$ele <- seq(from = sub$ele[nrow(sub)], to = sub$ele[1], length.out = 10)

    sub <- bind_rows(sub, new_points)
  }

  if(!is.null(lap_start)) {
    row_start <- which(sub$track_seg_point_id == lap_start)

    sub <- bind_rows(sub[row_start:nrow(sub), ],
                     sub[1:(row_start-1), ])
  }

  mutate(sub, track_fid = 0, track_seg_id = 0,
         track_seg_point_id = seq_len(nrow(sub)))

}

#' bezier smooth
#' @description replaces the track points between start and end points
#' with a bezier curve.
#' @param start_id integer start track_seg_point_id
#' @param end_id integer end track_seg_point_id
#' @param control numeric of length 2 or equivalent that can be coerced to matrix.
#' must be in same projection as track.
#' @param n_points integer number of points to create along the new curve.
#' @param reset_ids logical if FALSE, track ids will not be reset and new points
#' will not have ids.
#' @export
#' @inheritParams make_loop
#'
bez_smooth <- function(track, start_id, end_id, control, n_points = 10, reset_ids = FALSE) {

  control <- matrix(control, nrow = 1)

  start_row <- which(track$track_seg_point_id == start_id)
  end_row <- which(track$track_seg_point_id == end_id)

  start_point <- sf::st_coordinates(track[start_row, ])[, 1:2]
  end_point <- sf::st_coordinates(track[end_row, ])[, 1:2]

  new_points <- bezier::bezier(t = seq(0, 1, length = n_points), p = control,
                       start = start_point, end = end_point)

  new_points <- as.data.frame(new_points)
  names(new_points) <- c("X", "Y")

  new_points <- st_as_sf(new_points, coords = c("X", "Y"), crs = st_crs(track))

  new_points$ele <- seq(from = track$ele[start_row], to = track$ele[end_row], length.out = 10)

  out <- bind_rows(track[1:(start_row - 1), ], new_points, track[(end_row + 1):nrow(track), ])

  if(reset_ids) {
    out$track_seg_point_id <- 1:nrow(out)
  }

  out
}

#' Clean Elevation
#' @description executes a sliding mean over the specified window.
#' @inheritParams make_loop
#' @param window integer number of points to average over.
#' @export
clean_elev <- function(track, window = 4) {

  smooth_elev(track, c(1, nrow(track)), window)

}

#' smooth elev
#' @description Applies a variable size roling mean over
#' any number of sub domains.
#' @inheritParams make_loop
#' @param domains integer vector specifying breaks in elevation averaging.
#' @param windows integer vector specifying desired size of roling mean for
#' each domain. Should be length(domains) - 1 long.
#' @export
smooth_elev <- function(track, domains, windows) {

  if((length(domains) - 1) != length(windows))
    stop("windows must be one short than domains")

  e <- track$ele

  w_i <- 1
  w_min_i <- 1
  w_max_i <- 2

  s_o <- 1

  for(i in 1:length(e)) {
    s <- windows[w_i]

    w_min <- domains[w_min_i]
    w_max <- domains[w_max_i]

    e[i] <- mean(e[(i - (s_o - 1)):(i + (s_o - 1))])

    if((w_max - s_o) < i) {
      # increment s_o down
      s_o <- s_o - 1
    } else if(s_o < s) {
      # increment s_o up
      s_o <- s_o + 1
    }

    if(i == w_max) {
      w_i <- w_i + 1
      w_min_i <- w_min_i + 1
      w_max_i <- w_max_i + 1
    }

  }

  track$ele <- e

  track
}

#' cut track
#' @description cuts a gpx track at two specified points
#' @inheritParams make_loop
#' @param cut_one integer track_seg_point_id for start of cut
#' @param cut_two integer track_seg_point_id for end of cut
cut_track <- function(track, cut_one, cut_two) {
  cut_one <- which(track$track_seg_point_id == cut_one)
  cut_two <- which(track$track_seg_point_id == cut_two)

  rbind(track[1:cut_one, ], track[cut_two:nrow(track), ])
}

#' segmentize track
#' @description break track into segments
#' @inheritParams make_loop
#' @param l numeric maximum segment length in units of data projection
#' @export
segmentize_track <- function(track, l) {
  crs <- sf::st_crs(track)

  track <- sf::st_segmentize(to_line(track), l)

  to_points(track, crs)
}

#' simplify track
#' @description convert to line and simplify
#' @inheritParams make_loop
#' @param tolerance numeric passed to dTolerance
#' @export
simplify_track <- function(track, tolerance) {
  crs <- sf::st_crs(track)

  track <- sf::st_simplify(to_line(track), dTolerance = tolerance)

  to_points(track, crs)
}

to_line <- function(track) {
  track <- cbind(sf::st_coordinates(track), track$ele)

  sf::st_linestring(track, dim = "XYZ")
}

to_points <- function(track, crs) {
  track <- sf::st_coordinates(track)

  geo <- apply(track[, 1:2], 1, sf::st_point)

  sf::st_as_sf(
    data.frame(track_seg_point_id = seq_len(nrow(track)),
               ele = track[, 3], X = track[, 1], Y = track[, 2]),
    coords = c("X", "Y"), crs = crs)
}

#' Add distance
#' @inheritParams make_loop
#' @export
add_distance <- function(track) {
  track <- st_drop_geometry(bind_cols(track,
                                      as.data.frame(st_coordinates(st_transform(track, 5070)))))

  track <- select(track, x = X, y = Y, z = ele)

  track <- mutate(track, diff = sqrt((x - lag(x))^2 +
                                   (y - lag(y))^2 +
                                   (z - lag(z))^2),
                ele_diff = z-lag(z))

  track$diff[1] <- 0
  track$ele_diff[1] <- 0

  track$slope <- track$ele_diff / track$diff

  track$slope[1] <- track$slope[2]

  track$distance <- cumsum(track$diff)

  track
}

#' buffer track
#' @inheritParams make_loop
#' @param proj input to sf::st_crs to buffer points in
#' @export
buffer_track <- function(track, proj = 5070) {
  sf::st_transform(
    sf::st_buffer(
      sf::st_transform(track, proj),
      dist = 2, nQuadSegs = 2), 4326)
}
