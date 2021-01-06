track_f <- "Downloads/To_and_race_Fourth_.gpx"

track <- load_track_points(track_f)

mapview(track)

start_id <- 7150
end_id <- 11259
lap_start <- NULL

control <- NULL
control_point <- sf::st_sfc(sf::st_point(control), crs = st_crs(4326))

sub <- make_loop(track, start_id, end_id, lap_start, control)

windows <- c(0, 550, 1050, 1500, 1700, 2000, 2500, 2900, 3150, nrow(sub))
smoothers <- c(40, 2, 30, 2, 20, 1, 20, 2, 30)

plot(sub$track_seg_point_id, sub$ele)

sub <- clean_elev(sub, 2)

sub <- smooth_elev(sub, windows = windows, smoothers = smoothers)

points(sub$track_seg_point_id, sub$ele, col = "red", pch = ".", cex = 3)
abline(v = windows)

plot(st_geometry(sub), pch = ".")
plot(st_geometry(sub)[1], add = TRUE)
plot(st_geometry(sub)[nrow(sub)], add = TRUE)

mapview(list(sub))

sub <- select(sub, -power, -gpxtpx_TrackPointExtension)

sub$time <- track$time[1:nrow(sub)]

unlink("bmc.gpx")

st_write(sub, "bmc.gpx", "track_points", driver = "GPX")
