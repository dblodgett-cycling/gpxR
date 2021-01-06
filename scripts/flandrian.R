
track_f <- "Downloads/Felt_awful_rode_anyways_.gpx"

track <- load_track_points(track_f)

mapview(track)

start_id <- 1698
end_id <- 3094
lap_start <- 3079

control <- c(-89.19, 43.065)
control_point <- sf::st_sfc(sf::st_point(control), crs = st_crs(4326))

windows <- c(0, 125, 290, 510, 610, 1010, 1225, nrow(sub))
smoothers <- c(1, 20, 5, 20, 1, 20, 6)

sub <- make_loop(track, start_id, end_id, lap_start, control)

plot(sub$track_seg_point_id, sub$ele)

sub <- clean_elev(sub, 2)

sub <- smooth_elev(sub, windows = windows, smoothers = smoothers)

points(sub$track_seg_point_id, sub$ele, col = "red", pch = ".", cex = 3)
abline(v = windows)

plot(st_geometry(sub), pch = ".")
plot(st_geometry(sub)[1], add = TRUE)
plot(st_geometry(sub)[nrow(sub)], add = TRUE)

mapview(list(sub, control_point))

sub <- select(sub, -power, -gpxtpx_TrackPointExtension)

sub$time <- track$time[1:nrow(sub)]

unlink("flandrian.gpx")

st_write(sub, "flandrian.gpx", "track_points", driver = "GPX")

sub <- st_drop_geometry(bind_cols(sub, as.data.frame(st_coordinates(st_transform(sub, 5070)))))

sub <- select(sub, x = X, y = Y, z = ele)

sub <- mutate(sub, diff = sqrt((x - lag(x))^2 + 
                                 (y - lag(y))^2 + 
                                 (z - lag(z))^2),
              ele_diff = z-lag(z))

sub$diff[1] <- 0
sub$ele_diff[1] <- 0

sub$slope <- sub$ele_diff / sub$diff

sub$slope[1] <- sub$slope[2]

sub$distance <- cumsum(sub$diff)

plot(sub$distance, sub$z)
plot(sub$distance, sub$slope)

sub <- mutate(sub, slope_change = slope - lag(slope))
