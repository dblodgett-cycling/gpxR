track_f <- "Downloads/The_Irish_are_super_keen_cyclists_.gpx"

track <- load_track_points(track_f)

mapview(track)

start_id <- 4414
end_id <- 15988
lap_start <- NULL

control <- NULL
control_point <- sf::st_sfc(sf::st_point(control), crs = st_crs(4326))

sub <- make_loop(track, start_id, end_id, lap_start, control)

windows <- c(0, 1250, 1700, 4000, 7000, nrow(sub))
smoothers <- c(20, 80, 2, 40, 30)

plot(sub$track_seg_point_id, sub$ele)

sub <- clean_elev(sub, 4)

sub <- smooth_elev(sub, windows = windows, smoothers = smoothers)

points(sub$track_seg_point_id, sub$ele, col = "red", pch = ".", cex = 3)
abline(v = windows)

cut_one <- 5418
cut_two <- 5779

sub <- cut_route(sub, cut_one, cut_two)

plot(st_geometry(sub), pch = ".")
plot(st_geometry(sub)[1], add = TRUE)
plot(st_geometry(sub)[nrow(sub)], add = TRUE)

mapview(list(sub))

sub_diff <- st_drop_geometry(bind_cols(sub, as.data.frame(st_coordinates(st_transform(sub, 5070)))))

sub_diff <- select(sub_diff, x = X, y = Y, z = ele, track_seg_point_id)

sub_diff <- mutate(sub_diff, diff = sqrt((x - lag(x))^2 + 
                                           (y - lag(y))^2 + 
                                           (z - lag(z))^2),
                   ele_diff = z-lag(z))

sub_diff$diff[1] <- 0
sub_diff$ele_diff[1] <- 0

sub_diff$slope <- sub_diff$ele_diff / sub_diff$diff

sub_diff$slope[1] <- sub_diff$slope[2]

sub_diff$distance <- cumsum(sub_diff$diff)

plot(sub_diff$distance, sub_diff$z)
plot(sub_diff$track_seg_point_id, sub_diff$slope)

sub_diff <- mutate(sub_diff, slope_change = slope - lag(slope))

sub <- filter(sub, sub_diff$slope > -.2 & sub_diff$slope < .2)

sub <- select(sub, -power, -gpxtpx_TrackPointExtension)

sub$time <- track$time[1:nrow(sub)]

sub <- mutate(sub, track_fid = 0, track_seg_id = 0, 
       track_seg_point_id = seq_len(nrow(sub)))

unlink("wiklow.gpx")

st_write(sub, "wiklow.gpx", "track_points", driver = "GPX")

