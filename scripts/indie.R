library(mapview)
library(gpxr)

track_f <- "gpx/activity_3842010704.gpx"

track <- load_track_points(track_f)

mapview(track)

start_id <- 895
end_id <- 1034

start_id <- 1034
end_id <- 1157

start_id <- 1158
end_id <- 1287

start_id <- 1289
end_id <- 1416

start_id <- 1417
end_id <- 1417 + 127

sub <- make_loop(track, start_id, end_id, lap_start, control)

mapview(list(sub))

sub <- clean_elev(sub, 2)

plot(sub$track_seg_point_id, sub$ele)

windows <- c(0, 500, 825, 1000, 1300, nrow(sub))
smoothers <- c(5, 20, 5, 10, 1)

sub <- smooth_elev(sub, windows = windows, smoothers = smoothers)

points(sub$track_seg_point_id, sub$ele, col = "red", pch = ".", cex = 3)
abline(v = windows)

plot(st_geometry(sub), pch = ".")
plot(st_geometry(sub)[1], add = TRUE)
plot(st_geometry(sub)[nrow(sub)], add = TRUE)


sub <- select(sub, -power, -gpxtpx_TrackPointExtension)

sub$time <- track$time[1:nrow(sub)]

unlink("2016_WI_State_RR.gpx")

st_write(sub, "2016_WI_State_RR.gpx", "track_points", driver = "GPX")

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
