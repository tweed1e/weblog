library(sf)
# https://r-spatial.github.io/sf/articles/sf1.html
nc <- st_read(system.file("shape/nc.shp", package="sf"))

(nc_geom <- st_geometry(nc))

# par(mar = c(0,0,1,0))
plot(nc[2])
plot(nc[1,1], col = 'grey', add = TRUE)

par(mar = c(0,0,1,0))
(w <- which(sapply(nc_geom, length) > 1))
## [1]  4 56 57 87 91 95
plot(nc[w,1], col = 2:7)
