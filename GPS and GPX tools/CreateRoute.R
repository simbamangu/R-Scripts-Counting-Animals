### Create a 'snaking' route for transect flying
### 2016-11-04 (C) Howard Frederick

# Load a set of waypoints with E/W ends, e.g. C01E, C01W, C02E, etc.
wpts <- readOGR("flightplanning/su_C.gpx", layer = "waypoints", stringsAsFactors = F)

# restrict to only the ends
wpts <- wpts[nchar(wpts$name) == 4,] 

# Set up the route IDs
#  - route_fid = ID for each route
#  - route_point_id = the ID _along_ each route.
wpts$route_point_id <- wpts$route_fid <- 0

# Set up two temp vectors to do the ordering
ord1=rep(c(1:2, 2:1), length.out = length(wpts))
ord2 = rep(1:(length(wpts)/2), each = 2)

# Re-order
wpts <- wpts[order(ord2, ord1),]

# Set route_point_id (only one route for now)
wpts$route_point_id <- 1:NROW(wpts)
writeOGR(wpts, dsn = "flightplanning/testOGRrte.gpx", layer = "route_points", driver = "GPX")

