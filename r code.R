# Load required libraries
pacman::p_load(
  dodgr, sf, tidygraph, osmplotr, magrittr, igraph, dplyr, 
  tibble, ggplot2, units, tmap, osmdata, leaflet, readxl, rio, here
)

# Load and preprocess London data
london <- read_excel("data/msoa-data.xls")
england <- read.csv("data/MSOA_Dec_2011_PWC_in_England_and_Wales_2022_-7657754233007660732.csv")
england.2 <- read_excel("data/MLSOA_Dec_2021_PWC_in_England_and_Wales_2022_-3559472851201324412.xlsx")

# Harmonise column names
colnames(england)[2] <- "MSOA Code"
colnames(england.2)[2] <- "MSOA Code"

england.3 <- left_join(england, england.2, by = "MSOA Code") %>%
  select(c("MSOA Code", "MSOA11NM", "x.y", "y.y"))

# Filter London-specific data
london <- london %>% select("MSOA Code")
london.city <- left_join(london, england.3, by = "MSOA Code")
london.city <- left_join(london, england, by = "MSOA Code")

# Load additional data 
rac <- read.csv("data/OD_Table.csv")
colnames(rac)[1] <- "MSOA Code"
tes <- left_join(rac, london.city, by = "MSOA Code")
test <- tes[1:30, ]

# Transform to {sf} data structure
dt <- st_as_sf(test, coords = c("longitude", "latitude"), crs = 4326)
class(dt)

# Plot data on a Leaflet map
leaflet() %>% addTiles() %>% addMarkers(data = dt)

# Route mapping with osrm
osroute <- osrm::osrmRoute(loc = dt)

leaflet(data = dt) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(data = osroute,
               label = "OSRM engine",
               color = "red")

# Shortest paths using dodgr package
net <- dodgr_streetnet("city of london uk") # create net 
class(net)

net.1 <- net[, c("osm_id", "geometry")] 
head(net.1, 3)

# Visualise the network using tmap
tm_shape(net.1) + 
  tm_layout(legend.outside=TRUE,bg='black') + 
  tm_lines("osm_id",lwd=1.5)

# Generate a base map with osmplotr, magrittr
map <- osm_basemap(net, bg = "gray95") %>%   
  add_osm_objects(net, col = "gray5") %>% 
  add_axes() %>% 
  print_osm_map()

# Create weighted graph and calculate shortest paths
graph <- weight_streetnet(net, wt_profile = "motorcar") 
from <- sample(graph$from_id, size = 20)
to <- sample(graph$to_id, size = 5)

dp <- dodgr_paths(graph, from = from, to = to)  
dp [[1]] [[1]]
verts <- dodgr_vertices (graph)
path1 <- verts [match (dp [[1]] [[1]], verts$id), ]
head (path1)

# map
dt <- st_as_sf(path1, coords = c("x", "y"), crs = 4326)
dt <- dt[1:100, ]
leaflet() %>% addTiles() %>% addMarkers(data = dt)

# Copenhagen: Bounding box and random points
bb <- osmdata::getbb("Copenhagen")
npts <- 10
xy <- apply(bb, 1, function(i) min(i) + runif(npts)*diff(i))

head(xy, 10)

# Subset network for visualisation
net <- dodgr_streetnet(bb) # download street network 
trans_net <- weight_streetnet(net, wt_profile = "foot")
head(trans_net, 3)
d <- dodgr_dists(trans_net, from = xy, to = xy)

# Creat a small net 
bbox <- matrix(c(12.5926,55.6612,12.6325,55.6728),ncol=2)
colnames(bbox) <- c("min","max")
rownames(bbox) <- c("x","y")

small_net <- dodgr_streetnet(bbox)
temp <- small_net[, c("osm_id", "geometry")]

# Visualise small network
tm_shape(temp) + 
  tm_layout(legend.outside = TRUE, bg = "black") +
  tm_lines("osm_id", lwd = 1.5)
