# Häuser nach Preis gefärbt (Abbildung 10)

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("moderndive")

# lade Datensatz
df <- house_prices_sqmt
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Ausschnitt
sites <- data.frame(longitude = df$long, latitude = df$lat)

#####
#####---- Weitsicht
ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, shape = 23, fill = "darkred") + 
  coord_sf(xlim = c(-145.0, 0), ylim = c(0, 90), expand = FALSE)
#####----

# zoom in
ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 1, shape = 23, fill = "darkred") + 
  coord_sf(xlim = c(-122.8, -121.5), ylim = c(47.1, 47.8), expand = FALSE)

#####
# Häuser einfärben
cond <- df$price <= median(house_prices_sqmt$price)
df_1st <- df[cond,]
sites_1st <- data.frame(longitude = df_1st$long, latitude = df_1st$lat)

cond <- df$price > median(house_prices_sqmt$price)
df_2nd <- df[cond,]
sites_2nd <- data.frame(longitude = df_2nd$long, latitude = df_2nd$lat)

ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = sites_1st, aes(x = longitude, y = latitude), size = 1, shape = 23, fill = "blue") + 
  geom_point(data = sites_2nd, aes(x = longitude, y = latitude), size = 1, shape = 23, fill = "red") + 
  coord_sf(xlim = c(-122.8, -121.5), ylim = c(47.1, 47.8), expand = FALSE)
