library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(extrafont)

water <- st_read("F:\\30DayMapChallenge\\Day14\\gis_osm_waterways_free_1.shp")
water2 <- st_read("F:\\30DayMapChallenge\\Day14\\gis_osm_water_a_free_1.shp")
armenia <- st_read("F:\\30DayMapChallenge\\Day14\\gadm41_ARM_0.shp")

water   <- st_transform(water, st_crs(armenia))
water2  <- st_transform(water2, st_crs(armenia))

water_arm  <- st_intersection(water, armenia)
water2_arm <- st_intersection(water2, armenia)

water_arm$fclass_lbl  <- tools::toTitleCase(water_arm$fclass)
water2_arm$fclass_lbl <- tools::toTitleCase(water2_arm$fclass)
water2_arm_water <- water2_arm %>% filter(fclass_lbl == "Water")

colors1 <- c(
  "River"  = "#00EEEE",
  "Stream" = "#FF4040",
  "Canal"  = "#FFD700",
  "Drain"  = "#00CD00"
)
sizes1 <- c(
  "River"  = 0.5,
  "Stream" = 0.5,
  "Canal"  = 1.0,
  "Drain"  = 1.0
)

map <- ggplot() +
  geom_sf(data = armenia, fill = "black", color = "#7F7F7F", linewidth = 1.5) +
  geom_sf(data = water2_arm_water, fill = "#1C86EE", color = NA, alpha = 0.7, show.legend = FALSE) +
  geom_sf(data = water_arm, aes(color = fclass_lbl, size = fclass_lbl)) +
  scale_color_manual(values = colors1, name = "Waterway Type") +
  scale_size_manual(values = sizes1, name = "Waterway Type") +

  annotation_north_arrow(
    location = "tr",
    height = unit(2, "cm"),
    width  = unit(2, "cm"),
    style = north_arrow_fancy_orienteering(fill = "white", line_col = "white")
  ) +
  
  labs(title = "Waterways in Armenia") +
  
  theme(
    panel.background = element_rect(fill = "black", color = NA),
    plot.background  = element_rect(fill = "black", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    
    legend.key = element_rect(fill = "black", color = "black"),
    legend.background = element_rect(fill = "black"),
    
    legend.text = element_text(color = "white", family = "Times New Roman", face = "bold"),
    legend.title = element_text(color = "white", size = 14, family = "Times New Roman", face = "bold"),
    
    plot.title = element_text(
      family = "Times New Roman",
      face = "bold",
      size = 20,
      color = "white",
      hjust = 0.5
    )
  )

ggsave(
  "Armania.png",
  map,
  width = 26,
  height = 24,
  units = "cm",
  bg = "black",
  device = "png"
)
