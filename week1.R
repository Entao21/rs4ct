# ---- packages ----
library(sf)
library(terra)
library(tidyverse)

# ---- 1) load polygons (one shapefile per landcover) ----
# 把这些路径改成你自己的
bare_earth  <- st_read("data/week1/airport_Polygon.shp")  |> vect()
water      <- st_read("data/week1/waterbody_Polygon.shp")       |> vect()
vegetation     <- st_read("data/week1/greenness_Polygon.shp")      |> vect()
urban       <- st_read("data/week1/builtup_area_Polygon.shp")   |> vect()


# ---- 2) load rasters (GeoTIFF exported from SNAP) ----
sentinel <- rast("data/week1/S2C_MSIL2A_20251217T090411_N0511_R007_T35TPF_20251217T130512_resampled_msk_resampled.tif")
landsat  <- rast("data/week1/LC09_L2SP_180031_20250723_20250726_02_T1_msk.tif")

# Sentinel: keep first 6 layers (B2,B3,B4,B8,B11,B12)
sentinel_ref <- sentinel[[1:6]]
# Landsat: keep layers 4-9 (sr_b2..sr_b7), drop QA layers 1-3
landsat_ref  <- landsat[[4:9]]
landsat_ref <- project(landsat_ref, sentinel_ref)
# Band Names
band_levels <- c("blue","green","red","nir","swir1","swir2")
names(sentinel_ref) <- band_levels
names(landsat_ref)  <- band_levels

# ---- 5) function to extract pixel values and reshape to long table ----
band_fun <- function(sensor_rast, land_poly, sensor_name, land_name) {
  
  df <- terra::extract(sensor_rast, land_poly, progress = FALSE) |>
    as_tibble()
  
  df |>
    pivot_longer(
      cols = -1,
      names_to = "band",
      values_to = "value"
    ) |>
    mutate(sensor = sensor_name, land = land_name)
}

# ---- 6) run extraction for each landcover & each sensor ----
sen_urban    <- band_fun(sentinel_ref, urban,      "sentinel", "urban")
sen_concrete <- band_fun(sentinel_ref, bare_earth, "sentinel", "bare_earth")
sen_water    <- band_fun(sentinel_ref, water,      "sentinel", "water")
sen_veg      <- band_fun(sentinel_ref, vegetation, "sentinel", "vegetation")

ls_urban     <- band_fun(landsat_ref,  urban,      "landsat",  "urban")
ls_concrete  <- band_fun(landsat_ref,  bare_earth, "landsat",  "bare_earth")
ls_water     <- band_fun(landsat_ref,  water,      "landsat",  "water")
ls_veg       <- band_fun(landsat_ref,  vegetation, "landsat",  "vegetation")

all_vals <- bind_rows(
  sen_urban, sen_concrete, sen_water, sen_veg,
  ls_urban,  ls_concrete,  ls_water,  ls_veg
)

# ---- 7) summary stats (mean & sd per band, sensor, landcover) ----
means <- all_vals |>
  mutate(band = factor(band, levels = band_levels)) |>
  group_by(band, sensor, land) |>
  summarise(mean = mean(value, na.rm = TRUE),
            sd   = sd(value,   na.rm = TRUE),
            .groups = "drop")

# ---- 8) plot spectral profiles (mean ± sd) ----
p_landsat <- means |>
  filter(sensor == "landsat") |>
  ggplot(aes(x = band, y = mean, colour = land, group = land)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.15) +
  labs(x="Band", y="Reflectance / scaled SR", title="Landsat spectral signatures")

p_sentinel <- means |>
  filter(sensor == "sentinel") |>
  ggplot(aes(x = band, y = mean, colour = land, group = land)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.15) +
  labs(x="Band", y="Reflectance / scaled SR", title="Sentinel-2 spectral signatures")

p_landsat
p_sentinel


# ---- (optional) density plot for one sensor to see variation ----
p_density <- all_vals |>
  filter(sensor == "landsat") |>
  ggplot(aes(x = value, fill = land)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~band, scales = "free") +
  labs(x = "Pixel values", y = "Density", title = "Sentinel pixel value distributions by band & landcover")
p_density
p_density_no_bare <- all_vals |>
  filter(sensor == "landsat", land != "bare_earth") |>
  ggplot(aes(x = value, fill = land)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~band, scales = "free", ncol = 3) +
  labs(x = "Pixel values", y = "Density",
       title = "Sentinel pixel value distributions by band & landcover (excluding bare_earth)")
p_density_no_bare


ggsave("data/week1/land_spec.png", plot = p_landsat,  width = 10, height = 6, dpi = 300)
ggsave("data/week1/sent_spec.png", plot = p_sentinel, width = 10, height = 6, dpi = 300)
ggsave("data/week1/density_landsat.png", plot = p_density, width = 12, height = 8, dpi = 300)
ggsave("data/week1/density_landsat_no_bare.png", plot = p_density_no_bare, width = 12, height = 8, dpi = 300)