################################################################################
##  Fig2_CentralValley.R
##
##  Adaptation of Fig. 2 from Serra et al., "Global analysis of nitrogen inputs
##  from irrigation water to cropland" (Nature Sustainability, 2026),
##  re-centered on California's Central Valley.
##
##  FIGURE A  (the "zoom cascade" you asked for)
##    a  Global N_Irrig over irrigated cropland (as published Fig. 2a),
##       with California boxed
##    b  California, with the Central Valley boundary drawn
##    c  Central Valley focal map at native 100 km2 resolution
##
##  FIGURE B  (everything else the data supports for the region)
##    d  Crop-specific N_Irrig across the Central Valley
##    e  Total N_Irrig (Gg N/yr) and irrigated area by crop
##    f  Distribution of N_Irrig per hectare: World / USA / California / CV
##    g  County-level totals and intensities
##    h  N_Irrig dependency (share of total cropland N inputs), if available
##    i  Compiled field observations in and around the region
##
##  Run this from the SAME working directory as the original Fig1-3 scripts,
##  i.e. the one containing ./Output/Predictions/ and ./Data/masks/.
################################################################################


# ---------------------------------------------------------------------------- #
# 0. SETUP
# ---------------------------------------------------------------------------- #

## Attached:  terra sf ggplot2 dplyr patchwork rworldmap rnaturalearth
## Called with ::  ggthemes stringr forcats readxl exactextractr tigris rworldxtra
##   - tigris supplies both US states and California counties
##   - rworldxtra is needed for the high-resolution world basemap
##   - exactextractr is optional; terra::extract is used if it is missing
##   - readxl is only needed for panel (i)
##   - rnaturalearthhires is NOT needed

library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)
library(rworldmap)
library(rnaturalearth)

sf_use_s2(FALSE)   # county/state polygons behave better with s2 off

## ---- 0.1 paths -------------------------------------------------------------
# setwd('c:/Users/jserr/OneDrive/Modelling/Meta_irrigation/')   # <- as in Fig3.R

PRED_DIR   <- './Output/Predictions/'          # site_nirr_<crop>.tif
MASK_DIR   <- './Data/masks/'                  # crop irrigated area (ha per cell)

# The figshare deposit ships nirr_ha and nirr_kgN but not the MapSPAM masks.
# Since the pipeline computed nirr_kgN = nirr_ha * area, area is recoverable as
# nirr_kgN / nirr_ha, already carrying the >=5 ha filter. Leave this TRUE unless
# you have obtained the original masks and put them in MASK_DIR.
DERIVE_MASKS <- TRUE
OUT_DIR    <- './Output/Plots_CentralValley/'  # created below
DATASET    <- './Data/dataset.xlsx'            # the compiled field observations
CV_SHP     <- NA                               # optional: path to a Central Valley
                                               # polygon (e.g. the USGS Central
                                               # Valley alluvial-boundary shapefile).
                                               # Leave NA to build it from counties.

# Optional predictor layer used for the N_Irrig dependency panel (h).
# In the ML pipeline this is the total cropland N input predictor. Point this at
# whichever file holds it; the panel is skipped silently if it is not found.
NIN_TIF    <- NA   # e.g. './Data/predictors/Tot_Nin.tif'

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## ---- 0.2 crops -------------------------------------------------------------
# Same 15 crops used to build published Fig. 2a.
crop_names <- c('maize','vegetables','rice','wheat','sugarcane','barley','potato',
                'beans','chickpea','cotton','groundnut','rapeseed','sugarbeet',
                'sunflower','soyabean')

# Crops shown individually in panel d. Anything with no irrigated area in the
# Central Valley is dropped automatically further down.
cv_focus_crops <- c('maize','vegetables','rice','wheat','cotton','sugarbeet',
                    'potato','beans','sunflower','barley')

## ---- 0.3 region definition -------------------------------------------------
# Standard 19-county definition of the Central Valley (Sacramento Valley +
# San Joaquin Valley). Predictions are masked to irrigated cropland anyway, so
# the foothill fringes of these counties contribute nothing.
cv_counties <- c(
  # Sacramento Valley
  'Shasta','Tehama','Glenn','Butte','Colusa','Sutter','Yuba','Yolo','Placer',
  'Sacramento','Solano',
  # San Joaquin Valley
  'San Joaquin','Stanislaus','Merced','Madera','Fresno','Kings','Tulare','Kern'
)

# Sub-region to pick out with a heavier outline. Set to character(0) to disable.
# Default = the North San Joaquin Valley.
focus_counties <- character(0)

## ---- 0.4 projections and colour scale --------------------------------------
CRS_GLOBAL <- '+proj=eck4'   # as published
CRS_REGION <- 'EPSG:3310'    # NAD83 / California Albers - correct for CA maps

# Published breaks, kept identical so panels a-d are directly comparable with
# the paper. Explicit labels so the legend reads cleanly at regional scale.
NIRR_BREAKS <- c(0, 25, 50, 100, 150, Inf)
NIRR_LABELS <- c('0-25', '25-50', '50-100', '100-150', '>150')

bin_nirr <- function(x) {
  cut(x, breaks = NIRR_BREAKS, labels = NIRR_LABELS,
      include.lowest = TRUE, right = TRUE)
}

fill_nirr <- function(...) {
  scale_fill_viridis_d(option = 'G', direction = -1, drop = FALSE,
                       na.translate = FALSE, ...)
}

NIRR_LAB <- expression(N[Irrig]~'(kg N ha'^{-1}~yr^{-1}*')')

## ---- 0.5 shared themes -----------------------------------------------------
theme_map_ns <- function(base = 11) {
  ggthemes::theme_wsj(color = 'white', base_family = 'sans',
                      title_family = 'sans') +
    theme(
      plot.margin        = unit(c(0, 0.15, 0, 0.15), 'cm'),
      strip.text         = element_text(size = base + 1, face = 'bold'),
      legend.title       = element_text(face = 'bold', size = base + 1),
      legend.text        = element_text(size = base - 1, family = 'sans'),
      axis.text          = element_blank(),
      axis.ticks         = element_blank(),
      axis.line          = element_blank(),
      legend.key         = element_blank(),
      panel.grid.major   = element_blank(),
      panel.background   = element_rect(fill = 'white'),
      plot.background    = element_rect(fill = 'white'),
      legend.background  = element_rect(fill = NA),
      strip.background   = element_rect(fill = 'white')
    )
}

theme_stat_ns <- function(base = 11) {
  ggthemes::theme_wsj(color = 'white', base_family = 'sans',
                      title_family = 'sans') +
    theme(
      plot.margin       = unit(c(0.1, 0.2, 0.1, 0.2), 'cm'),
      strip.text        = element_text(size = base + 1, face = 'bold'),
      axis.title        = element_text(size = base, face = 'bold'),
      axis.text         = element_text(size = base - 1),
      legend.title      = element_text(size = base, face = 'bold'),
      legend.text       = element_text(size = base - 1),
      panel.grid.major  = element_line(colour = 'grey88'),
      panel.background  = element_rect(fill = 'white'),
      plot.background   = element_rect(fill = 'white'),
      legend.background = element_rect(fill = NA),
      strip.background  = element_rect(fill = 'white')
    )
}

## ---- 0.6 panel tags --------------------------------------------------------
# Applied panel by panel with `+` rather than with patchwork's `&`, which
# ggplot2 4.0 shadows with its own S7 `&` generic.
tag_theme <- theme(plot.tag = element_text(face = 'bold', size = 13),
                   plot.tag.position = c(0.02, 0.98))

## ---- 0.7 small helpers -----------------------------------------------------

# The original scripts match rasters with a bare crop-name pattern. Keep that
# behaviour but shout if a pattern is ambiguous, which is the classic way these
# stacks silently pick up the wrong layer.
find_one <- function(dir, pattern, what = 'file') {
  f <- list.files(path = dir, pattern = pattern, full.names = TRUE)
  if (length(f) == 0) stop('No ', what, ' matching "', pattern, '" in ', dir)
  if (length(f) > 1) {
    warning('Pattern "', pattern, '" matched ', length(f), ' files in ', dir,
            '; using ', basename(f[1]), call. = FALSE)
  }
  f[1]
}

# Raster -> tidy data.frame in a target CRS, ready for geom_tile.
rast_to_df <- function(r, crs_out, value_name = 'nirr',
                       method = 'near', na.rm = TRUE) {
  r <- terra::project(r, crs_out, method = method)
  d <- as.data.frame(r, xy = TRUE, na.rm = na.rm)
  names(d) <- c('x', 'y', value_name)
  d
}

# Zone sum that prefers exactextractr (partial-cell aware, as in Fig3.R).
zonal_sum <- function(r, polys, col = 'v') {
  if (requireNamespace('exactextractr', quietly = TRUE)) {
    out <- exactextractr::exact_extract(r, polys, 'sum', progress = FALSE)
  } else {
    out <- terra::extract(r, terra::vect(polys), fun = sum,
                          na.rm = TRUE, ID = FALSE)[, 1]
  }
  setNames(data.frame(as.numeric(out)), col)
}


# ---------------------------------------------------------------------------- #
# 1. BOUNDARIES
# ---------------------------------------------------------------------------- #

## ---- 1.1 world (identical to the published script) -------------------------
# getMap() silently falls back to a coarse world if rworldxtra is absent, which
# degrades the panel (a) basemap and the USA polygon used in panel (f). Fail
# loudly instead of quietly.
if (!requireNamespace('rworldxtra', quietly = TRUE)) {
  warning('rworldxtra is not installed, so the world basemap will be low ',
          'resolution. install.packages("rworldxtra") for the published look.',
          call. = FALSE, immediate. = TRUE)
}
wr  <- sf::st_as_sf(getMap(resolution = 'high'))
wr  <- sf::st_make_valid(wr)
rwr <- subset(sf::st_transform(wr, crs = CRS_GLOBAL), continent != 'Antarctica')

sphere <- sf::st_as_sf(ne_download(category = 'physical',
                                   type = 'wgs84_bounding_box',
                                   returnclass = 'sf'))
sphere <- sf::st_transform(sphere, crs = CRS_GLOBAL)
sphere <- sf::st_crop(sphere, rwr)

## ---- 1.2 US states and California ------------------------------------------
# tigris is preferred over rnaturalearth::ne_states() here for two reasons:
# ne_states() only serves 1:10m data and so requires rnaturalearthhires, which
# is not on CRAN; and taking states and counties from the same source means the
# two sets of boundaries agree exactly.
get_us_states <- function() {
  if (requireNamespace('tigris', quietly = TRUE)) {
    out <- try(sf::st_as_sf(tigris::states(cb = TRUE, progress_bar = FALSE)),
               silent = TRUE)
    if (!inherits(out, 'try-error')) {
      out$name <- out$NAME
      return(sf::st_make_valid(out))
    }
  }
  out <- ne_states(country = 'United States of America', returnclass = 'sf')
  sf::st_make_valid(out)
}

us_states <- get_us_states()
ca_state  <- subset(us_states, name == 'California')
stopifnot(nrow(ca_state) == 1)

## ---- 1.3 California counties -----------------------------------------------
# Three fallbacks so this runs wherever you happen to be.
get_ca_counties <- function() {
  if (requireNamespace('tigris', quietly = TRUE)) {
    out <- try(sf::st_as_sf(tigris::counties(state = 'CA', cb = TRUE,
                                             progress_bar = FALSE)),
               silent = TRUE)
    if (!inherits(out, 'try-error')) {
      out$county <- out$NAME
      return(sf::st_make_valid(out[, 'county']))
    }
  }
  out <- try(sf::st_as_sf(ne_download(scale = 10, type = 'admin_2_counties',
                                      category = 'cultural',
                                      returnclass = 'sf')), silent = TRUE)
  if (!inherits(out, 'try-error')) {
    out <- subset(out, REGION == 'CA' | grepl('California', out$REGION, fixed = TRUE))
    out$county <- gsub(' County$', '', out$NAME)
    return(sf::st_make_valid(out[, 'county']))
  }
  stop('Could not obtain California counties. Install {tigris}, or set CV_SHP ',
       'to a Central Valley polygon and skip the county panels.')
}

ca_cty <- get_ca_counties()
ca_cty <- sf::st_transform(ca_cty, sf::st_crs(ca_state))

## ---- 1.4 Central Valley polygon --------------------------------------------
if (!is.na(CV_SHP)) {
  cv_poly <- sf::st_make_valid(sf::st_read(CV_SHP, quiet = TRUE))
  cv_poly <- sf::st_transform(cv_poly, sf::st_crs(ca_state))
  cv_cty  <- ca_cty[sf::st_intersects(ca_cty, sf::st_union(cv_poly),
                                      sparse = FALSE)[, 1], ]
} else {
  cv_cty  <- subset(ca_cty, county %in% cv_counties)
  if (nrow(cv_cty) < length(cv_counties)) {
    warning('Only ', nrow(cv_cty), ' of ', length(cv_counties),
            ' Central Valley counties matched. Check county name spellings.',
            call. = FALSE)
  }
  cv_poly <- sf::st_union(cv_cty)
}

cv_focus <- subset(cv_cty, county %in% focus_counties)

# Cropping extents (WGS84) - derived, not hard-coded.
cv_bb    <- sf::st_bbox(sf::st_transform(cv_poly, 4326))
cv_ext   <- terra::ext(cv_bb['xmin'] - 0.4, cv_bb['xmax'] + 0.4,
                       cv_bb['ymin'] - 0.4, cv_bb['ymax'] + 0.4)
ca_bb    <- sf::st_bbox(sf::st_transform(ca_state, 4326))
ca_ext   <- terra::ext(ca_bb['xmin'] - 0.5, ca_bb['xmax'] + 0.5,
                       ca_bb['ymin'] - 0.5, ca_bb['ymax'] + 0.5)


# ---------------------------------------------------------------------------- #
# 2. LOAD PREDICTIONS AND IRRIGATED AREAS
# ---------------------------------------------------------------------------- #
# Layer conventions from ML_Nirr_predictions.R:
#   nirr_ha  = kg N per irrigated hectare per year
#   nirr_kgN = nirr_ha * irrigated area (ha)  -> total kg N per grid cell
# Masks in ./Data/masks/ hold crop irrigated area in ha per cell (<5 ha dropped).

message('Reading crop predictions and irrigated-area masks ...')

pred_files <- setNames(
  vapply(crop_names,
         function(x) find_one(PRED_DIR, paste0('site_nirr_', x), 'prediction'),
         character(1)),
  crop_names)

nirr_kgn_stack <- rast(lapply(crop_names, function(x) {
  r <- rast(pred_files[[x]])$nirr_kgN
  names(r) <- x
  r
}))

if (DERIVE_MASKS) {
  message('  deriving irrigated area as nirr_kgN / nirr_ha')
  tia_stack <- rast(lapply(crop_names, function(x) {
    p <- rast(pred_files[[x]])
    a <- p$nirr_kgN / p$nirr_ha
    a[p$nirr_ha <= 0] <- NA        # guard against division by zero
    names(a) <- x
    a
  }))
} else {
  tia_stack <- rast(lapply(crop_names, function(x) {
    r <- rast(find_one(MASK_DIR, x, 'crop mask'))
    r[r < 5] <- NA        # same 5 ha cut-off as the published pipeline
    names(r) <- x
    r
  }))
}

## ---- 2.0 area of applicability ---------------------------------------------
# CAST codes AOA as 1 = inside (prediction supported by the training data),
# 0 = outside (extrapolation). Absent in some builds, so this is conditional.
HAS_AOA <- 'aoa' %in% names(rast(pred_files[[1]]))

if (HAS_AOA) {
  aoa_stack <- rast(lapply(crop_names, function(x) {
    r <- rast(pred_files[[x]])$aoa
    names(r) <- x
    r
  }))
  # Share of a cell's irrigated area that sits inside the AOA, across all crops.
  t_aoa_frac <- sum(tia_stack * aoa_stack, na.rm = TRUE) / sum(tia_stack, na.rm = TRUE)
  names(t_aoa_frac) <- 'aoa_frac'
} else {
  message('  no aoa layer found - extrapolated cells will not be flagged')
}

## ---- 2.1 all-crop totals ----------------------------------------------------
t_irr_kgn <- sum(nirr_kgn_stack, na.rm = TRUE)   # kg N per cell, all crops
t_tia     <- sum(tia_stack,      na.rm = TRUE)   # irrigated ha per cell
t_irr_ha  <- t_irr_kgn / t_tia                   # kg N per irrigated ha
names(t_irr_ha) <- 'nirr_ha'

## ---- 2.2 global panel raster (aggregated for legibility, as published) -----
g_irr_ha <- aggregate(t_irr_ha, 2, 'mean', na.rm = TRUE)
df_global <- rast_to_df(g_irr_ha, CRS_GLOBAL, 'nirr', method = 'mode')
df_global$nirr <- round(df_global$nirr, 1)

## ---- 2.3 regional rasters (NATIVE 100 km2 - do not aggregate) --------------
ca_irr_ha  <- crop(t_irr_ha,  ca_ext)
ca_irr_kgn <- crop(t_irr_kgn, ca_ext)
ca_tia     <- crop(t_tia,     ca_ext)

cv_irr_ha  <- crop(t_irr_ha,  cv_ext)
cv_irr_kgn <- crop(t_irr_kgn, cv_ext)
cv_tia     <- crop(t_tia,     cv_ext)

# Mask strictly to the valley for the statistics (keep the unmasked version for
# the map so the surrounding context stays visible).
cv_vect       <- terra::vect(sf::st_transform(sf::st_as_sf(cv_poly), 4326))
cv_irr_ha_msk <- mask(cv_irr_ha,  cv_vect)
cv_kgn_msk    <- mask(cv_irr_kgn, cv_vect)
cv_tia_msk    <- mask(cv_tia,     cv_vect)

df_ca <- rast_to_df(ca_irr_ha, CRS_REGION, 'nirr')
df_cv <- rast_to_df(cv_irr_ha, CRS_REGION, 'nirr')

# Cells where most of the irrigated area falls outside the area of applicability,
# i.e. where the prediction is an extrapolation. Marked with crosses on the maps.
if (HAS_AOA) {
  df_ca_out <- rast_to_df(crop(t_aoa_frac, ca_ext), CRS_REGION, 'aoa_frac')
  df_ca_out <- subset(df_ca_out, aoa_frac < 0.5)
  df_cv_out <- rast_to_df(crop(t_aoa_frac, cv_ext), CRS_REGION, 'aoa_frac')
  df_cv_out <- subset(df_cv_out, aoa_frac < 0.5)
  message('  cells outside AOA - California: ', nrow(df_ca_out),
          ' | Central Valley window: ', nrow(df_cv_out))
} else {
  df_ca_out <- df_cv_out <- NULL
}

aoa_marks <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  geom_point(data = d, aes(x = x, y = y), shape = 4, size = 0.7,
             stroke = 0.3, colour = 'grey15', inherit.aes = FALSE)
}

## ---- 2.4 reproject vectors for the regional panels -------------------------
ca_state_p <- sf::st_transform(ca_state, CRS_REGION)
# Alaska and Hawaii are dropped before reprojecting: California Albers is not
# defined usefully for them and they only appear as background here anyway.
us_p       <- sf::st_transform(subset(us_states, !name %in% c('Alaska', 'Hawaii')),
                               CRS_REGION)
ca_cty_p   <- sf::st_transform(ca_cty,   CRS_REGION)
cv_cty_p   <- sf::st_transform(cv_cty,   CRS_REGION)
cv_poly_p  <- sf::st_transform(sf::st_as_sf(cv_poly), CRS_REGION)
cv_focus_p <- sf::st_transform(cv_focus, CRS_REGION)


# ---------------------------------------------------------------------------- #
# 3. PANEL a - GLOBAL, WITH CALIFORNIA BOXED
# ---------------------------------------------------------------------------- #

ca_box <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = as.numeric(ca_bb['xmin']) - 0.5, xmax = as.numeric(ca_bb['xmax']) + 0.5,
    ymin = as.numeric(ca_bb['ymin']) - 0.5, ymax = as.numeric(ca_bb['ymax']) + 0.5),
  crs = 4326))
ca_box_g <- sf::st_transform(ca_box, CRS_GLOBAL)

ca_lab <- sf::st_transform(
  sf::st_as_sf(data.frame(lon = -119.5, lat = 47.5),
               coords = c('lon', 'lat'), crs = 4326),
  CRS_GLOBAL)

p_world <- ggplot() +
  geom_sf(data = sphere, fill = 'white', colour = 'black', alpha = 0.7) +
  geom_sf(data = rwr, fill = 'azure4', colour = NA) +
  geom_tile(data = df_global, aes(x = x, y = y, fill = bin_nirr(nirr))) +
  fill_nirr() +
  geom_sf(data = ca_box_g, fill = NA, colour = 'black', linewidth = 0.6) +
  geom_sf_text(data = ca_lab, label = 'California',
               fontface = 'bold', size = 3.6) +
  guides(fill = guide_legend(title.position = 'top', nrow = 1,
                             override.aes = list(colour = NA))) +
  labs(x = NULL, y = NULL, fill = NIRR_LAB) +
  theme_map_ns()


# ---------------------------------------------------------------------------- #
# 4. PANEL b - CALIFORNIA
# ---------------------------------------------------------------------------- #

p_ca <- ggplot() +
  geom_sf(data = us_p, fill = 'grey92', colour = 'white', linewidth = 0.2) +
  geom_sf(data = ca_state_p, fill = 'azure4', colour = 'white', linewidth = 0.3) +
  geom_tile(data = df_ca, aes(x = x, y = y, fill = bin_nirr(nirr))) +
  fill_nirr() +
  aoa_marks(df_ca_out) +
  geom_sf(data = ca_state_p, fill = NA, colour = 'grey25', linewidth = 0.4) +
  geom_sf(data = cv_poly_p, fill = NA, colour = 'black', linewidth = 0.7) +
  coord_sf(xlim = range(df_ca$x), ylim = range(df_ca$y), expand = FALSE, datum = NA) +
  facet_wrap(. ~ 'California') +
  labs(x = NULL, y = NULL) +
  theme_map_ns() +
  theme(legend.position = 'none')


# ---------------------------------------------------------------------------- #
# 5. PANEL c - CENTRAL VALLEY FOCAL MAP
# ---------------------------------------------------------------------------- #

p_cv <- ggplot() +
  geom_sf(data = ca_cty_p, fill = 'grey92', colour = 'white', linewidth = 0.2) +
  geom_sf(data = cv_cty_p, fill = 'azure4', colour = 'white', linewidth = 0.2) +
  geom_tile(data = df_cv, aes(x = x, y = y, fill = bin_nirr(nirr))) +
  fill_nirr() +
  aoa_marks(df_cv_out) +
  geom_sf(data = cv_cty_p, fill = NA, colour = 'grey35', linewidth = 0.25) +
  geom_sf(data = cv_poly_p, fill = NA, colour = 'black', linewidth = 0.7) +
  coord_sf(xlim = range(df_cv$x), ylim = range(df_cv$y), expand = FALSE,
           datum = NA) +
  facet_wrap(. ~ 'Central Valley') +
  labs(x = NULL, y = NULL) +
  theme_map_ns() +
  theme(legend.position = 'none')

if (length(focus_counties) > 0 && nrow(cv_focus_p) > 0) {
  p_cv <- p_cv +
    geom_sf(data = cv_focus_p, fill = NA, colour = 'firebrick',
            linewidth = 0.6, linetype = '21')
}


# ---------------------------------------------------------------------------- #
# 6. ASSEMBLE FIGURE A
# ---------------------------------------------------------------------------- #

# Tags are set per panel rather than with tag_levels: nested patchworks assign
# tag_levels to the nesting blocks, not to the individual plots.
fig_a <- (p_world + labs(tag = '(a)') + tag_theme +
            theme(legend.position = 'bottom', legend.direction = 'horizontal')) /
         ((p_ca + labs(tag = '(b)') + tag_theme) |
          (p_cv + labs(tag = '(c)') + tag_theme)) +
  plot_layout(heights = c(1.25, 1.6), guides = 'collect') +
  plot_annotation(theme = theme(legend.position = 'bottom',
                                legend.direction = 'horizontal'))

ggsave(file.path(OUT_DIR, 'Fig2_CentralValley.jpeg'), fig_a,
       width = 7.5, height = 8.4, dpi = 600)
ggsave(file.path(OUT_DIR, 'Fig2_CentralValley.pdf'), fig_a,
       device = cairo_pdf, width = 7.5, height = 8.4)


################################################################################
##  FIGURE B - what else the data says about N_Irrig in the Central Valley
################################################################################

# ---------------------------------------------------------------------------- #
# 7. PANEL d - CROP-SPECIFIC N_Irrig ACROSS THE CENTRAL VALLEY
# ---------------------------------------------------------------------------- #

message('Building crop-specific Central Valley layers ...')

cv_crop_df <- do.call(rbind, lapply(cv_focus_crops, function(x) {
  r <- rast(pred_files[[x]])$nirr_ha
  r <- mask(crop(r, cv_ext), cv_vect)
  if (all(is.na(values(r)))) return(NULL)
  d <- rast_to_df(r, CRS_REGION, 'nirr')
  if (nrow(d) == 0) return(NULL)
  d$crop <- stringr::str_to_title(x)
  d
}))

cv_crop_out <- if (HAS_AOA) do.call(rbind, lapply(cv_focus_crops, function(x) {
  a <- rast(pred_files[[x]])$aoa
  a <- mask(crop(a, cv_ext), cv_vect)
  if (all(is.na(values(a)))) return(NULL)
  d <- rast_to_df(a, CRS_REGION, 'aoa')
  d <- subset(d, aoa < 0.5)
  if (nrow(d) == 0) return(NULL)
  d$crop <- stringr::str_to_title(x)
  d
})) else NULL

p_crops <- ggplot() +
  geom_sf(data = cv_cty_p, fill = 'grey92', colour = 'white', linewidth = 0.15) +
  geom_tile(data = cv_crop_df, aes(x = x, y = y, fill = bin_nirr(nirr))) +
  fill_nirr() +
  aoa_marks(cv_crop_out) +
  geom_sf(data = cv_poly_p, fill = NA, colour = 'black', linewidth = 0.4) +
  coord_sf(xlim = range(cv_crop_df$x), ylim = range(cv_crop_df$y),
           expand = FALSE, datum = NA) +
  facet_wrap(. ~ crop, nrow = 2) +
  labs(x = NULL, y = NULL, fill = NIRR_LAB) +
  theme_map_ns(base = 10) +
  theme(legend.position = 'right', legend.direction = 'vertical')


# ---------------------------------------------------------------------------- #
# 8. PANEL e - TOTAL N_Irrig AND IRRIGATED AREA BY CROP
# ---------------------------------------------------------------------------- #

cv_crop_tot <- do.call(rbind, lapply(crop_names, function(x) {
  kgn <- mask(crop(nirr_kgn_stack[[x]], cv_ext), cv_vect)
  tia <- mask(crop(tia_stack[[x]],      cv_ext), cv_vect)
  data.frame(
    crop     = x,
    nirr_GgN = as.numeric(global(kgn, 'sum', na.rm = TRUE)) / 1e9,  # kg -> Gg
    area_kha = as.numeric(global(tia, 'sum', na.rm = TRUE)) / 1e3   # ha -> kha
  )
}))
cv_crop_tot$nirr_ha <- with(cv_crop_tot,
                            (nirr_GgN * 1e9) / (area_kha * 1e3))     # kg N/ha
cv_crop_tot <- subset(cv_crop_tot, area_kha > 0 & is.finite(nirr_ha))
cv_crop_tot$crop <- stringr::str_to_title(cv_crop_tot$crop)
cv_crop_tot$share <- 100 * cv_crop_tot$nirr_GgN / sum(cv_crop_tot$nirr_GgN)

p_crop_tot <- ggplot(cv_crop_tot,
                     aes(x = forcats::fct_reorder(crop, nirr_GgN),
                         y = nirr_GgN, fill = nirr_ha)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf('%.0f kg N ha\u207b\u00b9  |  %.0f kha',
                                nirr_ha, area_kha)),
            hjust = -0.05, size = 3) +
  coord_flip(clip = 'off') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.45))) +
  scale_fill_viridis_c(option = 'G', direction = -1) +
  labs(x = NULL,
       y = expression('Total'~N[Irrig]~'(Gg N yr'^{-1}*')'),
       fill = expression(kg~N~ha^{-1})) +
  facet_wrap(. ~ 'N inputs from irrigation by crop') +
  theme_stat_ns() +
  theme(legend.position = 'none')


# ---------------------------------------------------------------------------- #
# 9. PANEL f - HOW THE CENTRAL VALLEY COMPARES
# ---------------------------------------------------------------------------- #
# Cell-level distributions over irrigated land, matching how the paper reports
# its gridded medians (median 46, IQR 35-71 kg N/ha globally).

usa_poly  <- subset(wr, ISO3 == 'USA')
# Crop to CONUS first: the USA polygon reaches Alaska across the dateline, so
# ext(usa_vect) would be the whole globe and the mask would cost a lot for
# nothing. Change these bounds if you want Alaska/Hawaii in the comparison.
conus_ext <- terra::ext(-125, -66, 24, 50)
usa_vect  <- terra::vect(sf::st_transform(usa_poly, 4326))
ca_vect   <- terra::vect(sf::st_transform(ca_state, 4326))

vals_of <- function(r, label) {
  v <- na.omit(as.numeric(values(r)))
  v <- v[v > 0]                     # log axis below; 0 kg N/ha cells drop out
  if (length(v) == 0) return(NULL)
  data.frame(region = label, nirr = v)
}

df_dist <- rbind(
  vals_of(t_irr_ha, 'World'),
  vals_of(mask(crop(t_irr_ha, conus_ext), usa_vect), 'USA (CONUS)'),
  vals_of(mask(crop(t_irr_ha, ca_ext), ca_vect), 'California'),
  vals_of(cv_irr_ha_msk, 'Central Valley')
)
df_dist$region <- factor(df_dist$region,
                         levels = c('World', 'USA (CONUS)', 'California',
                                    'Central Valley'))

dist_n <- df_dist |> count(region) |>
  mutate(lab = paste0(region, '\n(n = ', format(n, big.mark = ','), ')'))
df_dist <- left_join(df_dist, dist_n[, c('region', 'lab')], by = 'region')
df_dist$lab <- factor(df_dist$lab, levels = dist_n$lab)

p_dist <- ggplot(df_dist, aes(x = lab, y = nirr)) +
  geom_violin(fill = 'grey85', colour = NA, alpha = 0.6) +
  geom_boxplot(width = 0.18, fill = 'white', colour = 'black',
               outlier.shape = NA) +
  geom_hline(yintercept = 100, linetype = 'dashed', colour = 'firebrick') +
  annotate('text', x = 0.62, y = 108, hjust = 0, size = 3,
           colour = 'firebrick', label = 'hotspot threshold') +
  scale_y_continuous(trans = 'log10') +
  labs(x = NULL, y = NIRR_LAB) +
  facet_wrap(. ~ 'Gridded N_Irrig over irrigated land') +
  theme_stat_ns()


# ---------------------------------------------------------------------------- #
# 10. PANEL g - COUNTY TOTALS AND INTENSITIES
# ---------------------------------------------------------------------------- #

cty_sum <- cv_cty
cty_sum$nirr_kgN <- zonal_sum(t_irr_kgn, cv_cty)[, 1]
cty_sum$area_ha  <- zonal_sum(t_tia,     cv_cty)[, 1]

cty_df <- sf::st_drop_geometry(cty_sum) |>
  mutate(nirr_MgN = nirr_kgN / 1e3,
         nirr_ha  = nirr_kgN / area_ha,
         focus    = county %in% focus_counties) |>
  filter(is.finite(nirr_ha), area_ha > 0) |>
  arrange(desc(nirr_MgN))

p_county <- ggplot(cty_df,
                   aes(x = forcats::fct_reorder(county, nirr_MgN),
                       y = nirr_MgN)) +
  geom_segment(aes(xend = county, y = 0, yend = nirr_MgN, colour = focus),
               linewidth = 0.8, show.legend = FALSE) +
  geom_point(aes(size = area_ha / 1e3, fill = nirr_ha),
             shape = 21, colour = 'grey20') +
  coord_flip() +
  scale_fill_viridis_c(option = 'G', direction = -1) +
  scale_colour_manual(values = c(`TRUE` = 'firebrick', `FALSE` = 'grey70')) +
  scale_size_continuous(range = c(2, 8)) +
  labs(x = NULL,
       y = expression('Total'~N[Irrig]~'(Mg N yr'^{-1}*')'),
       fill = expression(kg~N~ha^{-1}),
       size = 'Irrigated\narea (kha)') +
  facet_wrap(. ~ 'County contribution') +
  theme_stat_ns() +
  theme(legend.position = 'right', legend.direction = 'vertical',
        legend.box = 'vertical')


# ---------------------------------------------------------------------------- #
# 11. PANEL h - N_Irrig DEPENDENCY (optional)
# ---------------------------------------------------------------------------- #
# Dependency = N_Irrig / total cropland N inputs, the indicator used in Fig. 1b
# and Extended Data Fig. 6a. Needs the total-N-input predictor layer.

p_dep <- NULL
if (!is.na(NIN_TIF) && file.exists(NIN_TIF)) {
  nin <- rast(NIN_TIF)[[1]]
  nin <- resample(crop(nin, cv_ext), cv_irr_ha, method = 'bilinear')
  # Assumes NIN_TIF holds synthetic + manure + BNF + deposition, i.e. WITHOUT
  # N_Irrig, so it is added back into the denominator to match the paper's
  # definition. If your layer already includes it, use: dep <- 100*cv_irr_ha/nin
  dep <- 100 * cv_irr_ha / (nin + cv_irr_ha)
  dep <- mask(clamp(dep, 0, 100, values = FALSE), cv_vect)
  names(dep) <- 'dep'

  df_dep <- rast_to_df(dep, CRS_REGION, 'dep')
  df_dep$bin <- cut(df_dep$dep, c(0, 10, 20, 30, 50, 100),
                    labels = c('0-10', '10-20', '20-30', '30-50', '>50'),
                    include.lowest = TRUE)

  p_dep <- ggplot() +
    geom_sf(data = cv_cty_p, fill = 'grey92', colour = 'white', linewidth = 0.15) +
    geom_tile(data = df_dep, aes(x = x, y = y, fill = bin)) +
    scale_fill_viridis_d(option = 'A', direction = -1, drop = FALSE,
                         na.translate = FALSE) +
    geom_sf(data = cv_poly_p, fill = NA, colour = 'black', linewidth = 0.5) +
    coord_sf(xlim = range(df_dep$x), ylim = range(df_dep$y), expand = FALSE, datum = NA) +
    facet_wrap(. ~ 'N_Irrig dependency') +
    labs(x = NULL, y = NULL, fill = '% of total N inputs') +
    theme_map_ns(base = 10) +
    theme(legend.position = 'right', legend.direction = 'vertical')
} else {
  message('NIN_TIF not set or not found - skipping the dependency panel (h).')
}


# ---------------------------------------------------------------------------- #
# 12. PANEL i - COMPILED FIELD OBSERVATIONS IN AND AROUND THE REGION
# ---------------------------------------------------------------------------- #
# Reality check on the predictions. Read the caveat this panel produces: the
# compiled dataset has essentially no in-valley observations, so the Central
# Valley map above is a model extrapolation, not an interpolation of local data.

p_obs <- NULL
if (file.exists(DATASET)) {
  obs <- readxl::read_excel(DATASET, sheet = 'dataset')
  obs <- obs |>
    filter(!is.na(lon), !is.na(lat),
           lon > -125, lon < -114, lat > 32, lat < 43)

  if (nrow(obs) > 0) {
    obs_sf <- sf::st_as_sf(obs, coords = c('lon', 'lat'), crs = 4326)
    obs_sf$in_cv <- sf::st_intersects(obs_sf, sf::st_as_sf(cv_poly),
                                      sparse = FALSE)[, 1]
    obs_sf$in_cv <- ifelse(obs_sf$in_cv, 'Inside Central Valley',
                           'Elsewhere in California')
    obs_p <- sf::st_transform(obs_sf, CRS_REGION)

    message('California field observations: ', nrow(obs_sf),
            ' | inside the Central Valley: ',
            sum(obs_sf$in_cv == 'Inside Central Valley'))

    p_obs <- ggplot() +
      geom_sf(data = ca_cty_p, fill = 'grey93', colour = 'white',
              linewidth = 0.15) +
      geom_sf(data = cv_poly_p, fill = 'azure3', colour = 'black',
              linewidth = 0.5) +
      geom_sf(data = obs_p, aes(size = N_X, colour = in_cv),
              alpha = 0.75) +
      scale_colour_manual(values = c('Inside Central Valley' = 'firebrick',
                                     'Elsewhere in California' = 'grey25')) +
      scale_size_continuous(range = c(1, 6)) +
      coord_sf(xlim = range(df_ca$x), ylim = range(df_ca$y), expand = FALSE, datum = NA) +
      facet_wrap(. ~ 'Field observations available for training') +
      labs(x = NULL, y = NULL, size = NIRR_LAB, colour = NULL) +
      theme_map_ns(base = 10) +
      theme(legend.position = 'right', legend.direction = 'vertical',
            legend.box = 'vertical')
  }
} else {
  message('dataset.xlsx not found at ', DATASET, ' - skipping panel (i).')
}


# ---------------------------------------------------------------------------- #
# 13. ASSEMBLE FIGURE B
# ---------------------------------------------------------------------------- #

p_crops    <- p_crops    + labs(tag = '(d)') + tag_theme
p_crop_tot <- p_crop_tot + labs(tag = '(e)') + tag_theme
p_dist     <- p_dist     + labs(tag = '(f)') + tag_theme
p_county   <- p_county   + labs(tag = '(g)') + tag_theme
if (!is.null(p_dep)) p_dep <- p_dep + labs(tag = '(h)') + tag_theme
if (!is.null(p_obs)) p_obs <- p_obs + labs(tag = '(i)') + tag_theme

bottom <- if (!is.null(p_dep) && !is.null(p_obs)) {
  p_county | (p_dep / p_obs)
} else if (!is.null(p_obs)) {
  p_county | p_obs
} else if (!is.null(p_dep)) {
  p_county | p_dep
} else {
  p_county
}

fig_b <- p_crops / (p_crop_tot | p_dist) / bottom +
  plot_layout(heights = c(1.3, 1, 1.35))

ggsave(file.path(OUT_DIR, 'Fig2_CentralValley_supporting.jpeg'), fig_b,
       width = 12, height = 14, dpi = 600, limitsize = FALSE)
ggsave(file.path(OUT_DIR, 'Fig2_CentralValley_supporting.pdf'), fig_b,
       device = cairo_pdf, width = 12, height = 14, limitsize = FALSE)


# ---------------------------------------------------------------------------- #
# 14. NUMBERS TO QUOTE IN THE TEXT
# ---------------------------------------------------------------------------- #

cv_vals   <- na.omit(as.numeric(values(cv_irr_ha_msk)))
cv_kgn_t  <- as.numeric(global(cv_kgn_msk, 'sum', na.rm = TRUE))
cv_area_t <- as.numeric(global(cv_tia_msk, 'sum', na.rm = TRUE))

# Area-weighted share of irrigated land above the 100 kg N/ha hotspot threshold.
hot_mask  <- cv_irr_ha_msk > 100
hot_area  <- as.numeric(global(cv_tia_msk * hot_mask, 'sum', na.rm = TRUE))

# Share of irrigated area supported by the training data.
aoa_share <- NA
if (HAS_AOA) {
  aoa_in <- as.numeric(global(mask(crop(sum(tia_stack * aoa_stack, na.rm = TRUE),
                                        cv_ext), cv_vect),
                              'sum', na.rm = TRUE))
  aoa_share <- 100 * aoa_in / cv_area_t
}

cv_summary <- data.frame(
  metric = c('Total N_Irrig (Gg N/yr)',
             'Irrigated area (kha)',
             'Area-weighted mean N_Irrig (kg N/ha/yr)',
             'Median cell N_Irrig (kg N/ha/yr)',
             'IQR lower (kg N/ha/yr)',
             'IQR upper (kg N/ha/yr)',
             'Share of irrigated area >100 kg N/ha (%)',
             'Share of irrigated area inside AOA (%)',
             'Number of 100 km2 cells'),
  value  = c(cv_kgn_t / 1e9,
             cv_area_t / 1e3,
             cv_kgn_t / cv_area_t,
             median(cv_vals),
             quantile(cv_vals, 0.25),
             quantile(cv_vals, 0.75),
             100 * hot_area / cv_area_t,
             aoa_share,
             length(cv_vals))
)

print(cv_summary, row.names = FALSE)
print(cv_crop_tot[order(-cv_crop_tot$nirr_GgN), ], row.names = FALSE)
print(cty_df[, c('county', 'nirr_MgN', 'nirr_ha', 'area_ha')], row.names = FALSE)

write.csv(cv_summary,  file.path(OUT_DIR, 'cv_summary.csv'),      row.names = FALSE)
write.csv(cv_crop_tot, file.path(OUT_DIR, 'cv_by_crop.csv'),      row.names = FALSE)
write.csv(cty_df,      file.path(OUT_DIR, 'cv_by_county.csv'),    row.names = FALSE)

message('Done. Figures and CSVs written to ', OUT_DIR)
