################################################################################
##  00_check_inputs.R
##
##  Run this BEFORE Fig2_CentralValley.R. It reports what you actually have,
##  so you find out about a missing mask or a renamed layer now rather than
##  forty minutes into a global raster read.
##
##  Nothing here writes anything. Run it from the project working directory.
################################################################################

library(terra)

PRED_DIR <- './Output/Predictions/'
MASK_DIR <- './Data/masks/'
DATASET  <- './Data/dataset.xlsx'

crop_names <- c('maize','vegetables','rice','wheat','sugarcane','barley','potato',
                'beans','chickpea','cotton','groundnut','rapeseed','sugarbeet',
                'sunflower','soyabean')

hr <- function(x) cat('\n', strrep('-', 70), '\n', x, '\n', strrep('-', 70), '\n', sep = '')

# ---- 1. do the directories exist? -------------------------------------------
hr('1. DIRECTORIES')
for (d in c(PRED_DIR, MASK_DIR)) {
  cat(sprintf('%-34s %s\n', d, if (dir.exists(d)) 'found' else 'MISSING'))
}
cat(sprintf('%-34s %s\n', DATASET,
            if (file.exists(DATASET)) 'found' else 'MISSING (panel i will be skipped)'))
cat('\nWorking directory: ', getwd(), '\n', sep = '')

# ---- 2. which prediction files are present? ---------------------------------
hr('2. PREDICTION RASTERS  (expected: site_nirr_<crop>.tif)')
pred_files <- list.files(PRED_DIR, pattern = '\\.tif$', full.names = FALSE)
cat('Total .tif files in ', PRED_DIR, ': ', length(pred_files), '\n\n', sep = '')
if (length(pred_files)) {
  cat('First 20 filenames:\n')
  print(utils::head(pred_files, 20))
}

cat('\nPer-crop check for the "site" cross-validation strategy:\n')
pred_status <- data.frame(crop = crop_names, n_matches = NA, file = NA, layers = NA)
for (i in seq_along(crop_names)) {
  f <- list.files(PRED_DIR, pattern = paste0('site_nirr_', crop_names[i]),
                  full.names = TRUE)
  pred_status$n_matches[i] <- length(f)
  if (length(f)) {
    pred_status$file[i]   <- basename(f[1])
    pred_status$layers[i] <- paste(names(rast(f[1])), collapse = ', ')
  }
}
print(pred_status, row.names = FALSE)

cat('\nWhat to look for:\n',
    '  n_matches = 1 for every crop, and layers containing nirr_ha and nirr_kgN.\n',
    '  n_matches = 0  -> the deposit uses different names; see step 3 of the notes.\n',
    '  n_matches > 1  -> ambiguous pattern (e.g. potato also matching sweetpotato).\n',
    '  an "aoa" layer -> good news, you can flag extrapolated cells on the map.\n', sep = '')

# ---- 3. which strategies are present? ---------------------------------------
hr('3. CROSS-VALIDATION STRATEGIES AVAILABLE')
strats <- c('kmeans','gaez','gadm','10nndm','10fold','5nndm','site','7030')
for (s in strats) {
  n <- length(list.files(PRED_DIR, pattern = paste0('^', s, '_nirr_')))
  cat(sprintf('  %-8s %2d files\n', s, n))
}
cat('\nThe published Fig. 2 uses "site". If only one strategy is deposited,\n',
    'it is almost certainly that one.\n', sep = '')

# ---- 4. crop masks -----------------------------------------------------------
hr('4. CROP IRRIGATED-AREA MASKS')
mask_files <- list.files(MASK_DIR, pattern = '\\.tif$')
cat('Total .tif files in ', MASK_DIR, ': ', length(mask_files), '\n\n', sep = '')
if (length(mask_files)) print(utils::head(mask_files, 20))

mask_status <- data.frame(crop = crop_names, n_matches = NA, file = NA)
for (i in seq_along(crop_names)) {
  f <- list.files(MASK_DIR, pattern = crop_names[i], full.names = TRUE)
  mask_status$n_matches[i] <- length(f)
  if (length(f)) mask_status$file[i] <- basename(f[1])
}
cat('\n'); print(mask_status, row.names = FALSE)

# ---- 5. geometry agreement ---------------------------------------------------
hr('5. GEOMETRY  (predictions and masks must be on the same grid)')
f_pred <- list.files(PRED_DIR, pattern = 'site_nirr_maize', full.names = TRUE)
f_mask <- list.files(MASK_DIR, pattern = 'maize', full.names = TRUE)

if (length(f_pred) && length(f_mask)) {
  rp <- rast(f_pred[1]); rm_ <- rast(f_mask[1])
  cat('prediction : '); print(rp)
  cat('\nmask       : '); print(rm_)
  cat('\nSame extent?     ', isTRUE(all.equal(as.vector(ext(rp)), as.vector(ext(rm_)))), '\n')
  cat('Same resolution? ', isTRUE(all.equal(res(rp), res(rm_))), '\n')
  cat('Same CRS?        ', identical(crs(rp, describe = TRUE)$code,
                                     crs(rm_, describe = TRUE)$code), '\n')
  cat('\nResolution in degrees: ', paste(round(res(rp), 5), collapse = ' x '),
      '  (0.0833 deg ~ 10 km, i.e. the 100 km2 grid)\n', sep = '')
  cat('\nIf extent or resolution differ, resample the mask onto the prediction\n',
      'grid before running the figure script.\n', sep = '')
} else {
  cat('Could not load a maize prediction and mask pair - fix steps 2 and 4 first.\n')
}

# ---- 6. is there anything over California at all? -----------------------------
hr('6. CALIFORNIA SANITY CHECK')
if (length(f_pred)) {
  ca_ext <- ext(-124.5, -114, 32.5, 42.1)
  r <- crop(rast(f_pred[1])$nirr_ha, ca_ext)
  v <- na.omit(as.numeric(values(r)))
  cat('Maize nirr_ha cells with data over California: ', length(v), '\n', sep = '')
  if (length(v)) {
    cat('  min / median / max: ',
        paste(round(c(min(v), median(v), max(v)), 1), collapse = ' / '),
        ' kg N ha-1 yr-1\n', sep = '')
    plot(r, main = 'maize nirr_ha over California - eyeball check')
  } else {
    cat('  No data. Either the mask dropped California, or the extent is wrong.\n')
  }
}

# ---- 7. packages -------------------------------------------------------------
hr('7. PACKAGES')
needed <- c('terra','sf','ggplot2','dplyr','patchwork','rworldmap','rnaturalearth',
            'ggthemes','stringr','forcats','readxl','exactextractr','tigris')
for (p in needed) {
  cat(sprintf('  %-16s %s\n', p,
              if (requireNamespace(p, quietly = TRUE)) 'installed' else 'MISSING'))
}
cat('\nexactextractr and tigris are optional but recommended.\n')

hr('DONE')
