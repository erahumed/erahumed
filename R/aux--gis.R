#' Landscape hydro-geographical components of the Albufera Natural Park
#'
#'
#'
#' @description
#' These helpers return `data.frame`s containing information on the
#' hydro-geographical components used by ERAHUMED to model the Albufera
#' Natural Park ecosystem: ditches and rice-field clusters.
#' For a detailed description of how these spatial units are represented
#' and modelled within ERAHUMED, see the
#' [user manual](https://erahumed.github.io/erahumed-book/chapters/birdseye.html#sec-hydrogeo).
#'
#' @param include_geometry `TRUE` or `FALSE`. Whether to include the geometries
#' of the various elements (as a column of class `sfc_MULTIPOLYGON` from the
#' `{sf}` package).
#'
#' @return
#' A `data.frame`.
#'
#' @name hydrogeo_components
#'
#' @export
info_clusters <- function(include_geometry = FALSE) {
  stopifnot(is.logical(include_geometry))
  assert_length_one(include_geometry)

  res <- albufera_clusters

  if (include_geometry) {
    requireNamespace("sf", quietly = TRUE)
    res <- merge(res, albufera_cluster_geometries, by = "element_id")
  }

  return(res)
}

#' @rdname hydrogeo_components
#' @export
info_ditches <- function(include_geometry = FALSE) {
  stopifnot(is.logical(include_geometry))
  assert_length_one(include_geometry)

  res <- albufera_ditches

  if (include_geometry) {
    requireNamespace("sf", quietly = TRUE)
    res <- merge(res, albufera_ditches_geometries, by = "element_id")
  }

  return(res)
}


# --- Helper: shared labels + palette -----------------------------------------
.rfms_palette <- function(rfms_map) {
  if (is.null(rfms_map) || is.null(rfms_map$map_df) || nrow(rfms_map$map_df) == 0) {
    return(list(domain = "N/A", palette = c("N/A" = "#f5e7c1")))
  }
  md <- rfms_map$map_df
  lbl <- paste(md$rfms_id, md$rfms_name, sep = ": ")   # single canonical label
  domain <- sort(unique(lbl))                          # stable order
  pal <- grDevices::hcl.colors(length(domain), palette = "Dynamic", rev = FALSE)
  names(pal) <- domain
  list(domain = domain, palette = pal)
}

# --- Public wrapper -----------------------------------------------------------
plot_albufera_clusters <- function(rfms_map = NULL) {
  tryCatch(.plot_albufera_clusters(rfms_map),
           error = function(cnd) {
             warning("Error while loading Albufera Leaflet map.")
             NULL
           })
}

# --- Leaflet map --------------------------------------------------------------
.plot_albufera_clusters <- function(rfms_map) {
  clusters_df <- info_clusters(include_geometry = TRUE) |>
    merge(info_ditches(), by.x = "ditch_element_id", by.y = "element_id")
  basins_df <- albufera_basins_geometries

  if (!is.null(rfms_map)) {
    map_df <- rfms_map$map_df
    map_df$element_id <- map_df$cluster_id
    map_df$rfms_label <- paste(map_df$rfms_id, map_df$rfms_name, sep = ": ")

    cluster_variety_map <- map_df[, c("element_id", "rfms_label")]
    clusters_df <- merge(clusters_df, cluster_variety_map, by = "element_id")

    pal <- .rfms_palette(rfms_map)
  } else {
    clusters_df$rfms_label <- "N/A"
    pal <- list(domain = "N/A", palette = c("N/A" = "#f5e7c1"))
  }

  color_map <- leaflet::colorFactor(
    palette = unname(pal$palette),
    domain  = pal$domain,
    ordered = TRUE
  )

  leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(
      data = plot_prepare_sf(basins_df),
      fillOpacity = 0, weight = 1, color = "black"
    ) |>
    leaflet::addPolygons(
      data = plot_prepare_sf(clusters_df),
      color = ~color_map(rfms_label),
      fillOpacity = 0.25, weight = 1,
      popup = ~paste("Cluster ID:", cluster_name, "<br>",
                     "Ditch:", ditch_name, "<br>",
                     "Tancat:", ifelse(tancat, "Yes", "No"), "<br>",
                     "RFMS:", rfms_label, "<br>",
                     "Area:", area, "m\u{00B2}"),
      highlightOptions = leaflet::highlightOptions(weight = 0, fillOpacity = 1),
      layerId = ~element_id
    )
}


# ---- helper: compute per-RFMS counts and areas (ordered to palette domain) ---
.rfms_stats <- function(rfms_map, domain) {
  # Map cluster -> rfms label
  md <- rfms_map$map_df
  md$rfms_label <- paste(md$rfms_id, md$rfms_name, sep = ": ")
  assign_df <- data.frame(
    cluster_id = md$cluster_id,
    rfms_label = md$rfms_label,
    stringsAsFactors = FALSE
  )

  # Get cluster areas (m^2) from metadata
  cl <- info_clusters(include_geometry = FALSE)[, c("element_id", "area")]

  # Join
  df <- merge(cl, assign_df, by.x = "element_id", by.y = "cluster_id")

  # Counts and areas
  counts <- tapply(df$area, df$rfms_label, function(x) length(x))
  areas_m2 <- tapply(df$area, df$rfms_label, sum, na.rm = TRUE)

  # Ensure all domain levels are present and ordered
  counts <- counts[domain]; counts[is.na(counts)] <- 0L
  areas_m2 <- areas_m2[domain]; areas_m2[is.na(areas_m2)] <- 0

  list(counts = counts, areas_m2 = areas_m2)
}

plot_albufera_pie <- function(rfms_map) {
  pal <- .rfms_palette(rfms_map)

  st <- .rfms_stats(rfms_map, pal$domain)
  areas_ha <- as.numeric(st$areas_m2) / 1e4
  counts   <- as.integer(st$counts)
  total_ha <- sum(areas_ha, na.rm = TRUE)
  total_n  <- sum(counts,   na.rm = TRUE)
  shares   <- if (total_ha > 0) areas_ha / total_ha else rep(0, length(areas_ha))

  fmt_ha  <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
  fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
  fmt_pct <- function(x) formatC(100 * x, format = "f", digits = 1)

  labels <- sprintf(
    "%s\n%s ha (%s%%)\n%s clusters",
    pal$domain, fmt_ha(areas_ha), fmt_pct(shares), fmt_int(counts)
  )

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  graphics::par(pty = "s", mar = c(2, 2, 3, 2))

  outer_r <- 0.95
  inner_r <- 0.55

  graphics::pie(
    x = areas_ha,
    labels = labels,
    col = unname(pal$palette[pal$domain]),
    border = NA,
    clockwise = TRUE,
    init.angle = 90,
    radius = outer_r,
    # main = "Surface by management system",
    cex = 1  # shrink text to reduce overlaps
  )

  # hole in the middle
  graphics::symbols(0, 0, circles = inner_r, inches = FALSE, add = TRUE,
                    bg = "white", fg = NA)

  # center totals
  graphics::text(0,  0.08, paste(fmt_ha(total_ha), "ha"), cex = 1.3, font = 2)
  graphics::text(0, -0.12, paste(fmt_int(total_n), "clusters"), cex = 1.0)
}








plot_prepare_sf <- function(df) {
  df |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4326) |>
    sf::st_make_valid()
}
