#' Get tidy osm power data
#'
#' @param region
#'
#' @return
#' @export
#'
#' @examples
#' get_tidy_osm_power_data("Zuid-Holland")
get_tidy_osm_power_data <- function(region) {
  # Get data based on user input
  # region <- readline("Download OSM data with power tags from region: ")
  power <- osmextract::oe_get(region) %>%
    dplyr::filter(grepl("power", other_tags))

  # Recreate columns from the `other_tags` column
  power_tidy <- power %>%
    dplyr::mutate(other_tags = strsplit(other_tags, ",")) %>%
    tidyr::unnest(other_tags) %>%
    tidyr::separate(other_tags, into = c('var', 'val'), sep = '=>') %>%
    tidyr::spread(var, val, fill = NA)

  # Clean column names
  names(power_tidy) = gsub('\"', '', names(power_tidy))

  # Write GPKG
  sf::st_write(power_tidy, paste0("osm-power-tidy-", tolower(region), ".gpkg"))
}
