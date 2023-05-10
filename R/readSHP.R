#' Shapefile reader for IMPACT FPUs
#'
#' @return Terra object with shapefile constaining FPUs for IMPACT
#' @importFrom sf read_sf
#' @export
#'
#' @examples
#' \dontrun{
#' readSHP()
#' }

readSHP <- function() {
    target_name <- "fpu2015_polygons_v3_multipart_polygons"
    fpath <-
        dirname(system.file("extdata", paste0(target_name, ".shp"),
                            package = "STATEROOM"))
    s <- read_sf(dsn = fpath, layer = target_name)
    return(s)
}
