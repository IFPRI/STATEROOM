#' FPU Level Production
#'
#' @param gdx final GDX from an IMPACT run
#' @param yrs Years to subset. Defaults to NULL for all years.
#' @param crop Crops to subset. Defaults to NULL for all crops.
#' @return FPU level production as sf object
#'
#' @importFrom DOORMAT readGDX
#' @importFrom magclass collapseNames as.magpie add_columns dimSums
#' getSets as.data.frame
#' @author Abhijeet Mishra
#' @examples
#' \dontrun{x <- calcYieldFPU(gdx)}
#' @export

calcYieldFPU <- function(gdx, yrs = NULL, crop = NULL) {

    # Shape data
    shp <- readSHP()
    names(shp) <- tolower(names(shp))
    names(shp) <- gsub(pattern = "fpu2015",
                       replacement = "fpu",
                       x = names(shp),
                       ignore.case = TRUE)

    # Build fixes
    getSets <- value <- NULL

    message(".....Retrieving FPU level area")
    area_fpu <- readGDX(gdx = gdx, name = "AREAX0")$data

    message(".....Retrieving FPU level yields")
    yld_fpu  <- readGDX(gdx = gdx, name = "YLDX0")$data

    area_fpu_mag <- collapseNames(as.magpie(area_fpu, spatial = "fpu"))
    yld_fpu_mag  <- collapseNames(as.magpie(yld_fpu, spatial = "fpu"))

    message(".....Calculating FPU level production")
    production_fpu <- area_fpu_mag * yld_fpu_mag

    production_fpu <- add_columns(x = production_fpu,
                                  dim = "fctr",
                                  addnm = "Combined",
                                  fill = NA)
    production_fpu[, , "Combined"] <-
        dimSums(x = production_fpu[, , "Combined", invert = TRUE],
                dim = "fctr",
                na.rm = TRUE)

    area_fpu_mag <- add_columns(x = area_fpu_mag,
                                  dim = "fctr",
                                  addnm = "Combined",
                                  fill = NA)
    area_fpu_mag[, , "Combined"] <-
        dimSums(x = area_fpu_mag[, , "Combined", invert = TRUE],
                dim = "fctr",
                na.rm = TRUE)

    message(".....Re-calculating FPU level yield")

    yield_mag <- production_fpu / area_fpu_mag

    yld <- magclass::as.data.frame(yield_mag)[, -1]

    colnames(yld) <- c(getSets(yield_mag), "value")

    # First filter
    if (is.numeric(yrs) || is.character(crop)) {
        yld <-
            yld[yld$yrs %in% yrs | yld$j %in% crop, ]
    }

    yld <- droplevels(yld)

    ignore <- c("fpu", "value")
    col_find <- names(yld)[!names(yld) %in% ignore]
    combos <- unique(yld[, col_find])
    rownames(combos) <- NULL

    # Create dummy dataframe to have all polygons

    temp <- list()
    fpus <- unique(shp$fpu)
    message("..... Fixing gaps and missing data")
    for (fpu in fpus) {
        temp2 <- combos
        temp2$fpu <- fpu
        temp[[fpu]] <- temp2
    }
    temp <- do.call(rbind, temp)

    message("..... Adding missing data for transparency")
    fpus_shps <- merge(shp, temp, all = TRUE)

    df <- merge(fpus_shps, yld, all = TRUE)

    return(df)
}
