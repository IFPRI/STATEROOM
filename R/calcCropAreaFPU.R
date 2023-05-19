#' FPU Level Crop area
#'
#' @param gdx final GDX from an IMPACT run
#' @return FPU level area_fpu as sf object
#'
#' @importFrom DOORMAT readGDX
#' @importFrom magclass collapseNames as.magpie add_columns dimSums
#' getSets as.data.frame
#' @author Abhijeet Mishra
#' @examples
#' \dontrun{x <- calcCropAreaFPU(gdx)}
#' @export

calcCropAreaFPU <- function(gdx, yrs = NULL, crop = NULL) {

    # Shape data
    shp <- readSHP()
    names(shp) <- tolower(names(shp))
    names(shp) <- gsub(pattern = "fpu2015",
                       replacement = "fpu",
                       x = names(shp),
                       ignore.case = TRUE)

    # Build fixes
    getSets <- value <- NULL

    message(".....Calculating FPU level area")
    area_fpu <- readGDX(gdx = gdx, name = "AREAX0")$data

    # First filter
    if (is.numeric(yrs) || is.character(crop)) {
        area_fpu <-
            area_fpu[area_fpu$yrs %in% yrs | area_fpu$j %in% crop, ]
    }

    area_fpu_mag <- add_columns(x = as.magpie(area_fpu, spatial = "fpu"),
                                  dim = "fctr",
                                  addnm = "Combined",
                                  fill = NA)
    area_fpu_mag[, , "Combined"] <-
        dimSums(x = area_fpu_mag[, , "Combined", invert = TRUE],
                dim = "fctr",
                na.rm = TRUE)

    area_fpu <- magclass::as.data.frame(area_fpu_mag)[, -1]

    colnames(area_fpu) <- c(getSets(area_fpu_mag), "value")

    area_fpu <- droplevels(area_fpu)

    ignore <- c("fpu", "value")
    col_find <- names(area_fpu)[!names(area_fpu) %in% ignore]
    combos <- unique(area_fpu[, col_find])
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

    message("..... Adding missing shapes")
    fpus_shps <- merge(shp, temp, all = TRUE)

    message("..... Adding missing data for transparency")
    df <- merge(fpus_shps, area_fpu, all = TRUE)

    return(df)
}
