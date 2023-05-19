#' FPU Level Production
#'
#' @param gdx final GDX from an IMPACT run
#' @return FPU level production as sf object
#'
#' @importFrom DOORMAT readGDX
#' @importFrom magclass collapseNames as.magpie add_columns dimSums
#' getSets as.data.frame
#' @author Abhijeet Mishra
#' @examples
#' \dontrun{x <- calcProductionFPU(gdx)}
#' @export

calcProductionFPU <- function(gdx, yrs = NULL, crop = NULL) {

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

    message(".....Calculating FPU level yields")
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

    production <- magclass::as.data.frame(production_fpu)[, -1]

    colnames(production) <- c(getSets(production_fpu), "value")

    # First filter
    if (is.numeric(yrs) || is.character(crop)) {
        production <-
            production[production$yrs %in% yrs | production$j %in% crop, ]
    }

    production <- droplevels(production)

    ignore <- c("fpu", "value")
    col_find <- names(production)[!names(production) %in% ignore]
    combos <- unique(production[, col_find])
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

    df <- merge(fpus_shps, production, all = TRUE)

    return(df)
}
