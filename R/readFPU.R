#' Read FPU level data and merge with FPU shapefile
#'
#' @param gdx GDX from IMPACT run
#' @param param_name Name of paramter with FPU column in IMPACT model
#' @param yrs Years to subset. HIGHLY recommended to use this for performance
#' reasons as the data frame might be too big to read all years information.
#' @param crop Crops to subset if available. HIGHLY recommended to use this
#' for performance reasons as the data frame might be too big to read all crops
#' information - specially if they have additional "fctr" information associated
#' with them like rainfed or irrigated areas.
#'
#' @return Spatial Vector which contains FPU level data from IMPACT
#' @importFrom DOORMAT readGDX
#' @export
#'
#' @examples
#' \dontrun{
#' readArea(gdx)
#' }
readFPU <- function(gdx,
                    param_name,
                    yrs = NULL,
                    crop = NULL) {
    data <- readGDX(gdx = gdx, name = param_name)$data
    if (!("fpu" %in% colnames(data))) stop("Column 'fpu' not found in ",
                                           param_name)
    shp <- readSHP()
    names(shp) <- tolower(names(shp))
    names(shp) <- gsub(pattern = "fpu2015",
                       replacement = "fpu",
                       x = names(shp),
                       ignore.case = TRUE)

    if (is.numeric(yrs) || is.character(crop)) {

        if (is.null(crop))  data <- data[data$yrs == yrs, ]
        if (is.null(yrs))   data <- data[data$j == crop, ]
        if (!is.null(yrs) && !is.null(crop))
            data <- data[data$yrs == yrs & data$j == crop, ]

    }

    # Find FPUs which are in shapefile but are missing in data
    missing_data <- setdiff(unique(shp$fpu), unique(data$fpu))

    temp <- data.frame()

    fill_fpus <- function(missing_vector, target_data){
        # Find how many unique columns need to be created for each FPU
        unique_finder <- target_data[target_data$fpu == head(target_data$fpu, 1),]
        # Find number of rows needed in dummy data frame
        rows <- nrow(unique_finder)

        ignore_cols <- c("fpu", "value")
        for(fpus in missing_vector){
            # Create dummy data frame with NAs
            dummy <- data.frame(matrix(nrow = rows, ncol = ncol(target_data)))
            # Give same names are model data
            colnames(dummy) <- colnames(target_data)
            # Dump missing FPUs
            # dummy$fpu <- missing_vector
            # Set columns to ignore
            dummy[,"fpu"] <- fpus
            for (col in setdiff(colnames(dummy), ignore_cols)) {
                dummy[, col] <- unique(unique_finder[, col])
            }
            temp <- rbind(temp, dummy)
        }
        return(temp)
    }

    # Add dummy set on top of real data
    dfx <- rbind(data,
                 fill_fpus(missing_vector = missing_data, target_data = data))

    # Force keeping all of model data rows
    df <- merge(shp, dfx, na.rm = FALSE, all = TRUE)

    return(df)

}
