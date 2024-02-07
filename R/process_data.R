library(ncdf4); library(ncdf4.helpers)
library(terra); library(data.table)

#' import and process NetCDF data
#'
#' this function reads NetCDF files from a specified directory, extracts relevant
#' information for specified cell IDs, and returns returns a list of data tables with columns for time,
#' cell_id, and corresponding values
#'
#' @param directory path to the directory containing the files
#' @param pattern file pattern to match NetCDF files
#' @param cell_ids vector of cell IDs to extract values
#'
#' @return a list of data tables split by cell_id
#'
#' @examples
#' process_nc(directory = "./data", pattern = ".nc", cell_ids = c(327, 328, 329))
#'
#' @export
process_nc <- function(directory = "./data",
                       pattern = ".nc",
                       cell_ids)  {
  #checking if the directory exists
  if (!dir.exists(directory)) {
    message("Specified directory does not exist")
    return(NULL)
  }

  #listing NetCDF files
  fls <- list.files(path = directory,
                    recursive = TRUE,
                    pattern = pattern,
                    full.names = TRUE)

  #checking if any files match the pattern
  if (length(fls) == 0) {
    stop("No files matching the specified pattern were found")
  }

  #using lapply to read and process each file
  dta_all <- lapply(
    X = fls,
    FUN = function(i) {
      e <- try({
        nc <- nc_open(filename = i)

        #extracting relevant variables
        lon <- ncvar_get(nc = nc, varid = "lon")
        lat <- ncvar_get(nc = nc, varid = "lat")
        pr <- ncvar_get(nc = nc, varid = "pr")
        time <- as.POSIXct(nc.get.time.series(f = nc), format = "%Y-%m-%d %H:%M:%S")

        nc_close(nc = nc)

        #creating a terra raster object
        r <- rast(x = pr)
        ext(x = r) <- c(range(lon), range(lat))
        crs(x = r) <- "epsg:4326"

        #extracting values for specified cell IDs
        xy <- xyFromCell(object = r, cell = cell_ids)
        val <- t(x = extract(x = r, y = xy))

        #creating a data.table with time and values
        dta <- data.table(time = time, value = val)
      }, silent = TRUE)

      if (inherits(x = e, what = "try-error")) {
        return(NULL)
      } else {
        return(dta)
      }
    }
  )

  #combining the list of data tables into a single data table
  dta_all <- rbindlist(l = dta_all)

  # reshaping from wide to long format
  dta_all_m <- melt(data = dta_all,
                    id.vars = "time",
                    variable.name = "cell_id")

  #calculating the maximum value for each cell and year
  mx <- dta_all_m[, .(mx = max(value)),
                  by = .(cell_id, year(x = time))]

  #splitting table by cell_id
  spl_dta <- split(x = dta_all_m,
                   f = dta_all_m$cell_id)

  #returning the list of data tables split by cell_id
  return(spl_dta)

}

