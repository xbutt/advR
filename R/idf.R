library(CoSMoS)

#' calculate Intensity-Duration-Frequency (IDF) quantiles and generate plot
#'
#' this function calculates IDF quantiles based on input data and parameters, and
#' generates a plot for visual representation of the IDF curve
#'
#' @param x_list a list of data tables containing time series data with columns "time", "value", and "cell_id"
#' @param rp vector of return periods for which IDF quantiles are calculated
#' @param dur vector of durations for which IDF quantiles are calculated (in hours)
#' @param aggfun aggregation function for calculating rolling statistics (default is "mean")
#' @param dist distribution to fit for IDF modeling (default is "gev")
#'
#' @return a list containing two elements:
#'   - 'data': a data table with columns for cell id, duration, return period, and corresponding IDF values
#'   - 'plot': a ggplot object visualizing the IDF curve
#'
#' @examples
#' idf_result <- idf(x_list = my_data, rp = c(2, 5, 10, 25, 50, 100), dur = c(1, 2, 5, 10, 24, 48))
#' print(idf_result$data)
#' print(idf_result$plot)
#' @export
idf <- function(x_list,
                rp = c(2, 5, 10, 25, 50, 100),
                dur = c(1, 2, 5, 10, 24, 48),
                aggfun = "mean",
                dist = "gev", ...) {

  #initializing an empty list to store results
  idf_dta <- list()

  #processing each data table in the list
  for (i in seq_along(x_list)) {
    tryCatch({
      x <- x_list[[i]]

      #calculating rolling statistics for each duration
      agg <- lapply(
        X = dur,
        FUN = function(d) {
          out <- x[, .(time = as.POSIXct(time, origin = "1970-01-01"),
                       val = do.call(what = paste0("froll", aggfun),
                                     args = list(x = value,
                                                 n = d,
                                                 align = "center",
                                                 fill = 0)))]
          out
        }
      )

      #calculating maximum values by year for each duration
      quant <- lapply(
        X = agg,
        FUN = function(a) {
          mx <- a[, .(mx = max(x = val,
                               na.rm = TRUE)),
                  by = year(x = time)]

          para <- fitDist(data = mx$mx,
                          dist = dist,
                          n.points = 10,
                          norm = "N4",
                          constrain = FALSE)

          prob <- 1 - 1/rp

          q <- qgev(p = prob,
                    loc = para$loc,
                    scale = para$scale,
                    shape = para$shape)

          names(x = q) <- rp

          as.list(x = q)
        }
      )

      #assigning names to quantiles
      names(x = quant) <- dur

      #combining quantiles into a single table
      quant_all <- rbindlist(l = quant,
                             idcol = "dur")

      #reshaping for plotting
      quant_idf <- melt(data = quant_all,
                        id.vars = "dur",
                        variable.name = "rp")

      #appending the result to the list
      idf_dta[[i]] <- quant_idf
    }, error = function(e) {
      warning(paste("Error processing data table", i, ": ", conditionMessage(e)))
    })
  }

  #combining the results from each data table
  idf_results <- rbindlist(l = idf_dta,
                       idcol = "cell_id")

  #creating the ggplot object for visualization
  idf_plot <- ggplot(data = idf_results,
                     mapping = aes(x = as.numeric(x = dur),
                                   y = value,
                                   colour = rp)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Return\nperiod",
                        values = c("red1", "orange2", "yellow3",
                                   "green4", "navy", "purple4")) +
    labs(x = "Duration (hours)",
         y = "Intensity (mm/h)",
         title = "IDF curve") +
    theme_light() +
    facet_wrap(facets = ~cell_id)

  # Return the calculated values
  return(list(data = idf_results, plot = idf_plot))
}

