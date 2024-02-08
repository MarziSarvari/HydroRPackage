# install.packages(c("ncdf4", "ncdf4.helpers","sf"))
# install.packages(c("terra", "data.table","curl"))
# install.packages("lubridate")
# install.packages("CoSMoS")
library(ncdf4); library(ncdf4.helpers)
library(terra); library(data.table)
library(lubridate);library(CoSMoS)
library(sf);library(curl)
#' Download and unzip data from a given URL
#'
#' Downloads a zip file from the provided URL, extracts it, and prints information about the file.
#'
#' @param url Character string. The URL of the zip file to download.
#' @return Invisible(NULL)
#' @export
#'
#' @examples
#' \dontrun{
#'   hydroPck_download_and_unzip()
#'   custom_url <- "https://example.com/custom_file.zip"
#'   hydroPck_download_and_unzip(url = custom_url)
#' }
hydroPck_download_and_unzip <- function(url = "https://owncloud.cesnet.cz/index.php/s/HyKD3KXSOontoKX/download") {
  # Create the 'raw' directory if it doesn't exist
  dir.create(path = "./data/raw/", recursive = TRUE, showWarnings = FALSE)

  # Download the file
  tryCatch(
    {
      curl_download(url = url, destfile = "./data/raw/NC.zip")
    },
    error = function(e) {
      stop("Error downloading the file. Please check the URL and try again.")
    }
  )

  # Display information about the downloaded file
  tryCatch(
    {
      print(file.info("./data/raw/NC.zip"))
    },
    error = function(e) {
      stop("Error getting information about the downloaded file.")
    }
  )
  # Unzip the file
  tryCatch(
    {
      unzip(zipfile = "./data/raw/NC.zip", exdir = "./data/")
    },
    error = function(e) {
      stop("Error extracting the contents of the zip file.")
    }
  )
  invisible(NULL)
}

#' Import NetCDF (*.nc) files
#'
#' This function imports NetCDF (*.nc) files and returns a data.table containing the imported data.
#'
#' @param path Character string. The path to the directory containing NetCDF files.
#' @param ids Numeric vector. The cell IDs to extract from each NetCDF file. Default is c(296, 263, 264, 265, 295, 297, 327, 328, 329).
#' @return A data.table containing imported data.
#'
#' @examples
#' \dontrun{
#'   hydroPck_import_nc_files(path = "/path/to/nc/files")
#'   hydroPck_import_nc_files(path = "/path/to/nc/files", ids = c(1, 2, 3))
#' }
hydroPck_import_nc_files <- function(path, ids = c(296, 263, 264, 265, 295, 297, 327, 328, 329)) {
  # List NetCDF files in the specified path
  fls <- list.files(path = path,
                    recursive = TRUE,
                    pattern = ".nc",
                    full.names = TRUE)

  dta_all <- lapply(
    X = fls,
    FUN = function(file_path) {
      e <- tryCatch(
        {
          nc <- nc_open(filename = file_path)

          lon <- ncvar_get(nc = nc, varid = "lon")
          lat <- ncvar_get(nc = nc, varid = "lat")
          pr <- ncvar_get(nc = nc, varid = "pr")
          time <- nc.get.time.series(f = nc)

          nc_close(nc = nc)

          r <- rast(x = pr)
          ext(x = r) <- c(range(lon), range(lat))
          crs(x = r) <- "epsg:4326"

          xy <- xyFromCell(object = r, cell = ids)
          val <- t(x = extract(x = r, y = xy))

          dta <- data.table(time = time, val)
        },
        error = function(e) {
          warning(paste("Error processing file:", file_path))
          return(NULL)
        }
      )

      if (inherits(x = e, what = "try-error")) {
        return(NULL)
      } else {
        return(dta)
      }
    }
  )

  # Combine the list of data.tables into one data.table
  dta_all <- rbindlist(l = dta_all, use.names = TRUE, fill = TRUE)

  return(dta_all)
}

#' Calculate IDF models
#'
#' This function calculates IDF models.
#'
#' @param data A data.table object containing imported data.
#' @return A data.table containing IDF models.
#'
#' @examples
#' \dontrun{
#'   # Assuming `hydroPck_import_nc_files` is correctly implemented and tested
#'   data <- hydroPck_import_nc_files(path = "/path/to/nc/files")
#'   hydroPck_calculate_idf(data)
#' }
hydroPck_calculate_idf <- function(data) {
  # Ensure data is a data.table
  if (!is.data.table(data)) {
    stop("Input 'data' must be a data.table.")
  }

  # Melt the data
  dta_all_m <- melt(data = data, id.vars = "time", variable.name = "cell_id")


  # Split the melted data
  spl_dta <- split(x = dta_all_m, f = dta_all_m$cell_id)

  # Function to calculate IDF
  idf <- function(x, rp = c(2, 5, 10, 25, 50, 100),
                  dur = c(1, 2, 5, 10, 24, 48),
                  aggfun = "mean", dist = "gev", ...) {
    agg <- lapply(
      X = dur,
      FUN = function(d) {
        out <- x[, .(time = time,
                     val = do.call(what = paste0("froll", aggfun),
                                   args = list(x = value,
                                               n = d,
                                               align = "center",
                                               fill = 0)))]
        out
      }
    )

    quant <- lapply(
      X = agg,
      FUN = function(a) {
        mx <- a[, .(mx = max(x = val, na.rm = TRUE)),
                by = year(x = time)]

        para <- tryCatch(
          {
            fitDist(data = mx$mx,
                    dist = dist,
                    n.points = 10,
                    norm = "N4",
                    constrain = FALSE)
          },
          error = function(e) {
            warning("Error fitting distribution. Skipping.")
            return(NULL)
          }
        )

        if (is.null(para)) return(NULL)

        prob <- 1 - 1/rp
        q <- qgev(p = prob,
                  loc = para$loc,
                  scale = para$scale,
                  shape = para$shape)

        names(x = q) <- rp
        as.list(x = q)
      }
    )

    names(x = quant) <- dur
    quant_all <- rbindlist(l = quant, idcol = "dur")
    quant_idf <- melt(data = quant_all, id.vars = "dur", variable.name = "rp")

    return(quant_idf)
  }

  # Calculate IDF for each subset
  idf_dta <- lapply(X = spl_dta, FUN = idf)

  # Combine the list of data.tables into one data.table
  idf_dta <- rbindlist(l = idf_dta, idcol = "cell_id", fill = TRUE)

  return(idf_dta)
}

#' Plot IDF Curves
#'
#' This function plots IDF curves.
#'
#' @param idf_data A data.table containing IDF models.
#' @return A ggplot object displaying IDF curves.
#'
#' @examples
#' \dontrun{
#'   # Assuming `hydroPck_calculate_idf` is correctly implemented and tested
#'   idf_data <- hydroPck_calculate_idf(data)
#'   hydroPck_plot_idf_curves(idf_data)
#' }
hydroPck_plot_idf_curves <- function(idf_data) {
  # Ensure idf_data is a data.table
  if (!is.data.table(idf_data)) {
    stop("Input 'idf_data' must be a data.table.")
  }

  # Plot IDF curves
  ggplot(data = idf_data,
         aes(x = as.numeric(x = dur),
             y = value,
             colour = factor(rp))) +
    geom_line() +
    geom_point() +
    scale_colour_manual(name = "Return\nperiod",
                        values = c("yellow", "blue", "red",
                                   "green", "orange", "purple")) +
    labs(x = "Duration (hours)",
         y = "Intensity (mm/h)",
         title = "IDF curve") +
    theme_bw() +
    facet_wrap(facets = ~cell_id)
}
# Export functions
#' @export
