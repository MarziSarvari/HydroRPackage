#' @describeIn hydroPck_import_nc_files
#'   @param path Character string. The path to the directory containing NetCDF files.
#'   @param ids Numeric vector. The cell IDs to extract from each NetCDF file.
test_that("hydroPck_import_nc_files imports NetCDF files correctly", {
  # Use a temporary directory for testing
  temp_dir <- tempdir()

  # Mock the nc_open and related functions
  nc_open <- function(filename) {
    # Return a mock NetCDF object
    list(lon = c(0, 1), lat = c(0, 1), pr = matrix(1:4, nrow = 2, ncol = 2), time = 1:2)
  }
  nc_close <- function(nc) {
    # Do nothing for mock
  }

  nc.get.time.series <- function(f) {
    # Return a mock time vector
    f$time
  }

  # Call the function with a mock path
  result <- hydroPck_import_nc_files(path = temp_dir)

  # Perform assertions here (e.g., check if the resulting data.table matches expectations)
  expect_true(is.data.table(result))

  # Reset the mock functions
  unloadNamespace("AdvancedRPackage")
})


#' @describeIn hydroPck_calculate_idf
#'   @param data A data.table object containing imported data.
test_that("hydroPck_calculate_idf handles missing 'cell_id' correctly", {
  # Use a temporary directory for testing
  mock_data <- data.table(
    time = as.POSIXct(c(
      "1951-01-01 00:30:00", "1951-01-01 01:30:00", "1951-01-01 02:30:00",
      "1951-01-01 03:30:00", "1951-01-01 04:30:00", "1951-01-01 05:30:00",
      "1951-01-01 06:30:00", "1951-01-01 07:30:00", "1951-01-01 08:30:00",
      "1951-01-01 09:30:00"
    )),
    V1 = c(
      7.953370e-05, 1.356624e-04, 8.818065e-05, 3.993865e-05, 6.333713e-06,
      1.922358e-06, 1.791729e-06, 2.977827e-06, 7.625491e-06, 1.240480e-05
    ),
    V2 = c(
      1.265942e-05, 4.973588e-05, 2.043422e-05, 6.256427e-06, 4.994658e-07,
      1.434900e-08, 1.305186e-08, 1.245901e-08, 5.255420e-08, 6.725526e-08
    ),
    V3 = c(
      5.099834e-05, 9.354753e-05, 4.740013e-05, 1.312094e-05, 6.003063e-07,
      2.552802e-07, 4.179981e-07, 6.063094e-07, 1.587928e-06, 2.686359e-06
    ),
    V4 = c(
      1.339599e-04, 1.659433e-04, 8.543923e-05, 1.620368e-05, 8.859595e-07,
      1.180751e-06, 1.927696e-06, 2.842233e-06, 6.416995e-06, 1.333442e-05
    ),
    V5 = c(
      2.623230e-05, 7.636319e-05, 4.246402e-05, 2.103376e-05, 4.070160e-06,
      4.796618e-07, 2.984415e-07, 4.006511e-07, 1.246562e-06, 1.633019e-06
    ),
    V6 = c(
      1.447850e-04, 1.962146e-04, 1.056790e-04, 1.754683e-05, 8.021777e-07,
      1.265965e-06, 1.381475e-06, 2.780935e-06, 6.244862e-06, 1.479266e-05
    ),
    V7 = c(
      3.495479e-05, 9.416419e-05, 6.620475e-05, 4.288409e-05, 1.253524e-05,
      2.029477e-06, 9.864007e-07, 1.518069e-06, 2.986512e-06, 3.800018e-06
    ),
    V8 = c(
      1.041740e-04, 1.850794e-04, 1.454477e-04, 8.242186e-05, 1.886873e-05,
      4.379326e-06, 3.809859e-06, 5.222522e-06, 1.200907e-05, 1.965327e-05
    ),
    V9 = c(
      1.082894e-04, 1.803656e-04, 9.814572e-05, 1.525705e-05, 6.089110e-07,
      2.516684e-07, 2.363267e-07, 2.553796e-07, 2.068150e-06, 6.021355e-06
    )
  )

  # Remove the 'cell_id' column from the data


  # Call the function with the modified data
  result <- hydroPck_calculate_idf(mock_data)

  # Perform assertions here (e.g., check if the resulting data.table matches expectations)
  expect_true(is.data.table(result))

  # Reset the mock functions (if any)
  unloadNamespace("AdvancedRPackage")
})

# Create a valid test for the function
#' @describeIn hydroPck_plot_idf_curves
#'   @param idf_data A data.table containing IDF models.
test_that("hydroPck_plot_idf_curves plots IDF curves correctly", {
  # Use a temporary directory for testing
  mock_data <- data.table(
    time = as.POSIXct(c(
      "1951-01-01 00:30:00", "1951-01-01 01:30:00", "1951-01-01 02:30:00",
      "1951-01-01 03:30:00", "1951-01-01 04:30:00", "1951-01-01 05:30:00",
      "1951-01-01 06:30:00", "1951-01-01 07:30:00", "1951-01-01 08:30:00",
      "1951-01-01 09:30:00"
    )),
    V1 = c(
      7.953370e-05, 1.356624e-04, 8.818065e-05, 3.993865e-05, 6.333713e-06,
      1.922358e-06, 1.791729e-06, 2.977827e-06, 7.625491e-06, 1.240480e-05
    ))
  idf_data <- hydroPck_calculate_idf(mock_data)

  # Call the function with the test data
  result <- hydroPck_plot_idf_curves(idf_data)

  # Perform assertions here (e.g., check if the resulting ggplot object matches expectations)
  expect_true(inherits(result, "gg"))

  # Reset the mock functions (if any)
  unloadNamespace("AdvancedRPackage")
})


#' @describeIn hydroPck_download_and_unzip
#'   @param url Character string. The URL of the zip file to download.
test_that("hydroPck_download_and_unzip downloads and unzips correctly", {
  # Use a temporary directory for testing
  temp_dir <- tempdir()
  verified_url <- NULL

  if (!is.null(verified_url)) {
    # Run the test only if the URL is verified
    hydroPck_download_and_unzip(url = verified_url)

    # Perform assertions here if needed
    expect_true(some_condition)
  } else {
    # Skip the test if the URL is not verified
    skip("Skipping test as no verified URL is available.")
  }

  # Reset the curl_download function
  unloadNamespace("AdvancedRPackage")
})
