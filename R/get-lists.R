#' Get mappings and locations for hospitals.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_hospital_mappings()
get_hospital_mappings <- function() {
  call_myhosp_api("reporting-units-downloads/mappings")
}

#' Get set of measure categories which can be used in `read_flat_data_extract()`.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_measure_categories()
get_measure_categories <- function() {
  res <- call_myhosp_api("measure-categories")
  tidy_resp_to_df(res$result)
}

#' Show available datasets.
#'
#' @param tidy_data A logical. Whether or not to tidy the data (rename
#' columns and remove columns which only have one unique value).
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_datasets()
get_datasets <- function(tidy_data = TRUE) {
  res <- call_myhosp_api("datasets")
  d <- tidy_resp_to_df(res$result)

  if (tidy_data) {
    d <- rename_dataset(d)
  }

  d
}

#' Get set of measure download codes that can be used in `get_measure_data()`.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_measure_download_codes()
get_measure_download_codes <- function() {
  res <- call_myhosp_api("measure-downloads/measure-download-codes")
  tidy_resp_to_df(res$result)
}


#' Get data by measure download code.
#'
#' @param measure_download_code A measure download code. See `datasheet_code` in
#' `get_measure_download_codes()`.
#'
#' @return data
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_measure_data("myh-adm")
get_measure_data <- function(measure_download_code) {
  assertthat::assert_that(assertthat::is.string(measure_download_code))
  assertthat::assert_that(measure_download_code %in% get_measure_download_codes()$datasheet_code)

  call_myhosp_api(paste0("measure-downloads/", measure_download_code))
}

#' Get all reporting units
#'
#' @return data.frame of reporting units
#' @export
get_reporting_units <- function() {
  # Use the existing hospital mappings function which provides reporting units
  get_hospital_mappings()
}

#' Get all flat data across all measure categories
#'
#' @return data.frame of all flat data
#' @export
get_all_flat_data <- function() {
  # Get categories first
  categories <- get_measure_categories()

  message(glue::glue("Downloading flat data from {nrow(categories)} measure categories"))

  # Download flat data for each category using existing function
  all_data <- categories$measure_category_code |>
    purrr::imap_dfr(~{
      message(glue::glue("Processing category {.y}/{nrow(categories)}: {.x}"))
      read_flat_data_extract(.x, return_caveats = FALSE)
    })

  all_data
}

#' Get all measures data
#'
#' @return data.frame of all measures
#' @export
get_measures <- function() {
  # Use existing exported function that calls the same endpoint
  categories <- get_measure_categories()

  # Get measures from all categories
  all_measures <- categories$measure_category_code |>
    purrr::map_dfr(~get_measures_from_category(.x, trim = FALSE))

  all_measures
}


#' Download all AIHW MyHospitals data to a single .rds file
#'
#' @param file Path to save data (default: "aihw_data.rds")
#' @param force Force redownload, even if file exists and is current (default: FALSE)
#' @return List containing all AIHW data with elements:
#'   \itemize{
#'     \item datasets - All available datasets
#'     \item measures - All measures across categories
#'     \item reporting_units - Hospital and facility information
#'     \item measure_categories - Available measure categories
#'     \item hospital_mappings - Hospital location mappings
#'     \item flat_data - All flat data extracts
#'     \item downloaded - Timestamp of download
#'     \item version_hash - Hash for cache validation
#'   }
#' @examplesIf interactive() && curl::has_internet()
#' # Download all data to default file
#' data <- download_all_aihw_data()
#'
#' # Force fresh download
#' data <- download_all_aihw_data(force = TRUE)
#'
#' @export
download_all_aihw_data <- function(file = "aihw_data.rds", force = FALSE) {

  # Check if we can use cached data
  if (!force) {
    cached_data <- check_cached_data(file)
    if (!is.null(cached_data)) {
      return(cached_data)
    }
  }

  message("Downloading AIHW data...")
  start_time <- Sys.time()

  # Get all data
  data <- list(
    datasets = get_datasets(tidy_data = FALSE),
    measures = get_measures(),
    reporting_units = get_reporting_units(),
    measure_categories = get_measure_categories(),
    hospital_mappings = get_hospital_mappings(),
    flat_data = get_all_flat_data(),
    downloaded = Sys.time(),
    version_hash = get_version_hash()
  )

  end_time <- Sys.time()
  total_time <- round(as.numeric(end_time - start_time, units = "mins"), 1)

  # Save all data
  saveRDS(data, file, compress = TRUE)
  message(glue::glue(
    "Saved {nrow(data$datasets)} datasets, ",
    "{nrow(data$measures)} measures, ",
    "{nrow(data$reporting_units)} reporting units, ",
    "{format(nrow(data$flat_data), big.mark = ',')} flat data rows ",
    "in {total_time} minutes"
  ))
  data
}
