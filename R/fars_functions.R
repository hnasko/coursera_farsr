globalVariables(c('STATE', 'MONTH', 'year', 'n'))

#' Read a delimited file into a tibble
#'
#' This function reads comma separated data from specified file path.
#' Files ending in .gz, .bz2, .xz, or .zip containing a csv file will be
#' automatically uncompressed.
#' Files should be stored locally. Remote files can not be automatically downloaded.
#'
#' @param filename A character string giving a path to a file.
#'
#' @return The function returns a data frame.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read(file.path("data", "accident_2013.csv.bz2"))
#' fars_read("accident_2013.csv")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Create a unified name of archive
#'
#' This function transform a character or a numeric vector with the
#' sequence of numbers into a unified filename.
#'
#' @param year A character or numeric vector which represents the sequence of numbers
#'
#' @return The function returns a character vector with a unified name of a file.
#'
#' @examples
#' \dontrun{
#' make_filename(2019)
#' make_filename(c(2019, 2018, "2017"))
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read files by years
#'
#' This function reads archives by specified years from working directory and
#' return a list of data frames with year and month columns
#'
#' @param years A character or numeric vector which represents the sequence of numbers.
#'
#' @return The list of data frames with year and month columns.
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(2019)
#' fars_read_years(c(2019, 2018, "2017"))
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Count number of entries by year and month
#'
#' This function reads archives by specified years from working directory and
#' count number of entries in files by year and month
#'
#' @param years A character or numeric vector which represents the sequence of numbers.
#'
#' @return a data frame with number of entries by year and month.
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2019)
#' fars_summarize_years(c(2019, 2018, "2017"))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Draw accidents on a map by specified state and year
#'
#' This function reads archive by specified year from working directory and
#' draw accidents on a map by specified state and year
#'
#' @param state.num A character or number which represents a state.
#' @param year A character or number which represents a year.
#'
#' @return a chart wich show accidents on a map.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#'
#' @examples
#' \dontrun{
#' fars_map_state(4,2013)
#' fars_map_state("6",2013)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
