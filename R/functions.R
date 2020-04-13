#' Create fars dataset name
#'
#'
#' This is a simple function to create names for datasets from US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System
#' in the working directory to use them in the relevant functions.
#' You can customize year in the dataset name (using the \code{year} argument).
#' @param year A number.
#' @return This function returns file name concatinated with the year input.
#' If input is integer or a class which can be coerced to integer, it easily
#' return expected output, otherwise filename would include NA.
#' @examples?
#' make_filename(2013)
#' make_filename(2014)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}



#' Read dataset
#'
#'
#' This function basically helps to read datasets in working directory.
#' It aimed to read datasets especially from US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#' @param filename A character
#' @return The dataset whose name is given as input in tibble data frame format.
#' If the dataset is not available in the working directory, it gives error.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' Read year and month columns from multiple datasets
#'
#'
#' This function allows you to read month and year columns in more than
#' one datasets in working directory at once.
#' It aimed to read datasets especially from US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#' @param years A numeric vector of which year's datasets should be readed.
#' @return This function returns a list, year and month columns
#' of loaded dataframes as elments.  If the datasets are not
#' available in the working directory, this function gives error like
#' "warning("invalid year: ", year)".
#' @importFrom dplyr mutate select
#' @examples
#' \dontrun{
#' fars_read_years(c(2013,2014))
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      MONTH <- NULL
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Read and summarize datasets
#'
#'
#' This function reads month and year columns in more than
#' one datasets in working directory at once, merges them and
#' summarize their count with respect to year and month.
#' It aimed to summarize datasets especially from US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#' @param years A numeric vector of which year's datasets should be summarized.
#' @return This function returns a dataframe with at leat two columns,
#' month and year columns which are inputs. Year columns have same names as input
#' and includes observation numbers per month.
#' If input is not a integer or a class which can be coerced to integer
#' this function can not create appropriate file name and can not read
#' relevant datasets and gives error like "warning("invalid year: ", year)".
#' @importFrom tidyr spread
#' @import dplyr
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  MONTH <- year <- NULL
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#' Plot State from Fatality Analysis Reporting System Dataset
#'
#'
#' This function helps to read dataset in working directory and
#' plot the state according to state number.
#' It aimed to plot using datasets from US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#' @param state.num A number representing state number.
#' @param year A number for which year's datasets should be readed
#' @return This function returns a plot of state from loaded dataset.
#' If input '\code{state.num}' is not avaliable in the dataset it will
#' throw an error looking like ""invalid STATE number: ", '\code{state.num}'.
#' If the '\code{year}' input is not an integer or a class can not
#' coerced to integer then it would not read the appropriarte file and
#' would give an error.
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' fars_map_state(50,2014)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  STATE <- NULL
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
    map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
