#' @title       Convert between AM and PM variablities
#' @description given an AM/PM string, convert to the appropriate string for
#' @return      AM or PM if matched, x if not
convert_am_pm <- function(
  x, pm_regex = '(p\\.m\\.|P\\.M\\.|p\\.M\\.|P\\.m\\.|pm)',
  am_regex = '(a\\.m\\.|A\\.M\\.|a\\.M\\.|A\\.m\\.|am)') {
  if (grepl(pm_regex, x, perl = TRUE)) {
    return(gsub(pm_regex, 'PM', x, perl = TRUE))
  }
  if (grepl(am_regex, x, perl = TRUE)) {
    return(gsub(pm_regex, 'AM', x, perl = TRUE))
  }
  warning(sprintf('Unable to find valid AM/PM value in %s', x))
  return(x)
}

#' @title       Retrieve an HTML object
#' @description simply a wrapper to retrieve an HTML page object from xml2
#' @return      an xml2 page object
get_page <- function(url) return(xml2::read_html(trimws(url), trim = TRUE,
                                 fill = TRUE))

#' @title       lookup a posix-compatible timezone
#' @description uses a combination of timezone datasets to make something
#'              useful
#' @return      the valid posix timezone
retrieve_timezone <- function(ab = NULL, dst = NULL, country = 'US') {
  abbreviation <- country_code <- abb <- NULL
  if (!is.null(ab) && is.null(abbreviation)) {
    return(covidR::tzdt[abb == ab &
                          country_code == country,][['zone_name']][1])
  }
  if (is.null(ab) && !is.null(dst)) {
    return(covidR::tzdt[abbreviation == dst &
                          country_code == country,][['zone_name']][1])
  }
}


fetch_number_in_text <- function(page_text, search_word = NULL) {
  comma_regex <- '(,*[\\d]+,*[\\d]*)+'
  return(
    as.integer(
      gsub(',', '',
         stringr::str_extract(page_text, paste0('(?<=', search_word, ')',
                                                comma_regex)))))
}

make_access_time <- function() {
  dt <- Sys.time()
  attr(dt, 'tzone') <- 'UTC'
  dt
}

make_datetime_utc <- function(x) {
  attr(x, 'tzone') <- 'UTC'
  x
}

sanitize_integer <- function(x) {
  x <- unlist(lapply(x, gsub, pattern = ',', replacement = '', fixed = TRUE))
  as.integer(unlist(x))
}

sanitize_numeric <- function(x) {
  x <- unlist(lapply(x, gsub, pattern = ',', replacement = '', fixed = FALSE))
  as.numeric(unlist(x))
}

sanitize_char <- function(x) {
  as.character(unlist(x))
}

make_dat <- function(country = 'US', state = NA_character_, url = NA_character_,
                     page = NA_character_,
                     cases = NA_integer_, update_date = NA,
                     deaths = NA, presumptive_positive = NA_integer_,
                     recovered = NA_integer_, tested = NA_integer_,
                     hospitalized = NA_integer_, county = NA_character_,
                     negative_tests = NA_integer_, counties = NA_character_,
                     severe_cases = NA_integer_, lat = NA_real_,
                     no_longer_monitored = NA_integer_, lon = NA_real_,
                     parish = NA_character_, fips = NA_integer_,
                     monitored = NA_integer_, pending_tests = NA_integer_,
                     active = NA_integer_, inconclusive = NA_integer_) {
  runtime <- make_access_time()
  if (is.na(update_date) || length(update_date) == 0) {
    update_date <- make_access_time()
  }
  data.table::data.table(country = country,
                         state = state,
                         url = url,
                         raw_page = as.character(page),
                         access_time = runtime,
                         county = county,
                         cases = sanitize_integer(cases),
                         updated = update_date,
                         deaths = sanitize_integer(deaths),
                         presumptive = sanitize_integer(presumptive_positive),
                         recovered = sanitize_integer(recovered),
                         tested = sanitize_integer(tested),
                         hospitalized = sanitize_integer(hospitalized),
                         negative = sanitize_integer(negative_tests),
                         counties = sanitize_integer(counties),
                         severe = sanitize_integer(severe_cases),
                         lat = sanitize_numeric(lat),
                         lon = sanitize_numeric(lon),
                         parish = parish,
                         fips = sanitize_numeric(fips),
                         monitored = sanitize_integer(monitored),
                         no_longer_monitored = sanitize_integer(
                           no_longer_monitored),
                         pending_tests = sanitize_integer(pending_tests),
                         active = sanitize_integer(active),
                         inconclusive = sanitize_integer(inconclusive)
  )
}

unlist_coordinates <- function(x) {
  dat <- data.table::data.table()
  for (i in 1:length(x)) {
    d <- data.table::data.table(lat = x[[i]][1],
                                lon = x[[i]][2])
    dat <- rbind(dat, d)
  }
  return(dat)
}

lat_lon_to_fips <- function(points_df) {
  counties <- maps::map('county', fill = TRUE, col = 'transparent', plot = FALSE)
  IDs <- sapply(strsplit(counties$names, ':'), function(x) x[1])
  counties_sp <- maptools::map2SpatialPolygons(
    counties, IDs, sp::CRS("+proj=longlat + datum=WGS85"))
  indices <- sp::over(points_df, counties_sp)
  county_names <- sapply(counties_sp@polygons, function(x) x@ID)
  county_names[indices]
}

convert_date <- function(date, format, tz = 'UTC') {
  if (length(date) == 1) {
    return(as.POSIXct(lubridate::as_datetime(date, format = format, tz = tz)))
  }
  if (length(date) > 1) {
    dates <- c()
    for (d in date) {
      dates <- append(dates, as.POSIXct(lubridate::as_datetime(date,
                                                               format = format,
                                                               tz = tz)))
    }
    return(dates)
  }
}