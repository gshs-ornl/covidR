#' @title       Convert between AM and PM variablities
#' @description given an AM/PM string, convert to the appropriate string for
#' @return      AM or PM if matched, x if not
#' @export
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
#' @export
get_page <- function(url) return(xml2::read_html(trimws(url), trim = TRUE,
                                 fill = TRUE))

#' @title       lookup a posix-compatible timezone
#' @description uses a combination of timezone datasets to make something
#'              useful
#' @return      the valid posix timezone
#' @export
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

#' @title
#' @description
#' @return
#' @export
fetch_number_in_text <- function(page_text, search_word = NULL) {
  comma_regex <- '(,*[\\d]+,*[\\d]*)+'
  return(
    as.integer(
      gsub(',', '',
         stringr::str_extract(page_text, paste0('(?<=', search_word, ')',
                                                comma_regex)))))
}

#' @title
#' @description
#' @return
#' @export
make_access_time <- function() {
  dt <- Sys.time()
  attr(dt, 'tzone') <- 'UTC'
  dt
}

#' @title
#' @description
#' @return
#' @export
make_datetime_utc <- function(x) {
  attr(x, 'tzone') <- 'UTC'
  x
}

#' @title
#' @description
#' @return
#' @export
sanitize_integer <- function(x) {
  x <- unlist(lapply(x, gsub, pattern = ',', replacement = '', fixed = TRUE))
  as.integer(unlist(x))
}

#' @title
#' @description
#' @return
#' @export
sanitize_numeric <- function(x) {
  x <- unlist(lapply(x, gsub, pattern = ',', replacement = '', fixed = FALSE))
  as.numeric(unlist(x))
}

#' @title
#' @description
#' @return
#' @export
sanitize_char <- function(x) {
  as.character(unlist(x))
}

#' @title       make the standardized data format
#' @description this provides the standard data format for import into the
#'              covid database
#' @import data.table
#' @export
make_dat <- function(country = 'US', state = NA_character_, url = NA_character_,
                     page = NA_character_, resolution = NA_character_,
                     cases = NA_integer_, update_date = NA,
                     deaths = NA, presumptive_positive = NA_integer_,
                     recovered = NA_integer_, tested = NA_integer_,
                     hospitalized = NA_integer_, county = NA_character_,
                     negative_tests = NA_integer_, counties = NA_character_,
                     severe_cases = NA_integer_, lat = NA_real_,
                     no_longer_monitored = NA_integer_, lon = NA_real_,
                     parish = NA_character_, fips = NA_integer_,
                     monitored = NA_integer_, pending_tests = NA_integer_,
                     active = NA_integer_, inconclusive = NA_integer_,
                     quarantined = NA_integer_) {
  runtime <- make_access_time()
  if (is.na(update_date) || length(update_date) == 0) {
    update_date <- make_access_time()
  }
  scrape_group = as.integer(format(Sys.time(), format = '%Y%m%d%H'))
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
                         inconclusive = sanitize_integer(inconclusive),
                         quarantined = sanitize_integer(quarantined),
                         scrape_group = scrape_group,
                         resolution = resolution
  )
}

#' @title
#' @description
#' @return
#' @export
unlist_coordinates <- function(x) {
  dat <- data.table::data.table()
  for (i in 1:length(x)) {
    d <- data.table::data.table(lat = x[[i]][1],
                                lon = x[[i]][2])
    dat <- rbind(dat, d)
  }
  return(dat)
}

#' @title
#' @description
#' @return
#' @export
lat_lon_to_fips <- function(points_df) {
  counties <- maps::map('county', fill = TRUE, col = 'transparent', plot = FALSE)
  IDs <- sapply(strsplit(counties$names, ':'), function(x) x[1])
  counties_sp <- maptools::map2SpatialPolygons(
    counties, IDs, sp::CRS("+proj=longlat + datum=WGS85"))
  indices <- sp::over(points_df, counties_sp)
  county_names <- sapply(counties_sp@polygons, function(x) x@ID)
  county_names[indices]
}

#' @title
#' @description
#' @return
#' @export
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

#' @title
#' @description
#' @return
#' @export
extract_county_number <- function(x) {
  county <- stringr::str_extract(x, '\\w{1,}')
  cases <- as.integer(stringr::str_extract(x, '\\d{1,}'))
  return(c(county, cases))
}

#' @title
#' @description
#' @return
#' @export
word2num <- function(word){
  wsplit <- strsplit(tolower(word)," ")[[1]]
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while(i <= length(wsplit)){
    j <- 1
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  return(list(word,out))
}

#' @title
#' @description
#' @return
#' @export
extract_county_number_to_df <- function(x) {
  county <- stringr::str_extract(x, '\\w{1,}')
  cases <- as.integer(stringr::str_extract(x, '\\d{1,}'))
  return(data.frame(county = county, cases = cases))
}

#' @title
#' @description
#' @return
#' @export
get_scraper_functions <- function() {
  states <- state.name
  states <- append(states, c('Puerto Rico', 'DC', 'hattiesburg'))
  paste0('scrape_', snakecase::to_snake_case(states), '()')
}

#' @title
#' @description
#' @return
#' @export
wrap_scraper <- function(fxn) {
  message(sprintf('Running scraper %s', fxn))
  tryCatch(
    eval(parse(text = fxn)),
    error = function(e) {
      warning(sprintf('An error %s occurred', e))
      return(NULL)
    },
    warning = function(w) {
      warning(sprintf('Warning occurred, returning anyway'))
      return(eval(parse(text = fxn)))
    }
  )
}

#' @title
#' @description
#' @return
#' @export
run_all_scripts <- function() {
  fxns <- get_scraper_functions()
  dts <- list()
  for (i in 1:length(fxns)) {
    if (grepl('maine', fxns[i], fixed = TRUE)) {
      warning('Maine script needs revisitng')
      dt <- NULL
    }
    dt <- wrap_scraper(fxns[i])
    if (!is.null(dt)) {
      dts <- append(dts, dt)
    }
  }
  data.table::rbindlist(dts, fill = TRUE)
}
