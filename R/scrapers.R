#' @title       Scrape the Alabama Website
#' @description Scrape the Alabama public health website for information about
#'               coronavirus
#' @return      a data.table ready for use or import into database
#' @importFrom data.table :=
#' @export
scrape_alabama <- function() {
  state <- NULL
  state_name <- 'Alabama'
  # parsed_url <- covidR::urls[state == state_name][['url']]
  parse_url <- 'https://www.alabamapublichealth.gov/infectiousdiseases/2019-coronavirus.html'

  page <- xml2::read_html(trimws(parse_url))
  table <- rvest::html_table(page, header = TRUE, trim = TRUE,
                             fill = TRUE)[[1]][-1,-3]

  page_raw <- rvest::html_text(page)
  update_date <- strsplit(unique(names(table)),
                          '\\nUpdated: ', perl = TRUE)[[1]][2]
  names(table) <- c('county', 'cases')
  timezone <- 'America/Chicago'
  update_date <- gsub(' \\(.*\\)', '', convert_am_pm(update_date))
  counties <- table[1]
  num_cases <- table[2]
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%B %d, %Y %I:%M %p',
                                        tz = timezone)
  updated <- update_date
  make_dat(state = state_name, url = parse_url, page = page_raw,
           county = counties, cases = num_cases, update_date = updated)
}

#' @title        Scrape the Alaskan URLs
#' @description Alaska has two URLs, one with the total cases, this script
#'              parses both URLs and returns a single data.table
#' @return      a data.table ready for use or import into database
#' @importFrom data.table :=
#' @export
scrape_alaska <- function() {
  # state_name <- 'Alaska'
  # urls <- covidR::urls[state == state_name][['url']]
  # page <- xml2::read_html(trimws(urls[1]))
  # access_time <- Sys.time()
  # table <- rvest::html_table(page, header = FALSE, fill = TRUE)
  print('Alaska requires further development with Selenium')
}

#' @title        Scrape the Arizona URLs
#' @description Alaska has two URLs, one with the total cases, this script
#'              parses both URLs and returns a single data.table
#' @return      a data.table ready for use or import into database
#' @importFrom data.table :=
#' @export
scrape_arizona <- function() {
  state <- NULL
  state_name <- 'Arizona'
  parse_url <- covidR::urls[state == state_name][['url']]
  page <- xml2::read_html(trimws(parse_url))
  access_time <- Sys.time()
  print('Arizona requires further development with Selenium')
}

#' @title        Scrape the Arizona URLs
#' @description Alaska has two URLs, one with the total cases, this script
#'              parses both URLs and returns a single data.table
#' @return      a data.table ready for use or import into database
#' @importFrom data.table :=
#' @export
scrape_arkansas <- function() {
  state_name <- 'Arkansas'
  base_url <- 'https://www.healthy.arkansas.gov/images/uploads'
  pic_url <- paste0(base_url, 'COVID-19_Case_Map_3.16.20.jpg')

  print('Arkansas requires image processing')
}

#' @title      scrape la times from california
#' @description scrapes data from the LA Times from the Case by county table
#' @return      a data.table ready for use or import into database
#' @importFrom data.table :=
#' @export
scrape_california <- function() {
  state <- NULL
  state_name <- 'California'
  parse_url <- covidR::urls[state == state_name][['url']][2]
  page <- xml2::read_html(trimws(parse_url))
  table <- rvest::html_table(page, header = TRUE, trim = TRUE, fill = TRUE)[[1]]
  page_raw <- rvest::html_text(page)
  timezone = 'America/Los_Angeles'
  update_date <- rvest::html_node(page,
                                  xpath = '/html/body/article/header/p[2]/time')
  update_date <- gsub(' Pacific', '', rvest::html_text(update_date),
                      ignore.case = FALSE, fixed = TRUE)
  update_date <- lubridate::as_datetime(convert_am_pm(update_date),
                                        format = '%B %d, %I:%M %p',
                                        tz = timezone)
  updated <- update_date
  num_counties = table[1]
  num_cases = table[2]
  num_deaths = table[3]

 make_dat(state = state_name, url = parse_url, page = page_raw, county = num_counties,
          cases = num_cases, update_date = updated, deaths = num_deaths)
}

#' @title      scrape la times from colorado
#' @description scrapes data from the LA Times from the Case by county table
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_colorado <- function() {
  state <- NULL
  state_name <- 'Colorado'
  dats <- list()
  parse_url <- covidR::urls[state == state_name][['url']][2]
  page <- get_page(parse_url)
  page_raw <- rvest::html_text(page)
  timezone <- 'America/Denver'
  page_text <- rvest::html_text(
    rvest::html_node(page, xpath = '//*[@id="articlebody"]/p[8]'))
  update_date <- gsub(' at', '', convert_am_pm(gsub(' updated ', '',
                                                     rvest::html_text(
    rvest::html_node(page, xpath = '//*[@id="articlebody"]/p[1]/i')))
  ))
  update_date <- lubridate::as_datetime(update_date, format = '%b %d %i:%m %p',
                                        tz = timezone)
  updated <- update_date
  num_cases <- fetch_number_in_text(page_text, ':cases: ')
  num_hospitalized <- fetch_number_in_text(page_text, 'hospitalized: ')
  num_tested <- fetch_number_in_text(page_text, 'tested')
  num_severe_cases <- fetch_number_in_text(page_text, 'deadly cases: ')
  counties <- fetch_number_in_text(page_text, 'counties: ')
  make_dat(state = state_name, url = as.character(parse_url), page = page_raw,
           cases = num_cases, update_date = updated,
           severe_cases = num_severe_cases, hospitalized = num_hospitalized)
}

#' @title      scrape connecticut
#' @description scrapes urls for connecticut
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_connecticut <- function(driver = NULL) {
  state <- NULL
  state_name <- 'Connecticut'
  # url <- 'https://portal.ct.gov/coronavirus'
  # page <- get_page(url)
  # page_raw <- rvest::html_table(page)
  # tbl <- rvest::html_table(page)[[1]]

  print('Must use selenium')
}

#' @title      scrape delaware
#' @description scrapes urls for delaware
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_delaware <- function() {
  state <- NULL
  state_name <- 'Delaware'
  parse_url <- covidR::urls[state == state_name][['url']]
  print(paste(state_name, 'requires Selenium'))
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_dc <- function() {
  state <- NULL
  state_name <- 'District of Columbia'
  timezone <- 'America/New_York'
  parse_url <- 'https://coronavirus.dc.gov/release/coronavirus-data-update-march-16'
  parse_url <- gsub('16',
              as.character(as.integer(format(Sys.Date(), '%d')) - 1),
  parse_url, fixed = TRUE)
  page <- get_page(parse_url)
  page_raw <- rvest::html_text(page)
  text <- rvest::html_text(
    rvest::html_node(
      page,
      xpath = '//*[@id="node-release-1467866"]/div[1]/div[2]/div/div/p[1]'))
  date_regex <-
    '((?<= As of )\\d{1,} \\w{2}\\son\\s\\w{1,},\\s\\w{1,}\\s\\d{1,})'
  update_date <- stringr::str_extract(text, date_regex)
  update_date <- gsub('\\sam\\s', ' AM ', update_date, perl = TRUE)
  update_date <- gsub('\\spm\\s', ' PM ', update_date, perl = TRUE)
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%H %p on %A, %B %d',
                                        tz = timezone)
  cases <- stringr::str_extract(text, '(?<=case total to )\\d{1,}')
  make_dat(state = state_name, url = parse_url, page = page_raw,
           cases = cases, update_date = update_date)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_florida <- function() {
  state <- NULL
  state_name <- 'Florida'
  timezone <- 'America/New_York'
  parse_url <- paste0('https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/arcgis/rest/',
                'services/Florida_COVID19_Cases/FeatureServer/0/query?f=json',
                '&where=Counts%20IS%20NOT%20NULL&returnGeometry=false&',
                'outFields=*&orderByFields=Counts%20desc')
  dat <- jsonlite::fromJSON(parse_url)
  page_raw <- as.character(dat)
  fdat <- dat$features$attributes
  # florida datestamp nonsensical, make our own
  update_date <- make_access_time()
  make_dat(state = state_name, url = parse_url, page = page_raw,
           cases = fdat[['TotalPositive']], county = fdat[['County_1']],
           tested = fdat[['TotalTests']],
           negative_tests = fdat[['TotalNegative']],
           deaths = fdat[['died']],
           pending_tests = fdat[['TotalPending']])
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_georgia <- function() {
  state <- NULL
  state_name <- 'Georgia'
  timezone <- 'America/New_york'
  # parse_url <- covidr::urls[state == state_name][['url']]
  parse_url <- 'https://dph.georgia.gov/covid-19-daily-status-report'
  page <- get_page(parse_url)
  page_raw <- rvest::html_text(page)
  update_date <- stringr::str_extract(page_raw, '(?<=report generated on: ).*')
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%m/%d/%y %h:%m:%s',
                                        tz = timezone)
  updated <- update_date
  table <- rvest::html_table(page)[[3]]
  make_dat(state = state_name, url = parse_url, page = page_raw,
           cases = table$cases, county = table$county, update_date = updated)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_hawaii <- function() {
  state <- NULL
  state_name <- 'Hawaii'
  timezone <- 'Pacific/Honolulu'
  parse_url <- covidR::urls[state == state_name][['url']]
  page <- get_page(parse_url)
  page_raw <- rvest::html_text(page)
  table <- rvest::html_table(page)[[1]]
  names(table) <- table[1,]
  update_date <- unique(names(table))
  update_date <- convert_am_pm(stringr::str_extract(
    update_date, '(\\d{1,}:\\d{2}\\w{2} on \\w+ \\d{1,}, \\d{4})'))
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%i:%m%p on %b %d, %y',
                                        tz = timezone)
  updated <- update_date
  table <- table[c(-1, -2, -3),]
  table <- table[1:nrow(table) - 1,]
  names(table) <- c('county', 'cases')
  make_dat(state = state_name, url = parse_url, page = page_raw,
           cases = table$cases, county = table$county,
           update_date = updated)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_idaho <- function() {
  state <- county <- NULL
  state_name <- 'Idaho'
  timezone <- 'America/Denver'
  parse_url <- covidR::urls[state == state_name][['url']]
  page <- get_page(parse_url)
  page_raw <- rvest::html_text(page)
  table <- rvest::html_table(page)[[1]]
  tested <- as.integer(table[2,2]) + as.integer(table[3,2]) +
    as.integer(table[4,2])
  num_cases <- table[5,2]
  update_date <- stringr::str_extract(
    page_raw,
    'data as of \\d{1,}:\\d{2} \\w\\.\\w\\. \\w{2,} \\d{1,}\\/\\d{1,}\\/\\d{4}')
  update_date <- gsub('a.m.', 'am', update_date)
  update_date <- gsub('p.m.', 'pm', update_date)
  update_date <- gsub('data as of ', '', update_date)
  update_date <- lubridate::as_datetime(
    update_date, format = '%i:%m %p mt %m/%d/%y',
    tz = timezone)
  county_cases <- stringr::str_extract_all(page_raw,
                                           '\\w{1,} \\(\\d{1,}\\)')[[1]]
  county_cases <- data.table::data.table(orig = county_cases)
  county_cases[, county := stringr::str_extract(orig, '\\w+')]
  county_cases[, cases := stringr::str_extract(orig, '\\d+')]
  updated <- update_date
  d1 <- make_dat(state = state_name, url = parse_url, page = page_raw,
                 cases = county_cases[['cases']], update_date = updated,
                 county = county_cases[['county']])
  d2 <- make_dat(state = state_name, url = parse_url, page = page_raw,
                 counties = nrow(county_cases), cases = num_cases,
                 update_date = updated)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_illinois <- function() {
  state <- NULL
  state_name <- 'Illinois'
  url <- covidR::urls[state == state_name][['url']]
  print('should likely use Selenium to get county resolution')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_indiana <- function() {
  state <- NULL
  state_name <- 'Indiana'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  print('needs selenium')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_iowa <- function() {
  print('Iowa needs selenium')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_kansas <- function() {
  state <- NULL
  state_name <- 'Kansas'
  url <- covidR::urls[state == state_name][['url']][1]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  county_tbl <- rvest::html_table(page, fill = TRUE)
  print('kansas needs selenium')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_kentucky <- function() {
  print('kentucky needs selenium')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_louisiana <- function() {
  state <- NULL
  state_name <- 'Louisiana'
  url <- 'https://services5.arcgis.com/O5K6bb5dZVZcTo5M/arcgis/rest/services/Cases_by_Parish_2/FeatureServer/0/query?f=json&where=PFIPS%20%3C%3E%2099999&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=Deaths%20desc%2CCases%20desc%2CParish%20asc&outSR=102100&resultOffset=0&resultRecordCount=65&cacheHint=true'
  page <- jsonlite::fromJSON(url)
  tbl <- page$features$attributes
  make_dat(state = state_name, url = url, parish = tbl$Parish,
           cases = tbl$Cases, deaths = tbl$Deaths, lat = tbl$Latitude,
           lon = tbl$Longitude, fips = tbl$PFIPS)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_maine <- function() {
  state <- NULL
  state_name <- 'Maine'
  timezone <- 'America/New_York'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbl <- rvest::html_table(page, fill = TRUE)[[2]]
  names(tbl) <- tbl[1,]
  county_cases <- tbl[-1,]
  tbl <- rvest::html_table(page, fill = TRUE)[[1]][, c(-4, -5, -6, -7)]
  names(tbl) <- c(snakecase::to_snake_case(tbl[2,1]),
                  snakecase::to_snake_case(tbl[2,2]),
                  snakecase::to_snake_case(tbl[2,3]))
  update_date <- gsub('Updated: ', '', unique(tbl[1,1]))
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%B %d, %y at %I:%M %p',
                                        tz = timezone)
  state_cases <- tbl[3,]
  cases <- as.integer(gsub(',', '', state_cases[1,1]))
  presumptive <- as.integer(gsub(',', '', state_cases[1,2]))
  negative <- as.integer(gsub(',', '', state_cases[1,3]))
  d1 <- make_dat(state = state_name, url = url, county = county_cases$County,
                 cases = county_cases$Confirmed, page = page_raw,
                 presumptive_positive = county_cases$`Presumptive Postive`,
                 recovered = county_cases$recovered, update_date = update_date)
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = state_cases$confirmed_cases_1,
                 presumptive_positive =
                   state_cases$presumptive_positive_cases_2,
                 negative_tests = state_cases$negative_tests_3,
                 update_date = update_date)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_maryland <- function() {
  print('requries selenium')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_massachusetts <- function() {
  state <- NULL
  state_name <- 'Massachusetts'
  print('requires selenium')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_michigan <- function() {
  state_name <- 'Michigan'
  timezone <- 'America/Chicago'
  url <-
    'https://www.michigan.gov/coronavirus/0,9753,7-406-98163-520743--,00.html'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tables <- rvest::html_table(page, fill = TRUE)
  county_cases <- tables[[1]]
  names(county_cases) <- county_cases[1,]
  county_cases <- county_cases[-1,]
  update_date <- paste0(format(Sys.Date(),
                               format = '%Y-%m-%d'), ' 2:00 PM')
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%Y-%m-%d %I:%M %p',
                                        tz = timezone)
  if (length(tables) > 3) {
    state_data <- tables[[4]]
    d1 <- make_dat(state = state_name, url = url, page = page_raw,
                   county = county_cases$county, cases = county_cases$cawses,
                   update_date = update_date)
    d2 <- make_dat(state = state_name, url = url, page = page_raw,
                   hospitalized = state_data$Hospitalized,
                   cases = state_data$Number)
    data.table::rbindlist(list(d1, d2), fill = TRUE)

  }
  make_dat(state = state_name, url = url, page = page_raw,
           county = county_cases$county, cases = county_cases$cawses,
           update_date = update_date)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_minnesota <- function() {
  state_name <- 'Minnesota'
  url <- 'https://www.health.state.mn.us/diseases/coronavirus/situation.html'
  timezone <- 'America/Chicago'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  update_date <- stringr::str_extract(page_raw,
                                      '(?<=As of )\\w{1,} \\d{1,}, \\d{4}')
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%B %d, %Y',
                                        tz = timezone)
  print('needs selenium for county results')
  state_cases <- as.integer(gsub(',', '',
                                 stringr::str_extract(page_raw,
                                                      '(?<=Positive: )\\d{1,}'))
                            )
  tested <- as.integer(gsub(',', '',
                            stringr::str_extract(page_raw,
                                                 '(?<=Lab: )\\d{1,}')))
  make_dat(state = state_name, url = url, update_date = update_date,
           cases = state_cases, tested = tested)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_mississippi <- function() {
  state_name <- 'Mississippi'
  page <- get_page('https://msdh.ms.gov/msdhsite/_static/14,0,420.html')
  page_raw <- rvest::html_text(page)
  cases <- rvest::html_node(page, xpath = '//*[@id="pvExplorationHost"]/div/div/exploration/div/explore-canvas-modern/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container-modern[2]/transform/div/div[3]/visual-modern/div/svg/g[1]/text/tspan')
  print('needs selenium')
}


#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_missouri <- function() {
  state_name <- 'Missouri'
  timezone <- 'America/Chicago'
  st_url <- covidR::urls[state == state_name][['url']]
  st_page <- get_page(st_url)
  st_page_raw <- rvest::html_text(st_page)
  st_tbls <- rvest::html_table(st_page)[[1]]
  ud <- rvest::html_text(rvest::html_node(st_page, xpath = '//*[@id="main-content"]/p[2]'))
  ud <- convert_am_pm(strsplit(ud, '\r\n\\s{1,}')[[1]][1])
  updated <- lubridate::as_datetime(ud, format = 'As of %I:%M %p CT, %B %d',
                                    tz = timezone)
  url <- 'https://health.mo.gov/living/healthcondiseases/communicable/novel-coronavirus/results.php'
  page <- get_page(url)
  county_page_raw <- rvest::html_text(page)
  tbls <- rvest::html_table(page)[[1]]
  d1 <- make_dat(state = state_name, url = st_url, page = st_page_raw,
                 cases = st_tbls[2,2], update_date = updated,
                 deaths = stringr::str_extract(st_tbls[1,1],
                                               '\\d{1,}'))
  d2 <- make_dat(state = state_name, url = url, page = county_page_raw,
                 county = tbls[,1], cases = tbls[,2],
                 update_date = updated)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_montana <- function() {
  state_name <- 'Montana'
  timezone <- 'America/Denver'
  url <- 'https://dphhs.mt.gov/publichealth/cdepi/diseases/coronavirusmt'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbl <- rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="dnn_ctr92389_HtmlModule_lblContent"]/div[2]'))
  tbl <- gsub('\n{1,}', ' ', tbl)
  state_cases <- as.integer(stringr::str_extract(
    tbl, '(?<=in Montana )(\\d{1,}|\\d{1,},\\d{3})'))
  tested <- as.integer(stringr::str_extract(
    tbl, '(?<=by MTPHL\\*\\* )(\\d{1,}|\\d{1,},\\d{3})'
  ))
  negative <- as.integer(stringr::str_extract(
    tbl, '(?<=negative results )(\\d{1,}|\\d{1,},\\d{3})'
  ))
  update_date <- convert_am_pm(stringr::str_extract(page_raw,
                                                    '(?<=Last updated: ).*'))
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%m/%d/%Y at %I:%M %p',
                                        tz = timezone)

  make_dat(state = state_name, url = url, update_date = update_date,
           cases = state_cases, tested = tested, negative_tests = negative)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_nebraska <- function() {
  state_name <- 'Nebraska'
  timezone <- 'America/Chicago'
  url <- covidR::urls[state == state_name,][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  cases <- as.integer(stringr::str_extract(
    page_raw, '(?<=cases – )(\\d{1,}|\\d{1,},\\d{3})'
  ))
  negative <- as.integer(stringr::str_extract(
    page_raw, '(?<= negative - )(\\d{1,}|\\d{1,},\\d{3})'
  ))
  update_date <- stringr::str_extract(
    page_raw, '(?<=Updated: )\\w{1,} \\d{1,}, \\d{4}'
  )
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%B %d, %Y',
                                        tz = timezone)
  make_dat(state = state_name, url = url, page = page_raw,
           cases = cases, negative_tests = negative, update_date = update_date)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_nevada <- function() {
  state_name <- 'Nevada'
  timezone <- 'America/Los_Angeles'
  url <- 'http://dpbh.nv.gov/coronavirus/'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  # tbl <- rvest::html_table(page)[[1]]
  # update_date <- lubridate::as_datetime(
  #   gsub('last updated ', '', tbl[2,1]), format = '%m/%d/%y, %i:%m %p',
  #   tz = timezone)
  # make_dat(state = state_name, url = url, page = page_raw,
  #          update_date = update_date, cases = tbl[3, 2],
  #          presumptive_positive = tbl[4, 2], negative_tests = tbl[5, 2],
  #          monitored = tbl[9,2])
  print('New Nevada resource needed')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_new_hampshire <- function() {
  print('County data is in a picture')
  state <- NULL
  state_name <- 'New Hampshire'
  timezone <- 'America/New_York'
  url <- 'https://nh.gov/covid19/'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbl <- rvest::html_table(page)[[1]]
  update_date <- gsub('updated', '', stringr::str_extract(rvest::html_text(
    rvest::html_node(
      page, xpath = '//*[@id="bodycontainer"]/main/div[3]/section[1]/h4')),
    '\\(([^\\)]+)\\)'
    ))
  update_date <- lubridate::as_datetime(update_date,
                                        format = '( %B %d, %Y, %I:%M %p',
                                        tz = timezone)
  make_dat(state = state_name, url = url, page = page_raw, cases = tbl[1,2],
           pending_tests = tbl[2,2], tested = tbl[3,2], monitored = tbl[4,2],
           update_date = update_date)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @importFrom magrittr %>%
#' @export
scrape_new_jersey <- function() {
  state <- NULL
  state_name <- 'New Jersey'
  timezone <- 'America/New_York'
  url <- 'https://services7.arcgis.com/Z0rixLlManVefxqY/arcgis/rest/services/Counties/FeatureServer/0/query?f=json&returnGeometry=true&spatialRel=esriSpatialRelIntersects&geometry=%7B%22xmin%22%3A-8766409.899972958%2C%22ymin%22%3A5009377.085700976%2C%22xmax%22%3A-8140237.764260961%2C%22ymax%22%3A5635549.2214129735%2C%22spatialReference%22%3A%7B%22wkid%22%3A102100%7D%7D&geometryType=esriGeometryEnvelope&inSR=102100&outFields=*&outSR=102100&resultType=tile'
  page <- jsonlite::fromJSON(url)
  page_raw <- jsonlite::toJSON(page)
  tbl <- data.table::as.data.table(page$features$attributes)
  tbl[, county := snakecase::to_title_case(COUNTY)]
  url2 <- 'https://maps.arcgis.com/sharing/rest/content/items/84737ef7f760486293b6afa536f028e0?f=json'
  page <- jsonlite::fromJSON(url2)
  print('date_updated will need to be retrieved via selenium')
  make_dat(state = state_name, url = url, page = page_raw,
           county = tbl[['county']], cases = tbl[['Positives']],
           negative_tests = tbl[['Negatives']])
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_new_mexico <- function() {
  state <- NULL
  state_name <- 'New Mexico'
  timezone <- 'America/Denver'
  url <- 'https://nmhealth.org/news/alert/2020/3/?view=856'
  print('url contains a date element, may need to evaluate after march')
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  st_total <- stringr::str_extract(rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="content"]/p[3]')),
    '(?<=total of )\\d{1,}')
  ud <- strsplit(rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="content"]/div[2]/em')), ' - ', fixed = TRUE)[[1]][1]
  ud <- lubridate::as_datetime(paste(ud, '12:00'), format = '%B %d, %Y %H:%M',
                               tz = timezone)
  cc <- rvest::html_text(rvest::html_node(page,
                                          xpath = '//*[@id="content"]/ul[2]'))
  cc <- strsplit(cc, '\r\n', fixed = TRUE)[[1]]
  counties <- c()
  cases <- c()
  for (c in cc) {
    cs <- strsplit(c, ': ', fixed = TRUE)
    counties <- append(counties, gsub(' County', '', cs[1]))
    cases <- append(cases, cs[2])
  }
  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = st_total, update_date = ud)
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 county = counties, cases = cases, update_date = ud)
  data.table::rbindlist(list(d1, d2), fill = TRUE)

}


#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_new_york <- function() {
  state_name <- 'New York'
  message('No URL Found for New York')
  return(NULL)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_hattiesburg <- function() {
  update_time <- NULL
  url <- 'https://data.hattiesburgamerican.com/media/jsons/smpj/covid19_r2.json?v='
  page <- jsonlite::fromJSON(url)
  page_raw <- jsonlite::toJSON(page)
  raw <- data.table::as.data.table(page$features$properties)
  geo <- data.table::as.data.table(
    page$features$geometry[,-length(names(page$features$geometry))])
  coords <- page$features$geometry$coordinates
  dat <- data.table::as.data.table(cbind(raw, unlist_coordinates(coords)))
  names(dat) <- c('country', 'cases', 'deaths', 'recovered', 'active', 'unkown',
                  'update_time', 'something', 'lat', 'lon')
  # ud <- unique(dat[['update_time']])
  # dummy_date <- as.integer(0)
  # class(dummy_date) <- c('POSIXlt', 'POSIXt')
  # dat[, update_date := ]
  # for (d in ud) {
  #   subdat <- dat[update_time == d,]
  #   repeats <- nrow(subdat)
  #   print(subdat)
  #   print(repeats)
  #   nrow(subdat)
  #   dat[udpate_time == d,
  #       update_date := rep(lubridate::as_datatime(update_time,
  #                                            format = '%B %d',
  #                                            tz = 'utc'), repeats)]
  # }
  d <- dat[, date := lubridate::as_datetime(update_time,
                                            format = '%B %d',
                                            tz = 'UTC'),
           by = c('update_time')]
  make_dat(country = dat[['country']], cases = dat[['cases']], url = url,
           deaths = dat[['deaths']], recovered = dat[['recovered']],
           update_date = dat[['update_date']],
           active = dat[['active']], lat = dat[['lat']], lon = dat[['lon']])
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_north_carolina <- function() {
  state_name <- 'North Carolina'
  url <- ' https://www.ncdhhs.gov/covid-19-case-count-nc'
  timezone <- 'America/New_York'
  print('County data will need selenium')
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbl <- rvest::html_table(page)[[1]]
  cases <- tbl[2,1]
  deaths <- tbl[2,2]
  tested <- tbl[2,3]
  update_date <- convert_am_pm(stringr::str_extract(page_raw,
                                                    '(?<=Last updated ).*\\.'))
  update_date <- gsub('a\\.m\\.', 'AM', update_date)
  update_date <- gsub('p\\.m\\.', 'PM', update_date)
  udpate_date <- lubridate::as_datetime(update_date,
                                        format = '%I:%M%p, %B %d, %Y.',
                                        tz = timezone)
  make_dat(state = state_name, cases = cases, deaths = deaths, url = url,
           page = page_raw, update_date = update_date)

}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_north_dakota <- function() {
  state_name <- 'North Dakota'
  print('ND has data in images, will need selenium and OCR to scrape')
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_ohio <- function() {
  state_name <- 'Ohio'
  url <- covidR::urls[state == state_name][['url']]
  timezone <- 'America/New_York'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  state_cases_html <- rvest::html_text(
    rvest::html_node(page, xpath = '//*[@id="odx-main-content"]/article/section[2]/div/div[1]')
  )
  state_cases_str <- gsub('\\s', '', state_cases_html, perl = TRUE)
  state_cases <- stringr::str_extract(state_cases_str, '\\d{1,}(?=Confirmed)')
  counties <- stringr::str_extract(state_cases_str,
                                   '\\d{1,}(?=NumberofCounties)')
  hospitalized <- stringr::str_extract(state_cases_str,
                                       '\\d{1,}(?=NumberofHospit)')
  deaths <- stringr::str_extract(state_cases_str,
                                 '\\d{1,}(?=NumberofDeaths)')
  update_date <- rvest::html_text(
    rvest::html_node(
      page,
      xpath = '//*[@id="odx-main-content"]/article/section[2]/div/div[2]'))
  update_date <- convert_am_pm(gsub('\\s', '', update_date))
  update_date <- lubridate::as_datetime(
    update_date,
    format = 'LastUpdated:%m/%d/%y(Updateddailyat%I%p',
    tz = timezone)
  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = state_cases, counties = counties,
                 hospitalized = hospitalized, deaths = deaths,
                 update_date = update_date)

  tbl <- rvest::html_text(
    rvest::html_node(page,
                     xpath = '//*[@id="odx-main-content"]/article/section[2]/div/div[3]/div/div/div/div[1]/div/p'))
  counties <- stringr::str_extract_all(tbl, '\\w{1,} \\(\\d{1,}\\)')[[1]]
  county_cases <- lapply(counties, extract_county_number)
  cases <- c()
  counties <- c()
  for (case in county_cases) {
    counties <- append(counties, case[1])
    cases <- append(cases, case[2])
  }
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = cases, county = counties, update_date = update_date)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_oklahoma <- function() {
  state_name <- 'Oklahoma'
  timezone <- 'America/Chicago'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  update_date <- rvest::html_text(
    rvest::html_node(
      page,
      xpath = '/html/body/div[2]/main/div/div/div[5]/div[1]/div/div[5]/div/p[2]/em'
      ))
  update_date <- stringr::str_extract(
    update_date,
    '\\d{4}-\\d{1,}-\\d{1,} at \\d{1,}:\\d{1,}\\s\\w{1,}')
  update_date <- gsub(' at', '', update_date, fixed = TRUE)
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%Y-%m-%d %I:%M %p',
                                        tz = timezone)
  tbls <- rvest::html_table(page)
  state_tbl <- tbls[[1]]
  county_tbl <- tbls[[4]]
  state_cases <- state_tbl[1,2]
  state_negative <- state_tbl[3,2]
  state_pending <- state_tbl[4,2]
  state_hospitalized <- state_tbl[5,2]
  state_deaths <- state_tbl[6,2]
  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = state_cases, negative_tests = state_negative,
                 pending_tests = state_pending, update_date = update_date,
                 hospitalized = state_hospitalized, deaths = state_deaths)
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = county_tbl[,2], county = county_tbl[,1],
                 update_date = update_date)
  data.table::rbindlist(list(d1, d2), fill = TRUE)

}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_oregon <- function() {
  state_name <- 'Oregon'
  timezone <- 'America/Los_Angeles'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbls <- rvest::html_table(page)
  update_date <- unique(names(tbls[[1]]))
  update_date <- stringr::str_extract(
    update_date, '\\d{1,}/\\d{1,}/\\d{1,}, \\d{1,}:\\d{1,} \\w\\.\\w.')
  update_date <- gsub('a.m.', 'AM', update_date)
  update_date <- gsub('p.m.', 'PM', update_date)
  update_date <- lubridate::as_datetime(
    update_date, format = '%m/%d/%Y, %I:%M %p', tz = timezone
  )
  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = tbls[[1]][1,2], negative_tests = tbls[[1]][2,2],
                 pending_tests = tbls[[1]][3,2], update_date = update_date,
                 hospitalized = tbls[[4]][1,2])
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 county = tbls[[2]][,1], cases = tbls[[2]][,2],
                 deaths = tbls[[2]][,3], negative_tests = tbls[[2]][,4],
                 update_date = update_date)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_pennsylvania <- function() {
  state_name <- 'Pennsylvania'
  timezone <- 'America/New_York'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbl <- rvest::html_text(
    rvest::html_node(
      page,
      xpath = '//*[@id="ctl00_PlaceHolderMain_PageContent__ControlWrapper_RichHtmlField"]/h4/span'))
  print('Pennsylvania needs Selenium to parse county data')
  cases <- stringr::str_extract(tbl, '\\d{1,}')
  update_date <- rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="WebPartWPQ6"]/div[1]/h3[2]'
  ))
 update_date <- gsub('Page last updated ', '', update_date)
 update_date <- convert_am_pm(update_date)
 update_date <- lubridate::as_datetime(update_date,
                                       format = '%B %d, %Y - %I:%M %p',
                                       tz = timezone)
 make_dat(state = state_name, update_date = update_date, cases = cases,
          url = url, page = page_raw)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_puerto_rico <- function() {
  state_name <- 'Puerto Rico'
  timezone <- 'Atlantic/Bermuda'
  url <- 'http://www.salud.gov.pr/Pages/coronavirus.aspx'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbl <- rvest::html_table(page, fill = TRUE)[[1]]
  update_date <- rvest::html_text(
    rvest::html_node(
      page, xpath = '//*[@id="WebPartWPQ6"]/div[1]/h3[2]'))
  update_date <- gsub(')', '', gsub('de', '', gsub('\\s{1,}', '',
                                     gsub('(Datos al', '', update_date,
                                                          fixed = TRUE))))
  update_date <- convert_month_language(update_date)
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%d%B%Y,%H:%M',
                                        tz = timezone)
  make_dat(state = state_name, url = url, page = page_raw, tested = tbl[3,2],
           cases = tbl[3,3], negative_tests = tbl[3,4],
           update_date = update_date, pending_tests = tbl[3,4])
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_rhode_island <- function() {
  state_name <- 'Rhode Island'
  timezone <- 'America/New_York'
  url <- 'https://docs.google.com/spreadsheet/tq?key=1n-zMS9Al94CPj_Tc3K7Adin-tN9x1RSjjx2UzJ4SV7Q&headers=0&range=A2%3AB5&gid=0&tqx=reqId%3A1'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  js <- gsub('/*O_o*/\ngoogle.visualization.Query.setResponse(', '', page_raw,
             fixed = TRUE)
  js <- gsub(');', '', js, fixed = TRUE)
  tbl <- unlist(jsonlite::fromJSON(js)$table$rows$c)
  make_dat(state = state_name, url = url, page = page_raw,
           cases = tbl[[2]], negative_tests = tbl[[8]],
           pending_tests = tbl[[10]], quarantined = tbl[[14]])
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_south_carolina <- function() {
  state_name <- 'South Carolina'
  url <- 'https://www.scdhec.gov/infectious-diseases/viruses/coronavirus-disease-2019-covid-19/monitoring-testing-covid-19'
  page <- get_page(url)
  raw_page <- rvest::html_text(page)
  update_date <- rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="main"]/article/div/div/section[1]/div/p[1]/span'))
  update_date <- convert_am_pm(gsub('\\s{1,}', '', update_date))
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%A,%B%d,%I:%M%p')
  print('South Carolina needs selenium to get county cases')
  tbl <- rvest::html_table(page)[[1]]
  make_dat(state = state_name, url = url, page = raw_page,
           update_date = update_date, negative_tests = tbl[1,2],
           cases = tbl[2,2])
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_south_dakota <- function() {
  state_name <- 'South Dakota'
  timzone <- 'America/Chicago'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbls <- rvest::html_table(page, fill = TRUE)
  update_date <- rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="content_block"]/p[5]/text()[2]'))
  ud <- convert_am_pm(gsub('\\s', '', update_date, perl = TRUE))
  ud <- gsub('Lastupdated:', '', ud, fixed = TRUE)
  update_date <- lubridate::as_datetime(
    ud, format = '%I:%M%p;%B%d,%Y', tz = timezone
  )
  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = tbls[[1]][1,2], negative_tests = tbls[[1]][2,2],
                 pending_tests = tbls[[1]][3,2], deaths = tbls[[2]][2,2],
                 update_date = update_date, resolution = 'state')
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = tbls[[3]][-8, ][,2], county = tbls[[3]][-8,][,1],
                 resolution = 'county')
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}
#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_tennessee <- function() {
  state_name <- 'Tennessee'
  timezone <- 'America/Chicago'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbls <- rvest::html_table(page)
  ud <- lubridate::as_datetime(paste(Sys.Date(), '14:00'),
                               format = '%Y-%m-%d %H:%M', tz = timezone)
  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 tested = tbls[[1]][1,2], negative_tests = tbls[[1]][1,3],
                 cases = tbls[[1]], resolution = 'state')
  out <- data.table::data.table(county = tbls[[2]][,1],
                                cases = tbls[[2]][, 2])
  out[, county := gsub(' County', '', county, fixed = TRUE)][-nrow(out),]
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 county = out[['county']], cases = out[['cases']])
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_texas <- function() {
  state_name <- "Texas"
  timezone <- 'America/Chicago'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbls <- rvest::html_table(page)
  update_date <- trimws(stringr::str_extract(page_raw, '(?<=Last updated ).*'))
  update_date <- lubridate::as_datetime(
    paste0(update_date, ' 12:00:00'), format = '%B %d, %Y %H:%M:%S',
    tz = timezone)
  d1 <- make_dat(state = state_name, tested = tbls[[1]][1,2],
                 cases = tbls[[2]][1,2], deaths = tbls[[2]][2,2],
                 update_date = update_date)
  d2 <- make_dat(state = state_name, county = tbls[[3]][,1],
                 cases = tbls[[3]][,2], update_date = update_date)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}

#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_utah <- function() {
  state_name <- 'Utah'
  timezone <- 'America/Denver'
  url <- 'https://coronavirus.utah.gov/case-counts/'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbls <- rvest::html_table(page, fill = TRUE)
  print('No Data available without Selenium for Utah')

}
#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_vermont <- function() {
  state_name <- 'Vermont'
  timezone <- 'America/New_York'
  url <- 'https://www.healthvermont.gov/response/infectious-disease/2019-novel-coronavirus'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbl <- rvest::html_table(page)[[1]]
  ud <- stringr::str_extract(page_raw, '(?<=Last updated: ).*')
  make_dat(state = state_name, url = url, page = page_raw,
           cases = tbl[1,2], tested = tbl[2,2], deaths = tbl[3,2],
           monitored = tbl[4,2], no_longer_monitored = tbl[5,2],
           update_date = ud)
}
#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_virginia <- function() {
  state_name <- 'Virgina'
  print('Virginia scraping requires selenium')
}
#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_washington <- function() {
  state_name <- 'Washington'
  timezone <- 'America/Los_Angeles'
  url <- 'https://www.doh.wa.gov/Emergencies/Coronavirus'
  page <- get_page(url)
  ud <- rvest::html_node(
    page, xpath = '//*[@id="dnn_ctr33855_HtmlModule_lblContent"]/p[6]')
  page_raw <- rvest::html_text(page)
  tbls <- rvest::html_table(page, fill = TRUE)
  print('Washington requires interaction with selenium')
}
#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_west_virginia <- function() {
  state_name <- 'West Virgina'
  timezone <- 'America/New_York'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbls <- rvest::html_table(page, fill = TRUE)
  state_dat <- tbls[[4]][1,1]
  county_dat <- tbls[[4]][4,2]
  ud <- paste0(stringr::str_extract(
    state_dat, '(?<=Updated: )\\d{1,}/\\d{1,}/\\d{4}'), '12:00')
  ud <- lubridate::as_datetime(ud, format = '%m/%d/%Y %H:%M',
                               tz = timezone)
  state_dat <- gsub('\r\n', '', state_dat)
  st_cases <- stringr::str_extract(state_dat, '(?<=Positive Cases)\\d{1,}')
  st_negs <- stringr::str_extract(state_dat, '(?<=Negative Cases)\\d{1,}')
  st_deaths <- stringr::str_extract(state_dat, '(?<=Deaths)\\d{1,}')
  st_pending <- stringr::str_extract(state_dat, '(?<=Pending)\\d{1,}')
  d1 <- make_dat(state = state_name, cases = st_cases, negative_tests = st_negs,
                 deaths = st_deaths, pending_tests = st_pending, url = url,
                 page = page_raw)
  county_dat <- extract_county_number_to_df(strsplit(
    gsub('Counties with positive cases: ', '', tbls[[7]][[1]]), ',')[[1]])
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 update_date = ud, county = county_dat[,1],
                 cases = county_dat[,2])
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}
#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_wisconsin <- function() {
  state_name <- 'Wisconsin'
  timezone <- 'America/Chicago'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  county_url <- 'https://services1.arcgis.com/ISZ89Z51ft1G16OK/arcgis/rest/services/COVID19_WI/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=POSITIVE%20DESC&outSR=102100&resultOffset=0&resultRecordCount=1000'
  cj <- jsonlite::fromJSON(county_url)$features$attributes
  ud <- lubridate::as_datetime(paste(unique(cj$DATE), '14:00'),
                               format = '%m/%d/%Y %H:%M', tz = timezone)
  page_raw <- rvest::html_text(page)
  cnty_raw <- as.character(cj)
  tbls <- rvest::html_table(page, fill = TRUE)[[1]]
  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 cases = tbls[1,2], monitored = tbls[2,2])
  d2 <- make_dat(state = state_name, url = county_url, page = cnty_raw,
                 cases = cj$POSITIVE, deaths = cj$DEATHS, county = cj$NAME)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}
#' @title      scrape DC
#' @description scrapes urls for DC
#' @importFrom data.table :=
#' @return      a data.table ready for use or import into database
#' @export
scrape_wyoming <- function() {
  state_name <- 'Wyoming'
  timezone <- 'America/Denver'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tn <- rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="et-boc"]/div/div/div/div[3]/div[1]/div/div/p[1]'))
  ud <- convert_am_pm(stringr::str_extract(tn, '^.*\\(\\d\\s\\w\\.\\w\\.'))
  updated <- lubridate::as_datetime(ud, format = '%m/%d/%y (%H %p',
                                    tz = timezone)
  pd <- stringr::str_extract(tn,
                             '(?<=Wyoming Public Health Laboratory: )\\d{1,}')
  cdctest <- stringr::str_extract(
    tn, '(?<=CDC lab: )\\d{1,}')
  ctest <- stringr::str_extract(tn, '(?<=commercial labs: )\\d{1,}')
  total_test <- as.integer(pd) + as.integer(cdctest) + as.integer(ctest)
  pos <- rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="et-boc"]/div/div/div/div[3]/div[1]/div/div/p[2]/strong'
  ))
  st_pos <- stringr::str_extract(pos, '\\d{1,}')
  cc <- rvest::html_text(rvest::html_node(
    page, xpath = '//*[@id="et-boc"]/div/div/div/div[3]/div[2]/div/div/p'
  ))
  wyoming_counties <- c('Albany', 'Big Horn', 'Campbell', 'Converse',
                        'Crook', 'Fremont', 'Goshen', 'Hot Springs',
                        'Johnnson', 'Laramie', 'Natrona', 'Niobrara',
                        'Park', 'Platte', 'Sheridan', 'Sublette',
                        'Sweetwater', 'Tenton', 'Uinta', 'Washakie',
                        'Weston')
  cregx <- paste0('(', paste0(wyoming_counties, collapse = '|'),
                  '): \\d{1,}')
  county_cases <- stringr::str_extract_all(cc, cregx)[[1]]
  ccc <- strsplit(county_cases, ':', fixed = TRUE)
  counties <- c()
  cases <- c()
  for (p in ccc) {
    counties <- append(counties, p[1])
    cases <- append(cases, trimws(p[2], 'l'))
  }

  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 tested = total_test, cases = st_pos, update_date = updated)
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 update_date = updated, county = counties, cases = cases)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}
