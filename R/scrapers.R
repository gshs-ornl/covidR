#' @title       Scrape the Alabama Website
#' @description Scrape the Alabama public health website for information about
#'               coronavirus
#' @return      a data.table ready for use or import into database
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
scrape_alaska <- function() {
  # state_name <- 'Alaska'
  # urls <- covidR::urls[state == state_name][['url']]
  # page <- xml2::read_html(trimws(urls[1]))
  # access_time <- Sys.time()
  # table <- rvest::html_table(page, header = FALSE, fill = TRUE)
  warning('Alaska requires further development with Selenium')
}

#' @title        Scrape the Arizona URLs
#' @description Alaska has two URLs, one with the total cases, this script
#'              parses both URLs and returns a single data.table
#' @return      a data.table ready for use or import into database
scrape_arizona <- function() {
  state <- NULL
  state_name <- 'Arizona'
  parse_url <- covidR::urls[state == state_name][['url']]
  page <- xml2::read_html(trimws(parse_url))
  access_time <- Sys.time()
  warning('Arizona requires further development with Selenium')
}

scrape_arkansas <- function() {
  state_name <- 'Arkansas'
  base_url <- 'https://www.healthy.arkansas.gov/images/uploads'
  pic_url <- paste0(base_url, 'COVID-19_Case_Map_3.16.20.jpg')

  warning('Arkansas requires image processing')
}

#' @title      scrape la times from california
#' @description scrapes data from the LA Times from the Case by county table
#' @return      a data.table ready for use or import into database
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
#' @importFrom magrittr %>%
#' @return      a data.table ready for use or import into database
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

scrape_connecticut <- function(driver = NULL) {
  state <- NULL
  state_name <- 'Connecticut'
  url <- 'https://portal.ct.gov/coronavirus'
  page <- get_page(url)
  page_raw <- rvest::html_table(page)
  tbl <- rvest::html_table(page)[[1]]

  warning('Must use selenium')
}

scrape_delaware <- function() {
  state <- NULL
  state_name <- 'Delaware'
  parse_url <- covidR::urls[state == state_name][['url']]
  warning(paste(state_name, 'requires Selenium'))
}

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

scrape_florida <- function() {
  state <- NULL
  state_name <- 'Florida'
  timezone <- 'America/New_York'
  parse_url <- paste0('https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/arcgis/rest/',
                'services/Florida_COVID19_Cases/FeatureServer/0/query?f=json',
                '&where=Counts%20IS%20NOT%20NULL&returnGeometry=false&',
                'outFields=*&orderByFields=Counts%20desc')
  dat <- jsonlite::fromJSON(parse_url)
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

scrape_hawaii <- function() {
  state <- NULL
  state_name <- 'Hawaii'
  timezone <- 'Pacific/Honolulu'
  parse_url <- covidR::urls[state == 'hawaii'][['url']]
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

scrape_idaho <- function() {
  state <- county <- NULL
  state_name <- 'Idaho'
  timezone <- 'America/Denver'
  parse_url <- covidR::urls[state == 'idaho',][['url']]
  page <- get_page(parse_url)
  page_raw <- rvest::html_text(page)
  table <- rvest::html_table(page)[[1]]
  tested <- table[2,2] + table[3,2] + table[4,2]
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

scrape_illinois <- function() {
  state <- NULL
  state_name <- 'Illinois'
  url <- covidR::urls[state == state_name][['url']]
  warning('should likely use Selenium to get county resolution')
}

scrape_indiana <- function() {
  state <- NULL
  state_name <- 'Indiana'
  url <- covidR::urls[state == state_name][['url']]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  warning('needs selenium')
}

scrape_iowa <- function() {
  warning('Iowa needs selenium')
}

scrape_kansas <- function() {
  state <- NULL
  state_name <- 'Kansas'
  url <- covidR::urls[state == state_name][['url']][1]
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  county_tbl <- rvest::html_table(page, fill = TRUE)
  warning('kansas needs selenium')
}

scrape_kentucky <- function() {
  warning('kentucky needs selenium')
}

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

scrape_maryland <- function() {
  warning('requries selenium')
}

scrape_massachusetts <- function() {
  state <- NULL
  state_name <- 'Massachusetts'
  warning('requires selenium')
}

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
  state_data <- tables[[4]]
  update_date <- paste0(format(Sys.Date(),
                               format = '%Y-%m-%d'), ' 2:00 PM')
  update_date <- lubridate::as_datetime(update_date,
                                        format = '%Y-%m-%d %I:%M %p',
                                        tz = timezone)
  d1 <- make_dat(state = state_name, url = url, page = page_raw,
                 county = county_cases$County, cases = county_cases$Cawses,
                 update_date = update_date)
  d2 <- make_dat(state = state_name, url = url, page = page_raw,
                 hospitalized = state_data$Hospitalized,
                 cases = state_data$Number)
  data.table::rbindlist(list(d1, d2), fill = TRUE)
}

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
  warning('needs selenium for county results')
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

scrape_mississippi <- function() {
  state_name <- 'Mississippi'
  page <- get_page('https://msdh.ms.gov/msdhsite/_static/14,0,420.html')
  page_raw <- rvest::html_text(page)
  cases <- rvest::html_node(page, xpath = '//*[@id="pvExplorationHost"]/div/div/exploration/div/explore-canvas-modern/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container-modern[2]/transform/div/div[3]/visual-modern/div/svg/g[1]/text/tspan')
  warning('needs selenium')
}

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

scrape_nevada <- function() {
  state_name <- 'Nevada'
  timezone <- 'America/Los_Angeles'
  url <- 'http://dpbh.nv.gov/coronavirus/'
  page <- get_page(url)
  page_raw <- rvest::html_text(page)
  tbl <- rvest::html_table(page)[[1]]
  update_date <- lubridate::as_datetime(
    gsub('Last updated ', '', tbl[2,1]), format = '%m/%d/%Y, %I:%M %p',
    tz = timezone)
  make_dat(state = state_name, url = url, page = page_raw,
           update_date = update_date, cases = tbl[3, 2],
           presumptive_positive = tbl[4, 2], negative_tests = tbl[5, 2],
           monitored = tbl[9,2])
}

scrape_new_hampshire <- function() {
  warning('County data is in a picture')
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

#' @importFrom magrittr %>%
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
  warning('date_updated will need to be retrieved via selenium')
  make_dat(state = state_name, url = url, page = page_raw,
           county = tbl[['county']], cases = tbl[['Positives']],
           negative_tests = tbl[['Negatives']])
}

script_new_mexico <- function() {
  state <- NULL
  state_name <- 'New Mexico'
  timezone <- 'America/Denver'
  url <- 'https://nmhealth.org/news/alert/2020/3/?view=856'
  warning('url contains a date element, may need to evaluate after march')
  page <- get_page(url)
  page_raw <- rvest::html_text(page)

}

scrape_hattiesburg <- function() {
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