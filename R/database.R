#' @title       Establish a Database Connection
#' @description Using environemntal variables, establish a database connection
#'              if no arguments are passed to it. Otherwise database connection
#'              parameters can be established. The database driver used by this
#'              function is \code{\link[RPostgres]{Postgres}()}.
#' @param       dbname the name of the database to connect
#' @param       port the port to connect to the database on
#' @param       host the host name of the database
#' @param       user the user to connect as
#' @param       pass the password of the user to connect as
#' @param       retry the number of times to retry before giving up
#' @return      a database connection object
#' @export
getDatabaseConnection <- function(dbname = NULL, port = NULL, host = NULL,
                                  user = NULL, pass = NULL, retry = 5) { # {{{2
  if (is.null(host)) {
    if (is.null(Sys.getenv('DB_HOST'))) {
      host <- 'localhost'
    } else {
      host <- Sys.getenv('DB_HOST')
    }
  }
  if (is.null(user)) {
    if (is.null(Sys.getenv('DB_USER'))) {
      user <- 'worker'
    } else {
      user <- Sys.getenv('DB_USER')
    }
  }
  if (is.null(pass)) {
    if (is.null(Sys.getenv('DB_PASS'))) {
      pass <- 'SparklyBannana86'
    } else {
      pass <- Sys.getenv('DB_PASS')
    }
  }
  if (is.null(dbname)) {
    if (is.null(Sys.getenv('DB_NAME'))) {
      dbname <- 'postgres'
    } else {
      dbname <- Sys.getenv('DB_NAME')
    }
  }
  if (is.null(port)) {
    if (is.null(Sys.getenv('DB_PORT'))) {
      port <- '5432'
    } else {
      port <- Sys.getenv('DB_PORT')
    }

  }
  con <- tryCatch({
    con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                          host = host, user = user, password = pass,
                          dbname = dbname)
  }, error = function(e) {
    warning(gettextf(paste('Connection to host "%s" failed:', Sys.time()),
                     host))
    invisible(NULL)
  })
  if (is.null(con) && retry > 0) {
    Sys.sleep(5)
    con <- getDatabaseConnection(dbname, port, host, user, pass, retry - 1)
  }

  con
}

#' @title       retrieve a database table as a data.table
#' @description sends a query to retrieve the specified schema.table from a
#'              database connection
#' @param       con the database connection object
#' @param       tbl the name of the table
#' @param       schema the name of the schema
#' @param       dt bool indicating whether to return a data.table or data.frame
#' @return      either a \code{data.table} or \code{data.frame}
#' @export
getDbAsDt <- function(con, tbl, schema, dt = TRUE) {
  schema <- match.arg(schema)
  query <- sprintf('SELECT * FROM %s.%s', schema, tbl)
  dat <- RPostgres::dbGetQuery(con, query)
  if (dt) {
    return(data.table::data.table(dat))
  } else {
    return(dat)
  }
} # 2}}} 1}}}
# writeTable() {{{1 -----------------------------------------------------------
#' @title      wrapper function around RPostgres::dbWriteTable
#' @description
#' @export
writeTable <- function(x, con, tbl, schema, ...) {
  RPostgres::dbWriteTable(con,
                          DBI::Id(schema = schema,
                                  table = tbl), x, row.names = FALSE, ...)
}
