#' Class ClickhouseResult
#'
#' Clickhouse's query results class.  This classes encapsulates the result of an SQL
#' statement (either \code{select} or not).
#'
#' @export
#' @keywords internal
setClass("ClickhouseResult",
  contains = "DBIResult",
  slots = list(
    sql = "character",
    env = "environment",
    conn = "ClickhouseConnection",
    ptr = "externalptr",
    Int64 = "character",
    toUTF8 = "logical"
  )
)

#' @rdname ClickhouseResult-class
#' @export
setMethod("dbFetch", signature = "ClickhouseResult", definition = function(res, n = -1, ...) {
  if (length(n) > 1) stop("n must be integer")
  if (is.infinite(n)) n <- -1
  if (n != as.integer(n) || (n < 0 && n != -1)) {
    stop("n must be a positive integer, -1 or Inf")
  }
  ret <- fetch(res@ptr, n)
  ret <- convert_Int64(ret, res@Int64)
  ret <- convert_Decimal(ret)

  if(res@toUTF8 == TRUE) ret <- encode_UTF(ret)

  return(ret)
})

#' @importFrom bit64 as.integer64
convert_Int64 <- function(df, Int64) {
  if (Int64 == "character") return(df)
  int64Types <- c('Int64', 'UInt64', 'Nullable(Int64)', 'Nullable(UInt64)')
  toConvert <- which(attr(df, 'data.type') %in% int64Types)
  if(length(toConvert) > 0){
  as_Int64 <- switch(Int64,
                      integer = as.integer,
                      numeric = as.numeric,
                      integer64 = as.integer64
                     )
  df[toConvert] <- suppressWarnings(lapply(df[toConvert], as_Int64))
  return(df)}else{
    return(df)
  }
}

convert_Decimal <- function(df) {
# identify the columns by index that need changing
  toConvert <- which(grepl("Decimal", attr(df, 'data.type')))

# get the scales
  parse_scale <- function(str){
    x <- strsplit(str, ",")[[1]][[2]]
    x <- substr(x,1,nchar(x)-1)
    x <- as.numeric(x)
    return(as.numeric(x))
  }

  for(column_i in toConvert) {
    raw_type <- attr(df, 'data.type')[[column_i]]
    scale <- parse_scale(raw_type)
    # forces String to decimal representation
    df[column_i] <- lapply(df[column_i], as.numeric)
    df[column_i] <- df[column_i] / (10^scale)
  }
  return(df)
}

encode_UTF <- function(df){
  toConvert <- which(attr(df, "data.type") %in% c("String", "FixedString", "Nullable(String)", "Nullable(FixedString)"))

  if(length(toConvert) > 0){
    df[toConvert] <- suppressWarnings(lapply(df[toConvert], function(x) enc2utf8(x)))
  }
  return(df)
}


#' @rdname ClickhouseResult-class
#' @export
setMethod("dbClearResult", "ClickhouseResult", definition = function(res, ...) {
  if (!validPtr(res@ptr)) {
    warning("Result has already been cleared.")
  } else {
    clearResult(res@ptr)
  }
  invisible(TRUE)
})

#' @rdname ClickhouseResult-class
#' @export
setMethod("dbHasCompleted", "ClickhouseResult", definition = function(res, ...) {
  hasCompleted(res@ptr)
})

#' @rdname ClickhouseResult-class
#' @inheritParams DBI::dbGetStatement
#' @export
setMethod("dbGetStatement", "ClickhouseResult", function(res, ...) {
  getStatement(res@ptr)
})

#' @rdname ClickhouseResult-class
#' @export
setMethod("dbIsValid", "ClickhouseResult", function(dbObj, ...) {
  validPtr(dbObj@ptr)
})

#' @rdname ClickhouseResult-class
#' @inheritParams DBI::dbGetRowCount
#' @export
setMethod("dbGetRowCount", "ClickhouseResult", function(res, ...) {
  getRowCount(res@ptr)
})

#' @rdname ClickhouseResult-class
#' @export
setMethod("dbGetRowsAffected", "ClickhouseResult", definition = function(res, ...) {
  getRowsAffected(res@ptr)
})

#' @rdname ClickhouseResult-class
#' @export
setMethod("dbColumnInfo", "ClickhouseResult", definition = function(res, ...) {
  df <- dbFetch(res, 0)
  data.frame(
    name = colnames(df),
    field.type = resultTypes(res@ptr),
    data.type = sapply(df, class)
  )
})
