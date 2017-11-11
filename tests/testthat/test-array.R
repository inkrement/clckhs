context("array")

library(DBI, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(dbplyr, warn.conflicts=F)

test_that("reading & writing array columns", {
  conn <- dbConnect(clckhs::clickhouse(), host="localhost")
  df <- as.data.frame(data_frame(x=list(c(1,3,5),c(1,2))))
  dbWriteTable(conn, "test", df, overwrite=T)
  r <- dbReadTable(conn, "test")
  expect_equal(r, df)
  dbDisconnect(conn)
})

#TODO: fix bug in clickhouse-cpp first
#test_that("nullable array columns", {
#  conn <- dbConnect(clckhs::clickhouse(), host="localhost")
#  df <- data.frame(x=I(list(c(1,3,5),c(1,2))))
#  attributes(df$x) <- NULL
#  dbWriteTable(conn, "test", df, overwrite=T)
#  r <- dbReadTable(conn, "test")
#  expect_equal(r, df)
#  dbDisconnect(conn)
#})

test_that("array columns with empty entries", {
  conn <- dbConnect(clckhs::clickhouse(), host="localhost")
  df <- as.data.frame(data_frame(x=list(c(1,2,3),as.numeric(c()),c(4,5))))
  dbWriteTable(conn, "test", df, overwrite=T)
  r <- dbReadTable(conn, "test")
  expect_equal(r, df)
  dbDisconnect(conn)
})