library(sbgr)

context("Auth Class")

## check_token <- function() {
##   if (missing(token)) {
##     skip("token not available")
##   }
## }

test_that("Test Auth constructor",{
    expect_is(Auth("fake_token"), "Auth")
    expect_error(Auth(token))
})
