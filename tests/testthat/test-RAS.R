#' Add sample matrix and vectors with known RAS solutions
m0 <- matrix(c(3,4,2,5,1,4),nrow=3,ncol=2,byrow=TRUE)
t_c <- c(5,14)
t_r <- c(6,7,6)

#' Check that RAS is working
test_that("RAS works", {
  m <- RAS(m0,t_r,t_c)
  show_failure(expect_false(isTRUE(all.equal(m,m0))))
})