library(FielDHub)
# Testing split_vectors()
x1 <- 1:10
x2 <- 1:15
x3 <- 19:201
test_that("split_vectors is a list of vectors", {
  expect_equal(split_vectors(x1, c(3,5,2)), list(1:3, 4:8, 9:10))
  expect_equal(split_vectors(x2, c(5,6,4)), list(1:5, 6:11, 12:15))
  expect_equal(split_vectors(x3, c(75,80,28)), list(19:93, 94:173, 174:201))
})

# Testing automatically_cuts()
r1 <- sample(c(rep(0, 9), 1))
r2 <- sample(c(rep(0, 9), 1))
r3 <- sample(c(rep(0, 9), 1))
r4 <- sample(c(rep(0, 9), 1))
r5 <- sample(c(rep(0, 9), 1))
m1_r <- matrix(data = c(r1, r2, r3, r4, r5), nrow = 5, ncol = 10, byrow = TRUE)
m1_c <- matrix(data = c(r1, r2, r3, r4, r5), nrow = 10, ncol = 5, byrow = FALSE)
r6 <- sample(c(rep(0, 9), 1))
r7 <- sample(c(rep(0, 9), 1))
r8 <- sample(c(rep(0, 9), 1))
r9 <- sample(c(rep(0, 9), 1))
r10 <- sample(c(rep(0, 9), 1))
m2_r <- matrix(data = c(r6, r7, r8, r9, r10), nrow = 5, ncol = 10, byrow = TRUE)
m2_c <- matrix(data = c(r6, r7, r8, r9, r10), nrow = 10, ncol = 5, byrow = FALSE)
M_data_r <- rbind(m2_r, m1_r)
M_data_c <- cbind(m2_c, m1_c)
test_that("automatically_cuts is a list of vectors", {
  expect_equal(automatically_cuts(data = M_data_r, planter_mov = "serpentine", 
                                  stacked = "By Row", dim_data = c(45, 45)),
               list(bks = list(1:5, 6:10), cuts = c(5,10)))
  expect_equal(automatically_cuts(data = M_data_r, planter_mov = "cartesian", 
                                  stacked = "By Row", dim_data = c(45, 45)),
               list(bks = list(1:5, 6:10), cuts = c(5,10)))
  expect_equal(automatically_cuts(data = M_data_r, planter_mov = "serpentine", 
                                  stacked = "By Row", dim_data = c(45, 25, 20)),
               list(bks = list(1:5, 6:8, 9:10), cuts = c(5,8,10)))
  expect_equal(automatically_cuts(data = M_data_r, planter_mov = "cartesian", 
                                  stacked = "By Row", dim_data = c(45, 25, 20)),
               list(bks = list(1:5, 6:8, 9:10), cuts = c(5,8,10)))
  expect_equal(automatically_cuts(data = M_data_r, planter_mov = "serpentine", 
                                  stacked = "By Row", dim_data = c(45, 27, 18)),
               list(bks = list(1:5, 6:8, 9:10), cuts = c(5,8,10)))
  expect_equal(automatically_cuts(data = M_data_r, planter_mov = "cartesian", 
                                  stacked = "By Row", dim_data = c(45, 27, 18)),
               list(bks = list(1:5, 6:8, 9:10), cuts = c(5,8,10)))
  expect_equal(automatically_cuts(data = M_data_c, planter_mov = "serpentine", 
                                  stacked = "By Column", dim_data = c(45, 45)),
               c(5,10))
  expect_equal(automatically_cuts(data = M_data_c, planter_mov = "cartesian", 
                                  stacked = "By Column", dim_data = c(45, 45)),
               c(5,10))
  expect_equal(automatically_cuts(data = M_data_c, planter_mov = "serpentine", 
                                  stacked = "By Column", dim_data = c(45, 27, 18)),
               c(5,8,10))
  expect_equal(automatically_cuts(data = M_data_c, planter_mov = "cartesian", 
                                  stacked = "By Column", dim_data = c(45, 27, 18)),
               c(5,8,10))
})

# Testing automatically_cuts()
seriePlot.numbers(plot.number = c(101, 1001, 2001), reps = 3, l = 3, t = 101)
plot.number1 <- c(101, 1001, 2001)
test_that("seriePlot.numbers is a list of vectors", {
  expect_equal(seriePlot.numbers(plot.number = plot.number1, reps = 3, l = 3, t = 100), 
               list(c(101, 201, 301), c(1001, 1101, 1201), c(2001, 2101, 2201)))
  expect_equal(seriePlot.numbers(plot.number = 101, reps = 3, l = 3, t = 100), 
               list(c(101, 201, 301), c(101, 201, 301), c(101, 201, 301)))
  expect_equal(seriePlot.numbers(plot.number = 101, reps = 3, l = 1, t = 100), 
               list(c(101, 201, 301)))
  expect_equal(seriePlot.numbers(plot.number = 1, reps = 3, l = 1, t = 105), 
               list(c(1, 106, 211)))
  expect_equal(seriePlot.numbers(plot.number = 1, reps = 1, l = 1, t = 20), 
               list(1))
  expect_equal(seriePlot.numbers(plot.number = 1, reps = 2, l = 1, t = 20), 
               list(c(1, 101)))
  expect_equal(seriePlot.numbers(plot.number = 1000, reps = 3, l = 1, t = 20), 
               list(c(1000, 1100, 1200)))
})
