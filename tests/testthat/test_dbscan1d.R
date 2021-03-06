library(dbscan1d)

expect_equal(dbscan1d(c(1, 2, 3,  6, 7, 8, 12, 13, 14), eps=1, minPoints=2), c(1,1,1,2,2,2,3,3,3), eps=1, minPoints=2)
expect_equal(dbscan1d(c(1, 2, 3,  6, 7, 8, 12, 13, 14), eps=1, minPoints=2), dbscan1d(c(1, 2, 3,  6, 7, 8, 12, 13, 14), eps=1, minPoints=2, FALSE))
expect_equal(dbscan1d(c(12, 2, 3,  6, 7, 8, 1, 13, 14), eps=1, minPoints=2), c(3,1,1,2,2,2,1,3,3), eps=1, minPoints=2)
expect_equal(dbscan1d(c(12, 7, 3,  6, 8, 1, 13, 14, 2), eps=1, minPoints=2), c(3,2,1,2,2,1,3,3,1), eps=1, minPoints=2)
expect_equal(dbscan1d(c(12, 7, 3, 13,  6, 8, 1, 14, 2), eps=1, minPoints=2), c(3,2,1,3,2,2, 1,3,1), eps=1, minPoints=2)
expect_equal(sort(dbscan1d(c(12, 7, 3, 13,  6, 8, 1, 14, 2), eps=1, minPoints=2)), c(1,1,1,2,2,2, 3,3,3))
expect_equal(sort(dbscan1d(c(12, 7, 3, 13,  6, 8, 1, 14, 2), eps=1, minPoints=2), FALSE), c(1,1,1,2,2,2, 3,3,3))
expect_equal(sort(dbscan1d(c(12, 7, 3, 13,  6, 8, 1, 14, 2), eps=1, minPoints=2), FALSE), dbscan1d(c(1, 2, 3,  6, 7, 8, 12, 13, 14), eps=1, minPoints=2))
expect_equal(dbscan1d(c(-12, 7, 3, -13,  6, 8, 1, -14, 2), eps=1, minPoints=2), c(1,3,2,1,3,3, 2,1,2), eps=1, minPoints=2)
