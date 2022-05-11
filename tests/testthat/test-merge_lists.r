list1 = list(
    "foo" = 1,
    "bar" = c(2, 3)
    )
list2 = list(
    "bar" = 4,
    "baz" = 5
    )

test_that("additive merge with both existing parameters", {
    expect_equal(add(list1, list2), list("foo"=1, "bar"=c(2,3,4), "baz"=5))
    })


test_that("additive merge with a single missing parameter", {
    expect_equal(add(list1, NULL), list1)
    expect_equal(add(list1, list()), list1)

    expect_equal(add(NULL, list1), list1)
    expect_equal(add(list(), list1), list1)
    })


test_that("additive merge with both missing parameters", {
    expect_equal(add(NULL, NULL), list())
    expect_equal(add(list(), list()), list())
    })


test_that("additive merge with a non-list argument", {
    expect_error(add(list(), NA))
    expect_error(add(matrix(), list()))
    expect_error(add(c(), new.env()))
    })


test_that("non-additive merge with both existing parameters", {
    expect_equal(merge(list1, list2), list("foo"=1, "bar"=4,"baz"=5))
    })


test_that("non-additive merge with a single missing parameter", {
    expect_equal(merge(list1, NULL), list1)
    expect_equal(merge(NULL, list1), list1)
    expect_equal(merge(list1, list()), list1)
    expect_equal(merge(list(), list1), list1)
    })


test_that("non-additive merge with both missing parameters", {
    expect_equal(merge(NULL, NULL), list())
    expect_equal(merge(list(), list()), list())
    })


test_that("non-additive merge with a non-list argument", {
    expect_error(merge(list(), NA))
    expect_error(merge(matrix(), list()))
    expect_error(merge(c(), new.env()))
    })
