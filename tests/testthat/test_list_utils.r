context("Testing various list utilities")


test_that("Can sort a simple named list:", {
    list1 = list("a"=1, "b"=2)
    list2 = list("b"=2, "a"=1)

    expect_equal(list1, sort_named_list(list2))
    })


test_that("Can sort structured named list:", {
    list1 = list("a"=1, "b"=list("c"=2, "d"=1))
    list2 = list("b"=list("d"=1, "c"=2), "a"=1)

    expect_equal(list1, sort_named_list(list2))
    })


test_that("fails when item is not list:", {
    expect_error(sort_named_list(1))
    expect_error(sort_named_list(c("a"=1,"b"=2)))
    expect_error(sort_named_list("ahoj"))
    })


test_that("Can distinguish empty and non-empty list", {
    expect_true(is.empty(list()))
    expect_true(is.empty(NULL))
    expect_false(is.empty(list("a"=1, "b"=2)))
    })
