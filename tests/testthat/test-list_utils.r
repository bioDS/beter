test_that("can sort a simple named list:", {
    list1 = list("a"=1, "b"=2)
    list2 = list("b"=2, "a"=1)

    expect_equal(list1, sort_named_list(list2))
    })


test_that("can sort structured named list:", {
    list1 = list("a"=1, "b"=list("c"=2, "d"=1))
    list2 = list("b"=list("d"=1, "c"=2), "a"=1)

    expect_equal(list1, sort_named_list(list2))
    })


test_that("can sort structured named list with an empty list:", {
    list1 = list("a"=1, "b"=list())
    list2 = list("b"=list(), "a"=1)

    expect_equal(list1, sort_named_list(list2))
    })


test_that("fails when item is not list:", {
    expect_error(sort_named_list(1))
    expect_error(sort_named_list(c("a"=1,"b"=2)))
    expect_error(sort_named_list("ahoj"))
    })


test_that("can distinguish empty and non-empty list", {
    expect_true(is.empty(list()))
    expect_true(is.empty(NULL))
    expect_false(is.empty(list("a"=1, "b"=2)))
    })


test_that("formatting structured list without scientific notation preserves the list", {
    lst = list(
        "a" = 1,
        "b" = list(
            "c" = " 2 ",
            "d" = 3
            ),
        "e" = 4
        )

    expected = lst
    expected[["a"]] = "1"
    expected[["b"]][["d"]] = "3"
    expected[["e"]] = "4"

    expect_identical(format_list(lst), expected)
    })


test_that("formatting list with scientific notation unrolls it", {
    lst = list(
        "a" = 5e5,
        "b" = list(
            "c" = " 5e5 ",
            "d" = 2.05e-5
            ),
        "e" = 4
        )

    expected = lst
    expected[["a"]] = format(expected[["a"]], scientific=FALSE, trim=TRUE)
    expected[["b"]][["d"]] = format(expected[["b"]][["d"]], scientific=FALSE, trim=TRUE)
    expected[["e"]] = "4"

    expect_identical(format_list(lst), expected)
    })


test_that("empty list is handled correctly", {
    lst = list()

    expect_identical(format_list(lst), lst)
    })


test_that("no numeric/list items are handled correctly", {
    lst = list("a"=1, "b"=2)

    expect_identical(format_list(lst), lapply(lst, as.character))

    lst = list("a"="a", "b"="b")

    expect_identical(format_list(lst), lst)
    })
