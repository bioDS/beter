test_that("flat list is converted to TOML", {
    lst = list("one"="one", "two"="2", "three"=3, "four"=1:4)
    expected = c(
        "one = \"one\"",
        "two = \"2\"",
        "three = 3",
        "four = [ 1, 2, 3, 4 ]",
        ""
        )

    expect_identical(list2toml(lst), expected)
    })


test_that("structured list is converted to TOML", {
    lst = list(
        "one" = list(
            "a" = 1,
            "b" = 1:2
            ),
        "two" = list(
            "c" = "one",
            "d" = c("one", "two")
            )
        )

    expected = c(
        "[one]",
        "a = 1",
        "b = [ 1, 2 ]",
        "",
        "[two]",
        "c = \"one\"",
        "d = [ \"one\", \"two\" ]",
        ""
        )

    expect_identical(list2toml(lst), expected)
    })


test_that("complex structured list with subtables is converted to TOML", {
    expect_equal(list2toml(complex_lst), complex_toml)
    })


test_that("writting and reading preserves structure", {
    temp_file = write_to_temp(list2toml(complex_lst))
    toml = RcppTOML::parseTOML(temp_file)
    unlink(temp_file)

    # RcppTOML::parseTOML does not preserve the original structure,
    # instead the TOML tables (sublists) are sorted according to their name
    complex_lst = sort_named_list(complex_lst)
    toml = sort_named_list(toml)

    expect_equal(complex_lst, toml)
    })


test_that("scientific format is written in a proper form", {
    lst = list("scientific" = 5e+05)
    expected = c("scientific = 5e5", "")

    expect_identical(list2toml(lst), expected)
    })


test_that("scientific format can be parsed by TOML parser", {
    lst = list("scientific" = 5e5)

    temp_file = write_to_temp(list2toml(lst))
    toml = RcppTOML::parseTOML(temp_file)
    toml = toml[1] # object returned by toml has some extra attributes
    unlink(temp_file)

    expect_equal(lst, toml)
    })
