# What I want to do?
# Test that a list2toml produce an expected output
# Test that list2toml
# Test different types of TOML config

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
