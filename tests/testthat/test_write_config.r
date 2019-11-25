context("Writing TOML config")

config = list(
    "xml" = list(
        first_chunk = "First XML chunk",
        second_chunk = "Second XML chunk"
        ),
    "templates" = list(
        first_subtemplate = "First subtemplate"
        ),
    "defaults" = list(
        third_parameter = list(
            first = 1,
            second = letters[1:3]
            ),
        first_parameter = "one",
        second_parameter = 1:3
        )
    )

test_that("Can parse list and produce TOML structure", {
    toml = c(
        "[xml]",
        "first_chunk = \"First XML chunk\"",
        "second_chunk = \"Second XML chunk\"",
        "",
        "[templates]",
        "first_subtemplate = \"First subtemplate\"",
        "",
        "[defaults]",
        "first_parameter = \"one\"",
        "second_parameter = [ 1, 2, 3 ]",
        "",
        "[defaults.third_parameter]",
        "first = 1",
        "second = [ \"a\", \"b\", \"c\" ]",
        ""
        )

    expect_equal(list2toml(config), toml)
    })


test_that("Writting and reading preserves structure", {
    temp_file = tempfile()
    write_config(config, temp_file)
    toml = RcppTOML::parseTOML(temp_file)
    unlink(temp_file)

    config = sort_named_list(config)
    toml = sort_named_list(toml)

    expect_equal(config, toml)
    })
