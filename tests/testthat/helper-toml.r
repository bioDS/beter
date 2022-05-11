# complex config
complex_lst = list(
        "xml" = list(
            first_chunk = "First XML chunk",
            second_chunk = "Second XML chunk"
            ),
        "templates" = list(),
        "defaults" = list(
            third_parameter = list(
                first = 1,
                second = letters[1:3]
                ),
            first_parameter = "one",
            second_parameter = 1:3
            )
        )


# Processed complex config
complex_toml = c(
        "[xml]",
        "first_chunk = \"First XML chunk\"",
        "second_chunk = \"Second XML chunk\"",
        "",
        "[templates]",
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
