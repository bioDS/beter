test_that("can render XML chunks in isolation", {
    xml_chunks = config$xml
    parameters = config$defaults
    expected = processed
    expect_equal(process_xml_chunks(xml_chunks, parameters), expected)
    })


test_that("can parse simple config", {
    config_file = write_to_temp(list2toml(config))
    expected = list(
        "data" = processed,
        "defaults" = config$defaults
        )
    expect_equal(parse_config(config_file), expected)

    unlink(config_file)
    })


test_that("can parse config with a subtemplates", {
    # set up subtemplate
    subtemplate = empty_config
    subtemplate$xml[["chunk_three"]] = "This is chunk {{three}}"
    subtemplate$defaults[["three"]] = 3
    subtemplate_file = write_to_temp(list2toml(subtemplate))

    # set up config
    config$templates[["subtemplate"]] = subtemplate_file
    config_file = write_to_temp(list2toml(config))

    # set up expected results
    expected = list(
        "data" = c(processed, "chunk_three" = "This is chunk 3"),
        "defaults" = config$defaults
        )

    expect_equal(parse_config(config_file), expected)

    # clean
    unlink(subtemplate_file)
    unlink(config_file)
    })


test_that("XML chunks with the same name do merge", {
    # set up subtemplate
    subtemplate = empty_config
    subtemplate$xml[["chunk_two"]] = "Just another chunk in the wall!"
    subtemplate_file = write_to_temp(list2toml(subtemplate))

    # set up config
    config$templates[["subtemplate"]] = subtemplate_file
    config_file = write_to_temp(list2toml(config))

    # set up expected results
    expected = list(
        "data" = processed,
        "defaults" = config$defaults
        )
    expected$data[["chunk_two"]] = c(expected$data[["chunk_two"]], subtemplate$xml[["chunk_two"]])

    expect_equal(parse_config(config_file), expected)

    # clean
    unlink(subtemplate_file)
    unlink(config_file)
    })


test_that("additional parameters overwrite defaults", {
    config_file = write_to_temp(list2toml(config))

    expected = list(
        "data" = processed,
        "defaults" = config$defaults
        )
    expected$data$chunk_one = "This is chunk 3"
    expected$defaults$one = 3

    expect_equal(parse_config(config_file, list("one"=3)), expected)

    unlink(config_file)
    })
