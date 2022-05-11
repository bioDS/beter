test_that("Test basic template processing", {
    processed_template = get_template("test_files/test.xml", "test_files/test.toml") 
    premade_template = readLines("test_files/test_processed.xml")

    expect_equal(processed_template, premade_template)
    })
sequences2xml

test_that("processing XML chunks with {{ mustache }} tags", {
    config_file = write_to_temp(list2toml(config))
    template = "{{chunk_one}}\n{{chunk_two}}"
    template_file = write_to_temp(template)
    expected = paste("This is chunk", 1:2)

    processed = get_template(template_file, config_file)

    expect_equal(processed, expected)

    unlink(config_file)
    unlink(template_file)
    })


test_that("XML chunks with the same name are added together", {
    # set up subtemplate
    subtemplate = empty_config
    subtemplate$xml[["chunk"]] = "Subtemplate chunk"
    subtemplate_file = write_to_temp(list2toml(subtemplate))

    # set up config
    config = empty_config
    config$xml[["chunk"]] = "Template chunk"
    config$templates[["subtemplate"]] = subtemplate_file
    config_file = write_to_temp(list2toml(config))

    # setup template
    template = "{{#chunk}}{{.}}\n{{/chunk}}"
    template_file = write_to_temp(template)

    expected = c("Template chunk", "Subtemplate chunk", "")

    expect_equal(get_template(template_file, config_file), expected)

    unlink(subtemplate_file)
    unlink(config_file)
    unlink(template_file)
    })


test_that("XML template with sequences is processed", {
    processed_nexus = get_template(
        "test_files/primates_template.xml",
        alignment = "test_files/primates.nex",
        format = "nexus"
        )
    processed_fasta = get_template(
        "test_files/primates_template.xml",
        alignment = "test_files/primates.fasta",
        format = "fasta"
        )
    expected = readLines("test_files/primates.xml")

    expect_equal(processed_fasta, expected)
    expect_equal(processed_nexus, expected)
    })


test_that("Alignment_id is replaced by input parameter", {
    template = get_template("test_files/primates_template.xml", alignment="test_files/primates.fasta")
    expect_equal(get_alignment_id(template), "primates")
    
    template = get_template(
        "test_files/primates_template.xml",
        alignment = "test_files/primates.fasta",
        parameters = list("alignment_id" = "foo")
        )
    expect_equal(get_alignment_id(template), "foo")
    
    template = get_template(
        "test_files/primates_template.xml",
        alignment = "test_files/primates.fasta",
        parameters = list("datatype"="nucleotide", "alignment_id" = "foo")
        )
    expect_equal(get_alignment_id(template), "foo")
    })


test_that("scientific notation is unrolled", {
    # number that R should, by default, convert to scientific format
    number = 5e+5

    # set up config
    config = empty_config
    config_file = write_to_temp(list2toml(config))


    # setup template
    template = "{{number}}"
    template_file = write_to_temp(template)

    expected = c("500000")

    expect_equal(
        get_template(template_file, config_file, parameters=list("number"=number)),
        expected
        )

    unlink(config_file)
    unlink(template_file)
    })


test_that("scientific notation is unrolled when processing chunk", {
    # number that R should, by default, convert to scientific format
    number = 5e+5

    # set up config
    config = empty_config
    config$xml[["chunk"]] = "{{number}}"
    config$defaults[["number"]] = number
    config_file = write_to_temp(list2toml(config))

    # setup template
    template = "{{{chunk}}}"
    template_file = write_to_temp(template)

    expected = c("500000")

    expect_equal(get_template(template_file, config_file,), expected)

    unlink(config_file)
    unlink(template_file)
    })


test_that("scientific notation is not unrolled when option is set", {
    beter_options(scientific=TRUE)
    on.exit(beter_options(scientific=NULL))

    # number that R should, by default, convert to scientific format
    number = 5e+5

    # set up config
    config = empty_config
    config_file = write_to_temp(list2toml(config))


    # setup template
    template = "{{number}}"
    template_file = write_to_temp(template)

    expected = c("5e+05")

    expect_equal(
        get_template(template_file, config_file, parameters=list("number"=number)),
        expected
        )

    unlink(config_file)
    unlink(template_file)
    })

