# Helper functions
get_template = function(
    template, config=NULL, alignment=NULL, format=NULL, parameters=NULL
    ){
    temp = tempfile()
    on.exit(unlink(temp))
    process_template(template, temp, config, alignment, format, parameters)
    readLines(temp)
    }


write_to_temp = function(text){
    tempfile = tempfile()
    writeLines(text, tempfile)
    tempfile
    }

# Empty config -- to make creating subtemplates easier
empty_config = list(
    "xml" = list(),
    "templates" = list(),
    "defaults" = list()
    )

# A simple config
config = list(
    "xml" = list(
        "chunk_one" = "This is chunk {{one}}",
        "chunk_two" = "This is chunk {{two}}"
        ),
    "templates" = list(),
    "defaults" = list(
        "one" = 1,
        "two" = 2
        )
    )

processed = list(
    "chunk_one" = "This is chunk 1",
    "chunk_two" = "This is chunk 2"
        )

# Test process_xml_chunks ------------------------------------------------------------------------ #
test_that("Can render XML chunks in isolation", {
    xml_chunks = config$xml
    parameters = config$defaults
    expected = processed
    expect_equal(process_xml_chunks(xml_chunks, parameters), expected)
    })


# Test parse_config ------------------------------------------------------------------------------ #
test_that("Can parse simple config", {
    config_file = write_to_temp(list2toml(config))
    expected = list(
        "data" = processed,
        "defaults" = config$defaults
        )
    expect_equal(parse_config(config_file), expected)

    unlink(config_file)
    })


test_that("Can parse config with a subtemplates", {
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


test_that("Additional parameters overwrite defaults", {
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


# Test process_template -------------------------------------------------------------------------- #
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
