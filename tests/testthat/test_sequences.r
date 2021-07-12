context("Processing alignment files")


sequences_xml = paste0(
    "<data id=\"alignment\" dataType=\"nucleotide\">\n",
    "    <sequence taxon=\"seq1\">ACTGACTG</sequence>\n",
    "    <sequence taxon=\"seq2\">CTGACTGA</sequence>\n",
    "    <sequence taxon=\"seq3\">TGACTGAC</sequence>\n",
    "</data>"
    )


sequences_xml_random = paste0(
    "<data id=\"alignment\" dataType=\"standard\">\n",
    "    <sequence taxon=\"seq_1\">1</sequence>\n",
    "    <sequence taxon=\"seq_2\">1</sequence>\n",
    "    <sequence taxon=\"seq_3\">1</sequence>\n",
    "    <userDataType",
    " id=\"StandardDataType\"",
    " spec=\"beast.evolution.datatype.StandardData\"",
    " nrOfStates=\"1\" />\n",
    "</data>"
    )



test_that("Guessing datatype and values from data", {
    expect_identical( random_sequences(1,1,1), list("seq_1"="1"))
    expect_identical( random_sequences(2,2,10), list("seq_1"=strrep(2, 10), "seq_2"=strrep(2, 10)) )
    expect_equal( guess_number_of_states(random_sequences(1)), 1)
    expect_equal( guess_number_of_states(random_sequences(1:2)), 2)
    expect_equal( guess_number_of_states(random_sequences(1:4)), 4)
    expect_equal( guess_number_of_states(sequences), 4)

    expect_equal( guess_datatype(random_sequences()), "standard")
    expect_equal( guess_datatype(sequences), "nucleotide")
    
    expect_error(guess_datatype("foo"), "Unrecognized data type: F, O")
    expect_error(guess_datatype("bar"), "Unrecognized data type: B, A, R")
    expect_error(guess_datatype("baz"), "Unrecognized data type: B, A, Z")
    })


test_that("Can convert sequences to xml", {
    expect_identical(sequences2xml(sequences), sequences_xml)
    expect_identical(sequences2xml(random_sequences(1, 3, 1)), sequences_xml_random)
    })


test_that("Data id is replaced by input parameter", {
    expect_identical(get_alignment_id(sequences2xml(sequences)), "alignment")
    expect_identical(get_alignment_id(sequences2xml(sequences, list(alignment_id="foo"))), "foo")
    })
