context("Processing alignment files")

# define variables that are used across tests:
sequences = list(
    seq1 = "ACTGACTG",
    seq2 = "CTGACTGA",
    seq3 = "TGACTGAC"
    )


fasta_file = "test_files/test.fasta"
nexus_file = "test_files/test.nex"

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
    

random_sequences = function(states=1:9, nseq=10, length=10){
    seqs = replicate(
        nseq,
        paste0(sample(as.character(states), size=length, replace=TRUE), collapse=""),
        simplify=FALSE
        )
    names(seqs) = paste0("seq_", 1:nseq)
    seqs
    }


test_that("Guessing datatype and values from data", {
    expect_identical( random_sequences(1,1,1), list("seq_1"="1"))
    expect_identical( random_sequences(2,2,10), list("seq_1"=strrep(2, 10), "seq_2"=strrep(2, 10)) )
    expect_equal( guess_number_of_states(random_sequences(1)), 1)
    expect_equal( guess_number_of_states(random_sequences(1:2)), 2)
    expect_equal( guess_number_of_states(random_sequences(1:4)), 4)
    expect_equal( guess_number_of_states(sequences), 4)

    expect_equal( guess_datatype(random_sequences()), "standard")
    expect_equal( guess_datatype(sequences), "nucleotide")
    })


test_that("Can convert sequences to xml", {
    expect_identical(sequences2xml(sequences), sequences_xml)
    expect_identical(sequences2xml(random_sequences(1, 3, 1)), sequences_xml_random)
    })
