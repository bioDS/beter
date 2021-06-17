context("Reading sequence alignment files")


test_fasta = "test_files/uppercase.fasta"
test_nexus = "test_files/uppercase.nex"
primates_fasta = "test_files/primates.fasta"
primates_nexus = "test_files/primates.nex"

# TODO split this according to the format?
# TODO dynamically create these tests?
test_that("Can read test sequence files", {
    expect_identical(read_fasta(test_fasta), sequences)
    expect_identical(read_nexus(test_nexus), sequences)
    })

test_that("Case of sequences is not modified", {
    expect_identical(read_fasta("test_files/uppercase.fasta"), sequences_uppercase)
    expect_identical(read_fasta("test_files/lowercase.fasta"), sequences_lowercase)
    expect_identical(read_fasta("test_files/mixed.fasta"), sequences_mixed)

    expect_identical(read_nexus("test_files/uppercase.nex"), sequences_uppercase)
    expect_identical(read_nexus("test_files/lowercase.nex"), sequences_lowercase)
    expect_identical(read_nexus("test_files/mixed.nex"), sequences_mixed)
    })

test_that("Uses a correct function to read alignment when FORMAT is specified", {
    expect_identical(read_sequences(tempcopy(test_fasta), format="fasta"), sequences)
    expect_identical(read_sequences(tempcopy(test_nexus), format="nexus"), sequences)
    })
 
 
test_that("Uses a correct function to read alignment by guessing format from extension", {
    expect_identical(read_sequences(test_fasta), sequences)
    expect_identical(read_sequences(test_nexus), sequences)
    })

test_that("Throws an error if unsupported file format is provided", {
    expect_error(read_sequences(test_fasta, format=""), "Unsupported format: ")
    expect_error(read_sequences(test_fasta, format="blargh"), "Unsupported format: blargh")
    expect_error(read_sequences("test."), "Unrecognized extension: ")
    expect_error(read_sequences("test.blargh"), "Unrecognized extension: blargh")
    })


test_that("Correctly reads primate file", {
    seq_fas = read_sequences(primates_fasta)
    seq_nex = read_sequences(primates_nexus)
    
    expect_identical(seq_fas, seq_nex)

    expect_true(all( nchar(seq_fas) == 898))
    expect_true(all( nchar(seq_nex) == 898)) 
    }) 
