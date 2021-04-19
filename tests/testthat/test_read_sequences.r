context("Reading sequence alignment files")


test_fasta = "test_files/test.fasta"
test_nexus = "test_files/test.nex"
primates_fasta = "test_files/primates.fasta"
primates_nexus = "test_files/primates.nex"

test_that("Can read test sequence files", {
    expect_identical(read_fasta(test_fasta), sequences)
    expect_identical(read_nexus(test_nexus), sequences)
    })


test_that("Uses a correct function to read alignment when FORMAT is specified", {
    expect_identical(read_sequences(tempcopy(test_fasta), format="fasta"), sequences)
    expect_identical(read_sequences(tempcopy(test_nexus), format="nexus"), sequences)
    })
 
 
test_that("Uses a correct function to read alignment by guessing format from extension", {
    expect_identical(read_sequences(test_fasta), sequences)
    expect_identical(read_sequences(test_nexus), sequences)
    })


test_that("Correctly reads primate file", {
    seq_fas = read_sequences(primates_fasta)
    seq_nex = read_sequences(primates_nexus)
    
    expect_identical(seq_fas, seq_nex)

    expect_true(all( nchar(seq_fas) == 898))
    expect_true(all( nchar(seq_nex) == 898)) 
    }) 
