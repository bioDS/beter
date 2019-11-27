# simulate.r
#
# Performs tree and sequence simulation together with BEAST analysis
# and its evaluation.
library("argparser")

import::from("src/beast.r", process_beast_template)
import::from("src/utils.r", settmpwd, mkdir, run_beast)
import::from("src/seqgen.r", seqgen_sampling)

main = function(){
    ntax = 20
    nsamples = 1
    repeats = 1

    mkdir("intermediate/sampling")

    # generate template to sample from prior
    parameters = list(
        "sample_from_prior" = "true",
        "taxa" = make_taxa_names(ntax),
        "chain_length" = 1000 * nsamples # number of samples
        )

    template_beast = "templates/BEAST.xml"
    template_seqgen = "templates/seqgen.xml"

    config = "templates/MHKY.toml"

    process_beast_template(
        template_beast,
        config,
        "intermediate/sampling/SAMPLING.xml",
        parameters
        )
    # run beast to sample from prior
    run_beast("intermediate/sampling/SAMPLING.xml")
    # simulate data for every sample n times
    seqgen_sampling(
    "intermediate/sampling/SAMPLING.log",
    "intermediate/sampling/SAMPLING.trees",
    template_seqgen,
    template_beast,
    config,
    config,
    "intermediate/test.xml",
    list(seqlength="20000"),
    repeats = repeats
    )
    # merge with another template without sampling from prior
    # run those beast analyses
    # evaluate
    }

make_taxa_names = function(ntax){
    make.unique(rep(LETTERS, length.out=ntax), sep="")
    }


if(!interactive()){
    main()
    }
