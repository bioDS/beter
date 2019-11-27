# seqgen.r
#
# simulate sequences based on parameter values
require("ape")
require("tools")
library("magrittr")

import::from("beast.r", merge, process_beast_template)
import::from("utils.r", run_beast)

seqgen = function(
    seqgen_template,
    merge_template,
    seqgen_config,
    merge_config,
    output,
    parameters = list(),
    repeats = 1
    ){
    merge_xml_temp_path = tempfile("merge_template", fileext=".xml")
    seqgen_xml_temp_path = tempfile("seqgen", fileext=".xml")
    output = normalize_path(output)

    process_beast_template(
        merge_template,
        merge_config,
        merge_xml_temp_path,
        parameters
        )
    parameters[["mergewith"]] = merge_xml_temp_path

    for(i in seq_len(repeats)){
        repeat_output = repeat_path(output, i) # some temp path
        parameters[["output"]] = repeat_output
        process_beast_template(
            seqgen_template,
            seqgen_config,
            seqgen_xml_temp_path,
            parameters
            )
        run_beast(seqgen_xml_temp_path)
        }
    }


seqgen_sampling = function(
    log,
    trees,
    seqgen_template,
    merge_template,
    seqgen_config,
    merge_config,
    output,
    parameters = list(),
    repeats = 1
    ){
    log = read_log(log)
    trees = ape::read.nexus(trees)[-1]

    if(nrow(log) != length(trees))
        stop("ERROR: rows of log should be identical to the number of trees")

    for(i in seq_along(trees)){
        variables = to_list(log[i,])
        variables[["tree"]] = ape::write.tree(trees[[i]], "")
        variables[["taxa"]] = trees[[i]]$tip.label
        variables = merge(variables, parameters)

        sampling_output = repeat_path(output, i)
        seqgen(
            seqgen_template,
            merge_template,
            seqgen_config,
            merge_config,
            sampling_output,
            variables,
            repeats
            )
        }
    }


normalize_path = function(x){
    wd = getwd()
    file.path(wd, x)
    }


repeat_path = function(path, i){
    paste(tools::file_path_sans_ext(path), i, tools::file_ext(path), sep=".")
    }


read_log = function(log){
    log = read.table(log, header=TRUE, stringsAsFactors=FALSE)
    log = log[-1,] # first values are starting positions
    bad_columns = c("Sample", "posterior", "likelihood", "prior")
    log = log[, !colnames(log) %in% bad_columns]
    log
    }


to_list = function(vec){
    # as.list(vec) would be enough if not for multidimensional parameters
    names = names(vec)
    dotnames = grep("\\.[0-9]+$", names)
    dotvec = vec[dotnames]
    vec = vec[-dotnames]
    dotlist = to_list_dotvec(dotvec)
    list = as.list(vec)
    return(c(vec, dotlist))
    }


to_list_dotvec = function(dotvec){
    unique_dotnames = unique(sub("\\.[0-9]+$", "", names(dotvec)))
    dotlist = lapply(unique_dotnames, to_list_dotname, dotvec)
    names(dotlist) = unique_dotnames
    dotlist
    }


to_list_dotname = function(name, dotvec){
    match = unlist(dotvec[grep(name, names(dotvec))])
    order = strsplit(names(match), split=".", fixed=TRUE)
    order = lapply(order, getElement, 2)
    order = as.numeric(unlist(order))
    names(match) = NULL
    match[order]
    }
