get_alignment_id = function(xml){
    if(length(xml) > 1)
        xml = grep("data id=", xml, value=TRUE)[1]

    sub("<data id=\"(.*)\" dataType.*", "\\1", xml)
    }
