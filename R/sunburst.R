library(rjson)
to_list <- function(data, palette, depth=1){
    n <- paste0("n", depth)
    l <- list()
    if (depth > 5) 
        return(l)
    
    s <- split(data, data[[n]])
    for (code in names(s)){
        children <- to_list(s[[code]], palette, depth+1)
        if (length(children)){
            l[[code]] <- list(name=code, color=unname(palette[code]), children=children)
        } else{
            l[[code]] <- list(name=code, color=unname(palette[code]), size=s[[code]]$turn2011)
        }
    }
    unname(l)
}

tp <- treepalette(dG, paste0("n", 2:5), palette.HCL.options=palette.HCL.optionsImp)
tp$id <- NA
tp$id <- as.character(tp$id)
for (depth in paste0("n", 5:2)){
    idx <- is.na(tp$id)
    tp$id[idx] <- as.character(tp[[depth]])[idx]
}
rownames(tp) <- tp$id
palette = tp$HCL.color
names(palette) <- tp$id

writeLines(toJSON(to_list(dG, palette)[[1]]), "sunburst.json")
