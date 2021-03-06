require(igraph)

set.seed(42)

generate_tree <- function(N = 21, children=4){
    g <- graph.tree(N, children=children)
    labels <- sample(letters, size=N)
    values <- runif(N)+0.5
    
    V(g)$label <- labels
    V(g)$value <- values
    g
}

tree2df <- function(g, labels=V(g)$label, values=V(g)$value){
    A <- get.adjacency(g)    
    dimnames(A) <- list(parent=labels, node=labels)
    
    N <- length(V(g))
    
    # get parents
    nbh <- neighborhood(g, order=3, mode="in")[6:21]
    
    #turn into dataframe
    df <- apply(t(sapply(nbh, rev)), 2, function(x) labels[x])
    df <- cbind(as.data.frame(df), value=values[6:21])
    df
}

g1 <- generate_tree()
df1 <- tree2df(g1)

g2 <- generate_tree()
df2 <- tree2df(g2)

g3 <- generate_tree()
df3 <- tree2df(g3)

g4 <- generate_tree()
df4 <- tree2df(g4)

g5 <- generate_tree()
df5 <- tree2df(g5)

g6 <- generate_tree()
df6 <- tree2df(g6)




#' @param k number of datasets
#' @param seeds seeds for k layouts
#' @param levs number of nodes per hierarchical layer. Each node is randomly assigned to a node in the parent layer. Therefore, the number of nodes in higher layers can be lower.
generateRandomHierData <- function(k, seeds, levs=c(5, 20, 60), addText=TRUE, prefices=c("Hoofdcategorie", "Subcategorie")) {
    dats <- list() # to be filled with k random datasets
    n <- sum(levs)
    for (i in 1:k) {
        if (!missing(seeds)) set.seed(seeds[i])
        if (n > 26) {
            l <- sample(paste0(rep(LETTERS[1:26], each=26), 
                               rep(LETTERS[1:26], times=26)), n)
        } else {
            l <- sample(LETTERS[1:26], n)
        }        
        h1 <- sample(l, levs[1])
        h2 <- sample(setdiff(l, h1), levs[2])
        h3 <- setdiff(l, c(h1,h2))
        
        if (addText) {
            h1 <- paste(prefices[1], h1)
            h2 <- paste(prefices[2], h2)
        }
        
        dats[[i]] <- data.frame(h1=factor(NA, levels=h1),
                                h2=factor(sample(h2, length(h3), replace=TRUE), levels=h2),
                                h3=factor(h3, levels=h3),
                                value = rnorm(length(h3), mean=100, sd=30))
        h2parents <- sample(h1, length(h2), replace=TRUE)
        dats[[i]]$h1 <- factor(h2parents[match(dats[[i]]$h2, h2)], h1)
    }
    dats
}

## function to create fancy levels, e.g. A3b
fancyLevels <- function(dat, index, symbols=c("LETTERS", "numbers", "letters"), sep=".") {
    k <- length(index)
    
    ## assign temporarily numbers to levels
    tempsymb <- 1:1000
    for (i in 1:k) {
        nm <- index[i]
        nl <- length(unique(as.character(dat[[nm]])))
        
        if (i==1) {
            dat[[nm]] <- factor(as.integer(dat[[nm]]), labels=tempsymb[1:nl]) 
        } else{
            dat[[nm]] <- factor(paste(dat[[index[i-1]]], as.vector(unlist(tapply(dat[[nm]], INDEX=list(dat[[index[i-1]]]), function(x)as.character(factor(as.character(x), labels=1:length(unique(x))))))), sep=sep))
        }
    }
    # replace numbers by symbols
    for (i in 1:k) {
        x <- levels(dat[[index[i]]])
        sx <- strsplit(x, split="\\.")
        sx <- lapply(1:i, function(ii) sapply(sx, function(iii)iii[ii]))
        
        sxu <- lapply(sx, unique)
        
        nx <- sapply(sxu, length)
        sxu2 <- mapply(getSymbols, symbols[1:i], nx, SIMPLIFY=FALSE)
        sx2 <- lapply(1:i, function(ii) {
            y <- sxu2[[ii]][match(sx[[ii]], sxu[[ii]])]        
        })
        
        lvls <- do.call("mapply", args=c(sx2, list(FUN="paste", MoreArgs=list(sep=sep))))
        dat[[index[i]]] <- factor(as.integer(dat[[index[i]]]), labels=unique(lvls))
    }
    dat
}
    
#' Function to return vector of sorted symbols
#' 
#' @param s one of "letters", "LETTERS", "numbers1", "numbers0", "numbers". The symbol set for numbers1 is 1:9, and for numbers0 it is 0:9. Numbers is equal to numbers0, except that is starts from 1.
#' @param n number of levels needed
getSymbols <- function(s, n) {
    sn <- switch(s, "letters"=26, "LETTERS"=29, "numbers1"=9, "numbers0"=10, "numbers"=10)
    symb <- switch(s, "letters"=letters, "LETTERS"=LETTERS, "numbers1"=1:9, "numbers0"=0:9, "numbers"=0:9)
    
    if (s=="numbers") {
        ndigit <- which(n <= (sn^(1:4)-1))[1]
        lvls <- c(symb[-1], rep(symb, length.out=n))[1:n]
    } else {
        ndigit <- which(n <= sn^(1:4))[1]
        lvls <- rep(symb, length.out=n)
    }
    
    if (ndigit > 1) for (i in 2:ndigit) {
        lvls <- paste(rep(symb, each=sn^(i-1), length.out=n+1)[-1], lvls, sep="")
    }
    lvls
}


addSymbols <- function(tm, rect.id, symbols="*", fontfamily="sans") {
    require(grid)
    
    symbols <- rep(symbols, length.out=length(rect.id))
    
    row_ids <- match(rect.id, as.character(tm$tm$h3)) 
    
    x <- tm$tm$x0[row_ids]
    y <- tm$tm$y0[row_ids]
    w <- tm$tm$w[row_ids]
    h <- tm$tm$h[row_ids]
    
    grid.text(symbols, x=x+.35*w, y=y+.65*h, gp=gpar(col="blue", cex=2, fontfamily=fontfamily))
}



generateGraph <- function(dat, method="HCP", hue_fraction=0.75) {
    require(treemap)
    require(RColorBrewer)
    
    datcolors <- treepalette(dat, index=c("h1", "h2", "h3"), palette.HCL.options=list(hue_fraction=hue_fraction))
    set1 <- brewer.pal(8, "Set2")
    datcolors$firstcat.color <- set1[as.integer(datcolors$h1)]
    colorname <- ifelse(method=="HCP", "HCL.color", "firstcat.color")
    
    ## create vertex data.frame
    vertices1 <- datcolors[is.na(datcolors$h2), c("h1", colorname)] 
    vertices2 <- datcolors[is.na(datcolors$h3) & !is.na(datcolors$h2), c("h2", colorname)]
    vertices3 <- datcolors[!is.na(datcolors$h3), c("h3", colorname)]
    
    names(vertices1)[1] <- "node"
    names(vertices2)[1] <- "node"
    names(vertices3)[1] <- "node"

    vertices <- do.call(rbind, list(vertices1, vertices2, vertices3))
    vertices$node <- as.character(vertices$node)
    names(vertices)[2] <- "color"
    vertices <- rbind(vertices, 
                      data.frame(node="  ", color="#DDDDDD"))
    
    ## create edges data.frame
    edges <- rbind(unique(data.frame(from="  ", to=as.character(dat$h1))),
                   unique(data.frame(from=as.character(dat$h1), 
                                     to=as.character(dat$h2))),
                   unique(data.frame(from=as.character(dat$h2), 
                                     to=as.character(dat$h3))))
    g <- graph.data.frame(edges, vertices=vertices)
    g
}



plotGraph <- function(dat, method="HCP", seed, hue_fraction=0.75, vertex.label.family="sans") {
    g <- generateGraph(dat, method=method, hue_fraction=hue_fraction)
    set.seed(seed)
    cols <- col2rgb(V(g)$color)
    
    # constrasting labels.
#     light <- apply(cols * c(.299, .587, .114), MARGIN=2, sum) >= 128
#     fontcolors <- ifelse(light, "black", "white")
#     
    require(colorspace)
    cols_hcl <- as(hex2RGB(V(g)$color), "polarLUV")
    
    luminence <- cols_hcl@coords[,1]
    fontcolors <- ifelse(luminence >=65, "black", "white")

#     luminence <- cut(cols_hcl@coords[,1],4)
#     greys <- rev(sequential_hcl(n=4, c.=0, l=c(0,100)))
#     fontcolors <- greys[luminence]
    
    plot(g, layout= layout.kamada.kawai(g), edge.arrow.size=.6, vertex.label.cex=.8, vertex.label.family=vertex.label.family, vertex.label.color=fontcolors)
}

plotBar <- function(dat, method="HCP", hue_fraction=0.5) {
    require(ggplot2)
    require(RColorBrewer)
    
    dat <- dat[order(dat$h1, dat$h2, decreasing=TRUE), ]
    dat$h1 <- factor(as.character(dat$h1), levels=rev(unique(as.character(dat$h1))))
    dat$h2 <- factor(as.character(dat$h2), levels=rev(unique(as.character(dat$h2))))
    dat$h3 <- factor(as.character(dat$h3), levels=rev(unique(as.character(dat$h3))))
    
    datcolors <- treepalette( dat, index=c("h1", "h2", "h3"), 
                              palette.HCL.options = list(hue_fraction=hue_fraction))
    
    dark2 <- brewer.pal(8, "Dark2")
    datcolors$firstcat.color <- dark2[as.integer(datcolors$h1)]
    colorname <- ifelse(method=="HCP", "HCL.color", "firstcat.color")
    
    dat$color <- datcolors[[colorname]][match(dat$h3, datcolors$h3)]
    
    dat$x <- addSpace(dat[, c("h1", "h2", "h3")], fact=1.1)
    dat$x <- max(dat$x) - dat$x
    
    ggplot(dat, aes(x=x, y=value, fill=h3)) +
        geom_bar(stat="identity") + 
        scale_x_continuous("", breaks=dat$x, labels=dat$h3) +
        scale_y_continuous("") +
        scale_fill_manual(values=rev(dat$color)) + coord_flip() + theme_bw() +
        theme(legend.position="none")
}


addSpace <- function(dat, fact=1.10) {
    d <- ncol(dat)
    dat <- lapply(dat, as.integer) 
    diff <- lapply(dat, function(x)x[-1]!=x[-(length(x))])
    diff <- matrix(unlist(diff), ncol=d)
    steps <- floor(log10(apply(diff, MARGIN=1,FUN=function(x)sum(1, as.numeric(x)*(10^(length(x):1))))))
    cumsum(c(0, fact^steps))
}

