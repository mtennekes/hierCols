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





#levs <- c(6, 20, 74) # number of hierarchical categories


dats <- list()

### group 1
levs <- c(5, 20, 60)

set.seed(20140114)
l <- sample(paste0(rep(LETTERS[1:26], each=26), 
                   rep(LETTERS[1:26], times=26)), sum(levs))

h1 <- sample(l, levs[1])
h2 <- sample(setdiff(l, h1), levs[2])
h3 <- setdiff(l, c(h1,h2))

h1 <- paste("Hoofdcategorie", h1)
h2 <- paste("Subcategorie", h2)

dats[[1]] <- data.frame(h1=factor(NA, levels=h1),
                        h2=factor(sample(h2, length(h3), replace=TRUE), levels=h2),
                        h3=factor(h3, levels=h3),
                        value = rnorm(length(h3), mean=100, sd=30))

h2parents <- sample(h1, length(h2), replace=TRUE)
dats[[1]]$h1 <- h2parents[match(dats[[1]]$h2, h2)]

### group 2
set.seed(20140115)
l <- sample(paste0(rep(LETTERS[1:26], each=26), 
                   rep(LETTERS[1:26], times=26)), sum(levs))

h1 <- sample(l, levs[1])
h2 <- sample(setdiff(l, h1), levs[2])
h3 <- setdiff(l, c(h1,h2))

h1 <- paste("Hoofdcategorie", h1)
h2 <- paste("Subcategorie", h2)

dats[[2]] <- data.frame(h1=factor(NA, levels=h1),
                   h2=factor(sample(h2, length(h3), replace=TRUE), levels=h2),
                   h3=factor(h3, levels=h3),
                   value = rnorm(length(h3), mean=100, sd=30))

h2parents <- sample(h1, length(h2), replace=TRUE)
dats[[2]]$h1 <- h2parents[match(dats[[2]]$h2, h2)]




addSymbols <- function(tm, rect.id, symbols="*") {
    require(grid)
    
    symbols <- rep(symbols, length.out=length(rect.id))
    
    row_ids <- match(rect.id, as.character(tm$tm$h3)) 
    
    x <- tm$tm$x0[row_ids]
    y <- tm$tm$y0[row_ids]
    w <- tm$tm$w[row_ids]
    h <- tm$tm$h[row_ids]
    
    grid.text(symbols, x=x+.35*w, y=y+.65*h, gp=gpar(col="blue", cex=2))
}

