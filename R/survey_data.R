# groepnummer (we hebben twee verschillende groepen)
group <- 1
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





set.seed(20140111)
levs <- c(6, 20, 74) # number of hierarchical categories
levs <- c(4, 16, 40)

l <- sample(paste0(rep(LETTERS[1:26], each=26), 
                   rep(LETTERS[1:26], times=26)), sum(levs))


h1 <- sample(l, levs[1])
h2 <- sample(setdiff(l, h1), levs[2])
h3 <- setdiff(l, c(h1,h2))

h1 <- paste("Hoofdcategorie", h1)
h2 <- paste("Subcategorie", h2)

dat <- data.frame(h1=factor(NA, levels=h1),
                  h2=factor(sample(h2, length(h3), replace=TRUE), levels=h2),
                  h3=factor(h3, levels=h3),
                  value = rnorm(length(h3), mean=100, sd=30))

h2parents <- sample(h1, length(h2), replace=TRUE)
dat$h1 <- h2parents[match(dat$h2, h2)]


