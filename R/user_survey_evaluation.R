### survey evaluation
us <- read.csv("user_survey/survey_evaluation.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

us
which(us$vraag!="")

id <- rep(0, nrow(us))
id[which(us$vraag!="")] <- 1
cumsum(id)

s <- split(us[,-1], cumsum(id))
names(s) <- c("cb", 
        "g1.a", "g1.b", "g1.eval", 
        "g2.a", "g2.b", "g2.eval",
        "g.mooi", "g.inter", "g.over",
        "t1.a", "t1.b", "t1.eval",
        "t2.a", "t2.b", "t2.eval",
        "t.mooi", "t.inter", "t.over",
        "b1", "b1.eval",
        "b2", "b2.eval",
        "b.mooi", "b.inter", "b.over")
        
s <- lapply(s, function(x){
    x$antwoord <- factor(x$antwoord, levels=unique(x$antwoord))
    x
})

fc_ind <- c(2:4, 14:16, 20:21)
tc_ind <- c(5:7, 11:13, 22:23)
ev_ind1 <- c(8:10, 24:26)
ev_ind2 <- 17:19

s[fc_ind] <- lapply(s[fc_ind], function(x){
    names(x)[2:3] <- c("TreeColors", "FirstColors")
    x
})
s[tc_ind] <- lapply(s[tc_ind], function(x){
    x <- x[, c(1,3,2)]
    names(x)[2:3] <- c("TreeColors", "FirstColors")
    x
})
s[ev_ind1] <- lapply(s[ev_ind1], function(x){
    x[,1] <- factor(c("FirstColors", "TreeColors", "indifferent"), levels=c("FirstColors", "TreeColors", "indifferent"))
    x
})
s[ev_ind2] <- lapply(s[ev_ind2], function(x){
    x[,1] <- factor(c("TreeColors", "FirstColors", "indifferent"), levels=c("FirstColors", "TreeColors", "indifferent"))
    x
})

s[2:7]
s[8:10]

s[11:16]


# reverse version 2 answers
q <- c("g.mooi", "g.inter", "g.over", "t.mooi", "t.inter", "t.over", "b.mooi", "b.inter", "b.over")
s[q] <- lapply(s[q], )

