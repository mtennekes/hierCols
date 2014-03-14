### survey evaluation
us <- read.csv("user_survey/survey_evaluation.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)


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
    names(x)[2:3] <- c("FirstColors", "TreeColors")
    x
})
s[tc_ind] <- lapply(s[tc_ind], function(x){
    x <- x[, c(1,3,2)]
    names(x)[2:3] <- c("FirstColors", "TreeColors")
    x
})
s[ev_ind1] <- lapply(s[ev_ind1], function(x){
    x[,1] <- factor(c("FirstColors", "TreeColors", "indifferent"), levels=c("FirstColors", "TreeColors", "indifferent"))
    x
})
s[ev_ind2] <- lapply(s[ev_ind2], function(x){
    x <- x[c(2, 1, 3), ]
    x[,1] <- factor(c("FirstColors", "TreeColors", "indifferent"), levels=c("FirstColors", "TreeColors", "indifferent"))
    x
})



ans1 <- list(
    g1 = c(JP=1, JK=2, JKP=2),
    g2 = c(KL=1, KJ=2, KJL=2),
    t1 = c(HZ_AF=1, HZ_SD=2, HZ_KL=2, HZ_SDKL=2, HZ_AFSD=2, HZ_AFSDKL=2),
    t2 = c(JM_PQ=1, JM_HD=2, JM_EP=2, JM_HDEP=2, JM_HDEPPQ=2),
    b1 = c(UT_EV=1, UT_XX=2, UT_GG=2, UT_EVGG=2, UT_XXEV=2, UT_XXEVGG=2),
    b2 = c(MX_IV=1, MX_QT=2, MX_DR=2, MX_QTIVDR=2))
ans2 <- c(
    g1 = "G_QRMKF",
    g2 = "B_OQ",
    t1 = "SA_5",
    t2 = "AJ_7")


t <- s
t[c(2, 5, 11, 14, 20, 22)] <- mapply(FUN=function(x, a){
    grp <- a[match(x$antwoord, names(a))]
    grp[is.na(grp)] <- 2
    grp <- factor(as.character(grp), levels=1:2)
    y <- aggregate(x[, -1], by=list(grp), sum)
    names(y)[1] <- "Correct"
    y
}, t[c(2, 5, 11, 14, 20, 22)], ans1, SIMPLIFY=FALSE)

t[c(3, 6, 12, 15)] <- mapply(FUN=function(x, a){
    grp <- rep(2, nrow(x))
    grp[x$antwoord==a] <- 1
    grp <- factor(as.character(grp), levels=1:2)
    y <- aggregate(x[, -1], by=list(grp), sum)
    names(y)[1] <- "Correct"
    if (nrow(y)==1) y <- rbind(y, data.frame(Correct=2, FirstColors=0, TreeColors=0))
    y
}, t[c(3, 6, 12, 15)], ans2, SIMPLIFY=FALSE)


read_id <- c(2,3,5,6,11,12,14,15,20,22)


t2 <- t
t2[read_id] <- lapply(t2[read_id], function(x){
    n <- colSums(x[,-1])
    p.est <- unlist(x[1,-1] / n)
    se <- sqrt(n*p.est*(1-p.est))
    
    
    est <- unlist(x[1, -1])
    
    est.perc <- est / n * 100
    se.perc <- se / n * 100
    names(se.perc) <- paste(names(se.perc), "se", sep=".") 
    
    c(est.perc, se.perc)
})



u <- as.data.frame(do.call(rbind, t2[read_id]), row.names = NA)
u$viz <- factor(c(rep("Graph", 4), rep("Treemap", 4), rep("Bar chart", 2)), 
                levels=c("Graph", "Treemap", "Bar chart"))
u$Version <- factor(c(rep(1, 2), rep(2, 2), rep(1, 2), rep(2, 2), 1, 2), levels=1:2)
qrel <- "Ques. about relations"
qoff <- "Ques. about offspring"

u$read <- factor(c(rep(c(qrel, qoff), 4), rep(qrel, 2)), levels=c(qrel, qoff))


uFC <- u[,-c(2, 4)]
uTC <- u[,-c(1, 3)]
names(uFC)[1:2] <- c("Value", "SE")
names(uTC)[1:2] <- c("Value", "SE")
uFC$method <- factor("First Colors", levels=c("First Colors", "Tree Colors"))
uTC$method <- factor("Tree Colors", levels=c("First Colors", "Tree Colors"))
u <- rbind(uFC, uTC)

require(ggplot2)
require(grid)

(g1 <- ggplot(u, aes(x=Version, y=Value, color=method, shape=method)) + 
     scale_shape("Method") + scale_color_discrete("Method") + 
     scale_y_continuous("Percentage correct answers") + geom_pointrange() + 
     facet_grid(viz~read) + coord_flip() + theme(panel.margin = unit(.5, "lines")))

ggsave("./plots/user_study_results.pdf", g1, width=4, height=2.5, scale=1.5)


(g2 <- ggplot(u, aes(x=Version, y=Value, color=method, shape=method, ymax=Value+SE, ymin=Value-SE)) + 
     scale_shape("Method") + scale_color_discrete("Method") + 
     scale_y_continuous("Percentage correct answers") + geom_pointrange() + 
     facet_grid(viz~read) + coord_flip() + theme(panel.margin = unit(.5, "lines")))

ggsave("./plots/user_study_results2.pdf", g2, width=4, height=2.5, scale=1.5)






## chi square
lapply(t[read_id], function(x)list(x, chisq.test(x[,-1])))

m <- t[[11]][,-1]

n <- sum(m[,2])
p.est <- m[1,2] / n
sqrt(p.est*(1-p.est)/n)*n

se <- sqrt(n*p.est*(1-p.est))

