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


names(t)
read_id <- c(2,3,5,6,11,12,14,15,20,22)
t[read_id] <- lapply(t[read_id], function(x){
    x$FirstColors <- x$FirstColors / sum(x$FirstColors) * 100
    x$TreeColors <- x$TreeColors / sum(x$TreeColors) * 100
    x
})


u <- do.call(rbind, t[read_id])
u$viz <- factor(c(rep("Graph", 8), rep("Treemap", 8), rep("Bar chart", 4)), 
                levels=c("Graph", "Treemap", "Bar chart"))
u$Version <- factor(c(rep(1, 4), rep(2, 4), rep(1, 4), rep(2, 4), rep(1, 2), rep(2, 2)), levels=1:2)
qrel <- "Ques. about relations"
qoff <- "Ques. about offspring"

u$read <- factor(c(rep(c(qrel, qrel, qoff, qoff), 4), rep(qrel, 4)), levels=c(qrel, qoff))

uFC <- u[,-3]
uTC <- u[,-2]
names(uFC)[2] <- "Value"
names(uTC)[2] <- "Value"
uFC$method <- factor("First Colors", levels=c("First Colors", "Tree Colors"))
uTC$method <- factor("Tree Colors", levels=c("First Colors", "Tree Colors"))
u <- rbind(uFC, uTC)

require(ggplot2)
require(grid)

(g1 <- ggplot(u[u$Correct==1,], aes(x=Version, y=Value, color=method, shape=method)) + 
     scale_shape("Method") + scale_color_discrete("Method") + 
    scale_y_continuous("Percentage correct answers") + geom_point() + facet_grid(viz~read) + coord_flip() + theme(panel.margin = unit(.5, "lines")))

ggsave("./plots/user_study_results.pdf", g1, width=4, height=2.5, scale=1.5)



v <- u[u$Correct==1,]
v$Value[v$method=="TreeColors"] <- v$Value[v$method=="TreeColors"] - v$Value[v$method=="FirstColors"]
v <- v[v$method=="TreeColors", 1:5]

v$viz_method <- paste(as.character(v$viz), as.character(v$read))

ggplot(v, aes(x=version, y=Value, fill=viz)) + geom_point() +
    facet_grid(viz_method~.) + coord_flip()






