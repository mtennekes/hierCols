### survey evaluation
us <- read.csv("user_survey/survey_evaluation.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)
#us <- read.csv("user_survey/survey_evaluation_color_blind.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)

FC <- "Main Branch Colors"
TC <- "Tree Colors"


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
    names(x)[2:3] <- c(FC, TC)
    x
})
s[tc_ind] <- lapply(s[tc_ind], function(x){
    x <- x[, c(1,3,2)]
    names(x)[2:3] <- c(FC, TC)
    x
})
s[ev_ind1] <- lapply(s[ev_ind1], function(x){
    x[,1] <- factor(c(FC, TC, "Indifferent"), levels=c(FC, TC, "Indifferent"))
    x
})
s[ev_ind2] <- lapply(s[ev_ind2], function(x){
    x <- x[c(2, 1, 3), ]
    x[,1] <- factor(c(FC, TC, "Indifferent"), levels=c(FC, TC, "Indifferent"))
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
    names(y) <- c("Correct", "FC", "TC")
    if (nrow(y)==1) y <- rbind(y, data.frame(Correct=2, "FC"=0, "TC"=0))
    names(y) <- c("Correct", FC, TC)
    y
}, t[c(3, 6, 12, 15)], ans2, SIMPLIFY=FALSE)


read_id <- c(2,3,5,6,11,12,14,15,20,22)

library(binom)

t2 <- t
t2[read_id] <- lapply(t2[read_id], function(x){
    n <- colSums(x[,-1])
    est <- unlist(x[1, -1])
    p.est <- unlist(x[1,-1] / n)

    # estimation in percentages
    est.perc <- est / n * 100

    # standard error in percentages
    se <- sqrt(n*p.est*(1-p.est))
    se.perc <- se / n * 100
    names(se.perc) <- paste(names(se.perc), "se", sep=".") 
    
    # confidence interval in percentages    
    res <- binom.confint(est, n, methods="ac")
    lb <- res$lower * n
    ub <- res$upper * n
    lb.perc <- lb / n * 100
    ub.perc <- ub / n * 100
    names(lb.perc) <- paste(names(lb.perc), "lb", sep=".") 
    names(ub.perc) <- paste(names(ub.perc), "ub", sep=".") 
    
     
    c(est.perc, se.perc, lb.perc, ub.perc)
})



u <- as.data.frame(do.call(rbind, t2[read_id]), row.names = NA)
u$viz <- factor(c(rep("Node-link diagram", 4), rep("Treemap", 4), rep("Bar chart", 2)),
                levels=c("Node-link diagram", "Treemap", "Bar chart"))
u$Dataset <- factor(c(1,1,2,2,1,1,2,2,1,2), levels=2:1)#c(1,1,2,2,4,4,3,3,5,6)
#u$Dataset <- factor(c(1,1,2,2,4,4,3,3,5,6))
u$VD <- factor(paste0(u$viz, ", dataset", u$Dataset))
qrel <- "Question about relations"
qoff <- "Question about offspring"

u$read <- factor(c(rep(c(qrel, qoff), 4), rep(qrel, 2)), levels=c(qrel, qoff))


uFC <- u[,-c(2, 4, 6, 8)]
uTC <- u[,-c(1, 3, 5, 7)]
names(uFC)[1:4] <- c("Value", "SE", "lb", "ub")
names(uTC)[1:4] <- c("Value", "SE", "lb", "ub")
uFC$method <- factor(FC, levels=c(FC, TC))
uTC$method <- factor(TC, levels=c(FC, TC))
u <- rbind(uFC, uTC)

require(ggplot2)
require(grid)
require(RColorBrewer)
require(scales)
require(reshape2)
require(plyr)
pal <- brewer.pal(3, "RdYlGn")

pal <- brewer.pal(6, "Dark2")[c(2,6,5)]
library(tabplot)
tabPal <- tablePalettes()
pal <- tabPal$qual$Set1[c(1, 5, 2)]


#pal[2] <- "#AAAAAA"



(g1 <- ggplot(u, aes(x=Dataset, y=Value, color=method, ymax=ub, ymin=lb)) + 
     scale_color_manual("Method", values=pal[-2]) + 
     scale_y_continuous("Percentage correct answers", breaks=seq(0, 100, 20)) + 
     geom_pointrange(size=.8, position=position_dodge(width=-.5)) + 
     facet_grid(viz~read, drop=FALSE) +theme_bw() + coord_flip() +
     theme(panel.grid.major = element_line(colour="gray"), 
            panel.grid.minor = element_blank(), 
            panel.margin = unit(.5, "lines")))


ggsave("./plots/user_study_results.pdf", width=5, height=2.5, scale=1.5)


grob <- ggplotGrob(g1)
grob$grobs[[5]]$children$axis$grobs[[1]]$label <- c("4", "3")
grob$grobs[[6]]$children$axis$grobs[[1]]$label <- c("6", "5")

scale=1.5
pdf("./plots/user_study_results_mod.pdf", width=5*scale, height=2.5*scale)
grid.draw(grob)
dev.off()





ev_ind <- sort(c(ev_ind1, ev_ind2))

v <- t[ev_ind]
Viz <- c(rep("Node-link diagram", 3), rep("Treemap", 3), rep("Bar chart", 3))
Subject <- rep(c("Prettiness", "Interpretation", "Overview"), 3)
v <- mapply(function(x, v, s) {
    x1 <- x[, -3]
    x2 <- x[, -2]
    names(x1)[2] <- "Value"
    names(x2)[2] <- "Value"
    x1$Value <- x1$Value / sum(x1$Value) * 100
    x2$Value <- x2$Value / sum(x2$Value) * 100
    x <- rbind(x1, x2)
    x$Version <- factor(c(rep(1,3),rep(2,3)))
    x$Viz <- factor(v, levels=unique(Viz))
    x$Subject <- factor(s, levels=unique(Subject))
    x
}, v, Viz, Subject, SIMPLIFY=FALSE)

v <- do.call(rbind, v)


v2 <- aggregate(v$Value, by=v[, c("antwoord", "Viz", "Subject")], FUN=function(x)sum(x)/2)

v3 <- v2
v3$antwoord <- factor(as.character(v3$antwoord), levels=c("Indifferent", FC, TC))

v3$Viz <- factor(as.character(v3$Viz), levels=rev(levels(v3$Viz)))


v3pos <- v3[v3$antwoord!=FC, ]
v3pos$x[v3pos$antwoord=="Indifferent"] <- v3pos$x[v3pos$antwoord=="Indifferent"] / 2

v3neg <- v3[v3$antwoord!=TC, ]
v3neg$x[v3neg$antwoord=="Indifferent"] <- v3neg$x[v3neg$antwoord=="Indifferent"] / 2
v3neg$x <- -v3neg$x 

levels(v3pos$antwoord)
levels(v3neg$antwoord)

v3posText <- v3pos
v3posText$label <- paste0(ifelse(v3posText$antwoord=="Indifferent", round(v3pos$x*2), round(v3pos$x)), "%")
v3posText$x[v3posText$antwoord==TC] <- v3posText$x[v3posText$antwoord==TC]/2 + v3posText$x[v3posText$antwoord=="Indifferent"]
v3posText$x[v3posText$antwoord=="Indifferent"] <- 0

v3negText <- v3neg
v3negText$label <- paste0(-round(v3neg$x), "%")
v3negText$label[v3negText$antwoord=="Indifferent"] <- ""
v3negText$x[v3negText$antwoord==FC] <- v3negText$x[v3negText$antwoord==FC]/2 + v3negText$x[v3negText$antwoord=="Indifferent"]
v3negText$x[v3negText$antwoord=="Indifferent"] <- 0

swap12 <- function(x) {
    y <- as.integer(x)
    y[y!=3] <- 3-y[y!=3]
    y
}


(g2 <- ggplot() + aes(Viz, x, fill=antwoord, order=antwoord) +
    geom_bar(data=v3pos, stat="identity") +
    geom_bar(data=v3neg, stat="identity") +
     geom_text(data=v3posText, aes(label=label), size=3) +
     geom_text(data=v3negText, aes(label=label), size=3) +
     scale_x_discrete("") +
    scale_fill_manual(values=pal[c(2,1,3)], "Preference", breaks=c(FC, "Indifferent", TC)) + 
    scale_y_continuous(name = "",
                       labels = paste0(seq(-80, 80, 20), "%"),
                       limits = c(-80, 80),
                       breaks = seq(-80, 80, 20)) +
    facet_wrap(~Subject, ncol=1) + coord_flip() + 
     theme_bw() +
     theme(panel.margin = unit(.5, "lines")))
    
ggsave("./plots/user_study_results2.pdf", g2, width=4, height=2.5, scale=1.7)



###### tried plotting help question next to reading question

# (g1b <- ggplot(u, aes(x=Dataset, y=Value, color=method, ymax=ub, ymin=lb)) + 
#      scale_color_manual("Method", values=pal[-2]) + 
#      scale_y_continuous("Percentage correct answers", breaks=seq(0, 100, 20)) + 
#      geom_pointrange(size=.8, position=position_dodge(width=-.5)) + 
#      facet_grid(viz~read, drop=FALSE) +theme_bw() + coord_flip() +
#      theme(panel.grid.major = element_line(colour="gray"), 
#            panel.grid.minor = element_blank(), 
#            panel.margin = unit(.5, "lines"),
#            legend.position = "none",
#            strip.text.y = element_blank()))
# 
# 
# strip.remover <- function(ggp, what="x") {
#     require(gridExtra)
#     
#     zeroGrob <- function() {
#         g0 <- grob(name="NULL")
#         class(g0) <- c("zeroGrob",class(g0))
#         g0
#     }
#     
#     g <- ggplotGrob(ggp)
#     
#     g$grobs <- lapply(g$grob, function(gr) {
#         if (any(grepl(paste0("strip.text.", what),names(gr$children)))) {
#             gr$children[[grep("strip.background",names(gr$children))]] <- zeroGrob()
#             gr$children[[grep("strip.text",names(gr$children))]] <- zeroGrob()
#         }
#         return(gr)
#     }
#     )
#     
#     class(g) = c("arrange", "ggplot",class(g)) 
#     g
# }
# 
# grob <- strip.remover(g1b, "y")
# grob$grobs[[5]]$children$axis$grobs[[1]]$label <- c("4", "3")
# grob$grobs[[6]]$children$axis$grobs[[1]]$label <- c("6", "5")
# 
# grid.draw(grob)



u <- t[c(4, 7, 13, 16, 21, 23)]
u <- mapply(function(x, n, v) {
    x$viz <- factor(n, levels=c("Node-link diagram", "Treemap", "Bar chart"))
    x$version <- factor(v, levels=1:2)
    x[[2]] <- x[[2]]/sum(x[[2]])*100
    x[[3]] <- x[[3]]/sum(x[[3]])*100
    x        
}, u, rep(c("Node-link diagram", "Treemap", "Bar chart"), each=2), c(1, 2, 1, 2, 1, 2), SIMPLIFY=FALSE)

u <- do.call(rbind, u)
uT <- u[,-2]
names(uT)[2] <- "value"
uF <- u[,-3]
names(uF)[2] <- "value"
uT$method <- factor(TC, levels=c(FC, TC))
uF$method <- factor(FC, levels=c(FC, TC))
u <- rbind(uT, uF)

u2 <- ddply(u, .(antwoord, viz, method), function(x)mean(x$value))
u2$subject <- "Colors helped"

levels(u2$antwoord) <- c("SD", "D", "N", "A", "SA")

(g3 <- ggplot(u2, aes(x=antwoord, fill=method, y=V1)) +
    geom_bar(stat="identity", position="dodge", width=.8) +
    scale_fill_manual("Method", values=pal[-2]) + 
    scale_y_continuous("Percentage") + 
    scale_x_discrete("Answer") +
    facet_grid(viz~subject) +theme_bw() +
    theme(panel.grid.major = element_line(colour="gray"), 
          panel.grid.minor = element_blank(), 
          panel.margin = unit(.5, "lines")))

library(gridExtra)
g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}
legend <- g_legend(g3)
plot(legend)
g1 <- g1 + theme(legend.position="none")
grob <- ggplotGrob(g1)
grob$grobs[[5]]$children$axis$grobs[[1]]$label <- c("4", "3")
grob$grobs[[6]]$children$axis$grobs[[1]]$label <- c("6", "5")

g3 <- g3 + theme(legend.position="none")


scale=1.5
pdf("./plots/user_study_results_mod2.pdf", width=5*scale, height=3*scale)
grid.arrange(arrangeGrob(grob, g3, ncol=2, widths=c(4.25/6, 1.75/6)), 
             arrangeGrob(legend, ncol=2, widths=c(1.5/6, 4.5/6)),
             ncol=1, heights=c(5/6, 1/6))
dev.off()
