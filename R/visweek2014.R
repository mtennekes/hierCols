library(devtools); load_all("../treemap/pkg")
library(RColorBrewer)
library(data.table)
library(colorspace)
library(grid)
library(reshape2)
library(ggplot2)
source("./R/treeplots.R")
source("./R/survey_data.R")

data(business)

## mooi voorbeeld voor in paper (meerdere evenwichten)
treegraph(business, index=c("NACE1", "NACE2", "NACE3"), show.labels=FALSE, vertex.layout=igraph::layout.fruchterman.reingold)

treegraph(business, index=c("NACE1", "NACE2", "NACE3"), show.labels=FALSE, vertex.layout=igraph::layout.auto)




# show permutations
for (i in 2:20) cat(i, ":", spread(i), "\n")


## use business sector F as example for dissociated category names
# hd <- business[as.integer(business$NACE1)==6, c("NACE2", "NACE3", "NACE4", "turnover")]
# hd <- fancyLevels(hd, index=names(hd)[1:3])

set.seed(20140217)
hd <- random.hierarchical.data(method="random.arcs", nodes.per.layer=c(3, 12, 25))



# plot for method description for dividing hue range
palette.HCL.options <- list(hue_start=0, hue_end=360, hue_spread=TRUE,
                            hue_fraction=0.75, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)

dat <- treepalette(hd[,1:3], palette.HCL.options=palette.HCL.options, prepare.dat=TRUE, return.parameters=TRUE)


#dat$label <- with(dat, paste(as.character(L1), as.character(L2), as.character(L3), sep="."))
#dat$label <- gsub(".NA", "", dat$label, fixed=TRUE)

dat$l <- ifelse(!is.na(dat$index3), 3, ifelse(!is.na(dat$index2), 2, 1))

dat$label <- ifelse(dat$l==1, as.character(dat$index1), ifelse(dat$l==2, as.character(dat$index2), as.character(dat$index3)))
    

rem1 <- as.list(as.data.frame(apply(dat[dat$l==1, c("HCL.hue_lb", "HCL.hue_ub")], MARGIN=1,FUN=function(x)x)))
rem2 <- as.list(as.data.frame(apply(dat[dat$l==2, c("HCL.hue_lb", "HCL.hue_ub")], MARGIN=1,FUN=function(x)x)))
rem3 <- as.list(as.data.frame(apply(dat[dat$l==3, c("HCL.hue_lb", "HCL.hue_ub")], MARGIN=1,FUN=function(x)x)))

inverse <- function(x) {
    y <- c(0, sort(unlist(x)), 360)
    split(y, rep(1:(length(y)/2), each=2))
}

cuts1 <- inverse(rem1)
cuts2 <- inverse(rem2)
cuts3 <- inverse(rem3)
borders1 <- c(dat$HCL.hue_lb[dat$l==1], dat$HCL.hue_ub[dat$l==1])
borders2 <- c(dat$HCL.hue_lb[dat$l==2], dat$HCL.hue_ub[dat$l==2])
labs1 <- dat$HCL.H[dat$l==1]
names(labs1) <- dat$label[dat$l==1]
labs2 <- dat$HCL.H[dat$l==2]
names(labs2) <- dat$label[dat$l==2]
labs3 <- dat$HCL.H[dat$l==3]
names(labs3) <- dat$label[dat$l==3]
labs3 <- labs3[-c(17:19,20:22)]  ## to prevent overplotting

gridsize <- 401#5e2+1

pdf("plots/hcl_method2.pdf", width=8, height=8.4)
#png("plots/hcl_method2.png", width=2000, height=2100, res=265)

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=4, ncol=2, heights = unit(rep(1,4), c("null", "lines", "null", "lines")))))

cellplot(1,1, e={
    cat(convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE), "\n",
        convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE), "\n")
    drawHCL(gridsize=gridsize, marks=c(0, 120, 240),
            marks.dashed=c(rep(F, sum(dat$l==1))))
})

cellplot(2,1, e={
    grid.text("(a) Hue range equally split in three", x=0.05, y=unit(0.5, "lines"), just="left")
})

cellplot(1,2, e={
    drawHCL(gridsize=gridsize, marks=c(0, 120, 240, borders1),
            marks.dashed=c(rep(F, sum(dat$l==1)), rep(T, 2*sum(dat$l==1))),
            cuts=cuts1, labels=labs1)
})
cellplot(2,2, e={
    grid.text("(b) Middle fractions assigned to first layer nodes", x=0.05, y=unit(0.5, "lines"), just="left")
})


cellplot(3,1, e={
    drawHCL(gridsize=gridsize, marks=c(37.5, 60, 82.5, 135+(18*(0:4)), 285, 315, borders1), 
            marks.dashed=F, 
            cuts=cuts2, labels=labs2, labels.cex=0.7)
})
cellplot(4,1, e={
    grid.text("(c) Recursively applied to second layer nodes", x=0.05, y=unit(0.5, "lines"), just="left")
})

cellplot(3,2, e={
    drawHCL(gridsize=gridsize, 
            marks.dashed=T, 
            cuts=cuts3, labels=labs3, labels.cex=0.6)
})
cellplot(4,2, e={
    grid.text("(d) Recursively applied to third layer nodes", x=0.05, y=unit(0.5, "lines"), just="left")
})

dev.off()



## 
pdf("plots/HCPgraph.pdf", width=6, height=6)
set.seed(20140203)
#treegraph(dat, index=c("index1", "index2", "index3"), show.labels=TRUE, vertex.layout=igraph::layout.auto,vertex.size=4, palette.HCL.options=palette.HCL.options)
treegraph(dat, index=c("index1", "index2", "index3"), show.labels=TRUE, vertex.size=4, palette.HCL.options=palette.HCL.options, directed=FALSE)
dev.off()





## use Statline data for real world application
data(business)
source("./R/statline_business_data.R")

str(business)

treemap(d, index=c("n1", "n2", "n3", "n4"), vSize="emp2010")
treemap(d[as.integer(d$n1)==3,], index=c("n2", "n3", "n4"), vSize="emp2010", overlap.labels=.1)

pdf("plots/TMbusiness.pdf", width=9, height=7)
treemap(d[as.integer(d$n1)==7,], index=c("n2", "n3", "n4"), vSize="turn2011", overlap.labels=.1, bg.labels=255, title="")
dev.off()

treegraph(d, index=c("n1", "n2", "n3", "n4"))


###### experiment with hue fraction
set.seed(20140207)
dat4childs <- random.hierarchical.data(method="random.arcs", nodes.per.layer=c(4,16,64), value.generator=rnorm, value.generator.args=list(mean=10, sd=2),labels.prefix=c("Main", "Sub", ""))

dat3childs <- random.hierarchical.data(method="random.arcs", nodes.per.layer=c(3,9,27), value.generator=rnorm, value.generator.args=list(mean=10, sd=2))

dat5childs <- random.hierarchical.data(method="random.arcs", nodes.per.layer=c(5,25,125), value.generator=rnorm, value.generator.args=list(mean=10, sd=2))


palette.HCL.options <- list(hue_start=0, hue_end=360, hue_spread=TRUE,
                            hue_fraction=.75, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)




fs <- c(0.25, .5, .75, 1)#seq(0, 1, length.out=11)
fs_format <- format(fs)

nr <- 2
nc <- 2

pdf("plots/Treemaps_hue.pdf", width=10, height=10)
dat <- dat4childs

grid.newpage()
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
pushViewport(viewport(layout=grid.layout(nr,nc)))

ir <- ic <- 1
for (i in 1:length(fs)) {
    treemap(dat, index=names(dat)[1:(ncol(dat)-1)], vSize="x", palette.HCL.options=list(hue_fraction=fs[i]), vp=vplayout(ir,ic), title=paste("Fraction =", fs_format[i]), overlap=0.1, bg.labels=255)
    ic <- ic + 1
    if (ic > nc) {
        ic <- 1
        ir <- ir + 1
    }
}
dev.off()


pdf("plots/Graph_hue.pdf", width=8, height=8)
dat2 <- dat3childs

par(mfrow=c(2,2))
for (i in 1:length(fs)) {
    set.seed(20140212)
    treegraph(dat2, index=c("index1", "index2", "index3"), directed=FALSE, show.labels=TRUE, vertex.layout=igraph::layout.auto,vertex.size=8, vertex.label.dist=0.6,palette.HCL.options=list(hue_fraction=fs[i]), mai=c(.2,.2,.2,.2))
    title(paste("\n                               Fraction =", fs_format[i]), font.main=1)
}
dev.off()


## teaser
set.seed(20140216)
#dat2 <- random.hierarchical.data(method="random", number.children=3, value.generator=rnorm, value.generator.args=list(mean=3), labels.prefix=c("Main", "Sub", ""))
dat2 <- random.hierarchical.data(method="random.arcs", nodes.per.layer=c(5,20, 40), value.generator=rnorm, value.generator.args=list(mean=3), labels.prefix=c("Main", "Sub", ""))

pdf("plots/Treemap_teaser.pdf", width=8, height=8)
treemap(dat2, index=c("index1", "index2", "index3"), vSize="x", palette.HCL.options=list(hue_fraction=.5), title="", overlap=.1, fontsize.labels=14, bg.labels=255)
dev.off()

pdf("plots/Graph_teaser.pdf", width=8, height=8)
set.seed(20140213)
p <- treegraph(dat2, index=c("index1", "index2", "index3"), directed=FALSE, show.labels=TRUE, vertex.size=6, vertex.layout=igraph::layout.fruchterman.reingold, palette.HCL.options=list(hue_fraction=.95), edge.arrow.size=1)
dev.off()


## permutation order
n <- 3:12
nr <- 5
nc <- 18

palette.HCL.options2 <- list(hue_start=120, hue_end=240, hue_spread=TRUE,
                            hue_fraction=1, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)

colorlist <- list()
for (i in 1:length(n)) {
    dat3 <- data.frame(index1=factor(1:n[i]))
    dat3 <- treepalette(dat3, "index1", prepare.dat=FALSE, palette.HCL.options=palette.HCL.options2)
    colorlist[[i]] <- dat3$HCL.color
}
    
colors <- matrix(NA, nrow=nr, ncol=nc)
colors[1, 2:4] <- colorlist[[1]]
colors[2, 2:5] <- colorlist[[2]]
colors[3, 2:6] <- colorlist[[3]]
colors[4, 2:7] <- colorlist[[4]]
colors[5, 2:8] <- colorlist[[5]]

colors[1, 6:17] <- colorlist[[10]]
colors[2, 7:17] <- colorlist[[9]]
colors[3, 8:17] <- colorlist[[8]]
colors[4, 9:17] <- colorlist[[7]]
colors[5, 10:17] <- colorlist[[6]]

texts <- matrix("", nrow=nr, ncol=nc)
texts[1, 2:4] <- LETTERS[order(spread(3))]
texts[2, 2:5] <- LETTERS[order(spread(4))]
texts[3, 2:6] <- LETTERS[order(spread(5))]
texts[4, 2:7] <- LETTERS[order(spread(6))]
texts[5, 2:8] <- LETTERS[order(spread(7))]

texts[1, 6:17] <- LETTERS[order(spread(12))]
texts[2, 7:17] <- LETTERS[order(spread(11))]
texts[3, 8:17] <- LETTERS[order(spread(10))]
texts[4, 9:17] <- LETTERS[order(spread(9))]
texts[5, 10:17] <- LETTERS[order(spread(8))]

texts[1:5,1] <- 3:7
texts[1:5,18] <- 12:8

pdf("plots/Permutations.pdf", width=6, height=2.5)

grid.newpage()
pushViewport(viewport(layout=grid.layout(nr, nc)))

ir <- ic <- 1
for (i in 1:(nr*nc)) {
    cellplot(ir, ic, e={
        grid.rect(width=.9, height=.7, gp=gpar(col=NA,fill=colors[ir, ic]))        
        grid.text(texts[ir, ic])        
    })
    ic <- ic + 1
    if (ic > nc) {
        ic <- 1
        ir <- ir + 1
    }
}

dev.off()


## tree depth



depthColors <- matrix(hcl(l=rep(seq(20, 80, by=10), each=9), 
                        c=rep(seq(50, 90, by=5), times=7), 
                        h=rep(300, 36)), ncol=7)
depthColors <- rbind(NA, depthColors)
depthColors <- rbind(NA, depthColors)
depthColors <- cbind(NA, depthColors)
depthColors <- cbind(NA, depthColors)
depthText <- matrix("", ncol=9, nrow=11)
depthText[3:11,2] <- seq(50, 90, by=5)
depthText[2, 3:9] <- seq(20, 80, by=10)


pdf("plots/LC.pdf", width=6, height=3)


grid.newpage()
nr <- 11; nc <- 9
pushViewport(viewport(layout=grid.layout(nr, nc)))

ir <- ic <- 1
for (i in 1:(nr*nc)) {
    cellplot(ir, ic, e={
        grid.rect(width=.9, height=.7, gp=gpar(col=NA,fill=depthColors[ir, ic]))        
        grid.text(depthText[ir, ic])
    })
    ic <- ic + 1
    if (ic > nc) {
        ic <- 1
        ir <- ir + 1
    }
}
cellplot(6, 1, e={
    grid.text("Chroma")
})
cellplot(1, 5, e={
    grid.text("Luminance")
})

dev.off()




depthColors2 <- matrix(hcl(l=rep(c(70, 70, 70, 70, 70, 30, 30, 30, 30, 30), times=7), 
                          c=rep(c(50, 60, 70, 80, 90, 50, 60, 70, 80, 90), times=7), 
                          h=rep(seq(120, 360, length.out=7), each=10)), ncol=7)
depthColors2 <- rbind(NA, depthColors2)
depthColors2 <- rbind(NA, depthColors2)
depthColors2 <- cbind(NA, depthColors2)
depthColors2 <- cbind(NA, depthColors2)
depthText2 <- matrix("", ncol=9, nrow=12)
depthText2[3:12,2] <- c("C=50, L=70", "C=60, L=70", "C=70, L=70", "C=80, L=70", "C=90, L=70",
                        "C=50, L=30", "C=60, L=30", "C=70, L=30", "C=80, L=30", "C=90, L=30")
depthText2[2, 3:9] <- seq(120, 360, length.out=7)

pdf("plots/LC2.pdf", width=6, height=3.5)


grid.newpage()
nr <- 12; nc <- 9
pushViewport(viewport(layout=grid.layout(nr, nc, widths=unit(c(.15, .2, rep(1,nc-2)), c("npc", "npc", rep("null", nc-2))))))
# cellplot(3:9, 3:9, e={
#     grid.lines(x=c(.105,.895), y=c(.1,.9), gp=gpar(col="grey", lwd=5))
# })
ir <- ic <- 1
for (i in 1:(nr*nc)) {
    cellplot(ir, ic, e={
        grid.rect(width=.9, height=.7, gp=gpar(col=NA,fill=depthColors2[ir, ic]))        
        grid.text(depthText2[ir, ic])
        #if (depthSel[ir, ic]) grid.rect(width=.9, height=.7, gp=gpar(col="black", lwd=3, fill=NA))
    })
    ic <- ic + 1
    if (ic > nc) {
        ic <- 1
        ir <- ir + 1
    }
}
cellplot(8, 1, e={
    grid.text("Chroma\nLuminance", just="left", x=.1)
})
cellplot(1, 5, e={
    grid.text("Hue")
})

dev.off()