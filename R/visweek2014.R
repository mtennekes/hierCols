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
hd <- business[as.integer(business$NACE1)==6, c("NACE2", "NACE3", "NACE4", "turnover")]


hd <- fancyLevels(hd, index=names(hd)[1:3])





# plot for method description for dividing hue range
palette.HCL.options <- list(hue_start=0, hue_end=360, hue_spread=TRUE,
                            hue_fraction=0.75, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)

dat <- treepalette(hd[,1:3], palette.HCL.options=palette.HCL.options, prepare.dat=TRUE, return.parameters=TRUE)


#dat$label <- with(dat, paste(as.character(L1), as.character(L2), as.character(L3), sep="."))
#dat$label <- gsub(".NA", "", dat$label, fixed=TRUE)

dat$l <- ifelse(!is.na(dat$L3), 3, ifelse(!is.na(dat$L2), 2, 1))

dat$label <- ifelse(dat$l==1, as.character(dat$L1), ifelse(dat$l==2, as.character(dat$L2), as.character(dat$L3)))
    

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
labs3 <- labs3[-c(14:16, 18:20, 12)]  ## to prevent overplotting

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
    drawHCL(gridsize=gridsize, marks=c(60, 157.5, 202.5, 285, 315, borders1), 
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
treegraph(dat, index=c("L1", "L2", "L3"), show.labels=TRUE, vertex.layout=igraph::layout.auto,vertex.size=4, palette.HCL.options=palette.HCL.options)
dev.off()






## use Statline data for real world application
source("./R/statline_business_data.R")

data(business)
str(business)

library(treemap)
treemap(d, index=c("n1", "n2", "n3", "n4"), vSize="emp2010")
treemap(d[as.integer(d$n1)==7,], index=c("n2", "n3", "n4"), vSize="emp2010", overlap.labels=.1)
treemap(d[as.integer(d$n1)==7,], index=c("n2", "n3", "n4"), vSize="turn2011", overlap.labels=.1)


###### experiment with hue fraction
dat <- generateRandomHierData(1, levs=c(3,9,27), addText=FALSE)[[1]]
dat <- generateRandomHierData(1, levs=c(4,16,64), addText=FALSE)[[1]]
dat <- generateRandomHierData(1, levs=c(5,25,125), addText=FALSE)[[1]]
dat <- generateRandomHierData(1, levs=6^(1:3), addText=FALSE)[[1]]


dat$value[dat$value<0] <- 0

dat <- fancyLevels(dat, index=names(dat)[1:3])

palette.HCL.options <- list(hue_start=0, hue_end=360, hue_spread=TRUE,
                            hue_fraction=.75, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)


treegraph(dat, index=c("h1", "h2", "h3"), show.labels=FALSE, vertex.layout=igraph::layout.auto,vertex.size=4, palette.HCL.options=palette.HCL.options)

fs <- c(0.25, .5, .75, 1)#seq(0, 1, length.out=11)
nr <- 1
nc <- 4

grid.newpage()
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
pushViewport(viewport(layout=grid.layout(nr,nc)))

ir <- ic <- 1

for (i in 1:length(fs)) {
    treemap(dat, index=names(dat)[1:(ncol(dat)-1)], vSize="value", palette.HCL.options=list(hue_fraction=fs[i]), vp=vplayout(ir,ic), title=paste("f", fs[i]))
    ic <- ic + 1
    if (ic > nc) {
        ic <- 1
        ir <- ir + 1
    }
}


