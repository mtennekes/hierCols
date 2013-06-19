library(devtools); load_all("../treemap/pkg")
library(RColorBrewer)
library(data.table)
library(colorspace)
library(grid)
#source("./R/treefunctions.R")
source("./R/treeplots.R")

### sbi example
source("./R/preprocess_SBI.R")

### create random variables
set.seed(20130618)
sbi$x <- rnorm(nrow(sbi), mean=100, sd=20) #rlnorm(1412, sdlog=3)
# sbi$x2 <- sbi$x * rlnorm(nrow(sbi),sdlog=0.2)
# sbi$x3 <- sbi$x2 * rlnorm(nrow(sbi),sdlog=0.2)
# sbi$x4 <- sbi$x3 * rlnorm(nrow(sbi),sdlog=0.2)

vars <- paste0("y", 1:8)
for (v in vars) sbi[[v]] <- rlnorm(nrow(sbi), sdlog=.5)
sums <- rowSums(sbi[vars])
sbi[vars] <- sbi[vars] / sums



palette.HCL.options <- list(hue_start=30, hue_end=390, hue_spread=TRUE,
                            hue_fraction=0.25, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)

sbi$color <- treepalette(sbi[,3:6], palette.HCL.options=palette.HCL.options, prepare.dat=TRUE)

### for treemaps, only select lowest layer records
sbi_SBI4 <- sbi[!is.na(sbi$SBI4), ]




## tree and treemap for whole sbi range

# pdf("./plots/sbi_all.pdf", width=7, height=7)
# drawtree(sbi[,3:6], color=sbi$color, vertex.size=5)
# dev.off()
# 
# pdf("plots/treemap_all.pdf", width=10, height=6)
#     treemap(sbi_SBI4, index=c("name1", "name2", "name3"), vSize="x", type="index",title="")
# dev.off()



### sbi selection (F sector)
sbiSel <- sbi[sbi$SBI1=="F" & !is.na(sbi$SBI2), ]
sbiSel$color <- treepalette(sbiSel[,4:6], palette.HCL.options=palette.HCL.options, prepare.dat=TRUE)
sbiSel2 <- sbiSel[sbiSel$SBI.level=="SBI4",]
sbiSel2 <- sbiSel2[!is.na(sbiSel2$name4),]

## tree and treemap for sbi F sector

pdf("plots/sbi_F.pdf", width=7, height=7)
    drawtree(sbiSel[,4:6], color=sbiSel$color, vertex.size=8, show.labels=TRUE, rootlabel="F", vertex.label.dist=.3, vertex.label.cex=1)
dev.off()

pdf("plots/treemap_F.pdf", width=10, height=6)
    treemap(sbiSel2, index=c("name2", "name3", "name4"), vSize="x", type="index",title="", position.legend="none")
dev.off()


## stacked bar chart
str(sbiSel2)
dat <- sbiSel2[, c("name4", "x", "x2", "x3", "x4", "color")]

library(reshape2)
dat2 <- melt(dat, id = c("name4", "color"))
levels(dat2$variable) <- letters[1:4]


library(ggplot2)
ggplot(dat2, aes(x=variable, y=value, fill=name4)) + geom_bar(stat="identity") + scale_fill_manual(values=dat2$color) + coord_flip()

######################################################
## method description with SBI2008 F sector
######################################################

dat <- sbiSel[,4:6]
k <- ncol(dat)
res <- treeapply(dat, list(lb=0, ub=360), fun="addRange", frc=0.5, prepare.dat=TRUE)

res$point <- with(res, (lb+ub)/2)
# res$C <- 60 - (k-res$l) * 5 #75
# res$L <- 70 - (res$l-1) * 10 #95
res$color <- treepalette(dat, palette.HCL.options=palette.HCL.options, prepare.dat=TRUE)
    
# hcl(res$point,c=palette.HCL.options$chroma, l=palette.HCL.options$luminance)

dat <- cbind(dat, res)


dat$label <- with(dat, paste(as.character(SBI2), as.character(SBI3), as.character(SBI4), sep="."))


dat$label <- gsub(".NA", "", dat$label, fixed=TRUE)


str(dat)

rem1 <- as.list(as.data.frame(apply(dat[dat$l==1, c("lb", "ub")], MARGIN=1,FUN=function(x)x)))
rem2 <- as.list(as.data.frame(apply(dat[dat$l==2, c("lb", "ub")], MARGIN=1,FUN=function(x)x)))
rem3 <- as.list(as.data.frame(apply(dat[dat$l==3, c("lb", "ub")], MARGIN=1,FUN=function(x)x)))

inverse <- function(x) {
    y <- c(0, unlist(x), 360)
    split(y, rep(1:(length(y)/2), each=2))
}

cuts1 <- inverse(rem1)
cuts2 <- inverse(rem2)
cuts3 <- inverse(rem3)
borders1 <- c(dat$lb[dat$l==1], dat$ub[dat$l==1])
borders2 <- c(dat$lb[dat$l==2], dat$ub[dat$l==2])
labs1 <- dat$point[dat$l==1]
names(labs1) <- dat$label[dat$l==1]
labs2 <- dat$point[dat$l==2]
names(labs2) <- dat$label[dat$l==2]
labs3 <- dat$point[dat$l==3]
names(labs3) <- dat$label[dat$l==3]

gridsize <- 5e2+1

pdf("plots/hcl_method.pdf", width=8, height=8.4)

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=4, ncol=2, heights = unit(rep(1,4), c("null", "lines", "null", "lines")))))

cellplot(1,1, e={
    cat(convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE), "\n",
        convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE), "\n")
    drawHCL(gridsize=gridsize, marks=c(0, 120, 240),
        marks.dashed=c(rep(F, sum(dat$l==1))))
})

cellplot(2,1, e={
    grid.text("(a) Hue range split in three", x=0.05, y=unit(0.5, "lines"), just="left")
})

cellplot(1,2, e={
    drawHCL(gridsize=gridsize, marks=c(0, 120, 240, borders1),
        marks.dashed=c(rep(F, sum(dat$l==1)), rep(T, 2*sum(dat$l==1))),
        cuts=cuts1, labels=labs1)
})
cellplot(2,2, e={
    grid.text("(b) Middle parts assigned to first layer nodes", x=0.05, y=unit(0.5, "lines"), just="left")
})


cellplot(3,1, e={
    drawHCL(gridsize=gridsize, marks=c(60, 170, 190, 285, 300, 315, borders1), 
        marks.dashed=F, 
        cuts=cuts2, labels=labs2, labels.cex=0.7)
})
cellplot(4,1, e={
    grid.text("(c) Recursively divided among second layer nodes", x=0.05, y=unit(0.5, "lines"), just="left")
})


cellplot(3,2, e={
    drawHCL(gridsize=gridsize, 
        marks.dashed=T, 
        cuts=cuts3, labels=labs2, labels.cex=0.6)
})
cellplot(4,2, e={
    grid.text("(d) Recursively divided among third layer nodes", x=0.05, y=unit(0.5, "lines"), just="left")
})

dev.off()