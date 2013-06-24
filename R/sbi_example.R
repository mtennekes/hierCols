#############################################
######### load libraries and scripts
#############################################

library(devtools); load_all("../treemap/pkg")
library(RColorBrewer)
library(data.table)
library(colorspace)
library(grid)
library(reshape2)
library(ggplot2)
source("./R/treeplots.R")

#############################################
######### load example data (SBI)
#############################################

### sbi example
source("./R/preprocess_SBI.R")

#############################################
######### set parameters
#############################################

palette.HCL.options <- list(hue_start=0, hue_end=360, hue_spread=TRUE,
                            hue_fraction=0.75, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)

## note: hue_start=30 and hue_end=390 give slightly better results, but 0 and 360 are easier to use for describing the method

#############################################
######### determine colors for whole SBI range
#############################################

sbi$color <- treepalette(sbi[,3:6], palette.HCL.options=palette.HCL.options, prepare.dat=TRUE)

#############################################
######### add fictive data
#############################################

### create random variables
set.seed(20130618)
sbi$x <- rnorm(nrow(sbi), mean=100, sd=20) #rlnorm(1412, sdlog=3)
sbi$y <- sbi$x * rlnorm(nrow(sbi), mean=0, sdlog=.5)


#############################################
######### preprocess data, make selection (sector F), and prepare for plots
#############################################

### for treemaps, only select lowest layer records
sbi_SBI4 <- sbi[!is.na(sbi$SBI4), ]

### sbi selection (F sector)
sbiSel <- sbi[sbi$SBI1=="F" & !is.na(sbi$SBI2), ]
sbiSel$color <- NULL

res <- treepalette(sbiSel[,4:6], palette.HCL.options=palette.HCL.options, prepare.dat=TRUE, return.parameters=TRUE)

sbiSel <- cbind(sbiSel, res[, 4:9])

sbiSel2 <- sbiSel[sbiSel$SBI.level=="SBI4",]
sbiSel2 <- sbiSel2[!is.na(sbiSel2$name4),]

### data for bar chart
bcdat <- sbiSel2[, c("name4", "y", "color")]
names(bcdat)[1] <- "sector"

bcdat$sector <- factor(bcdat$sector, levels=rev(unique(bcdat$sector)))
levels(bcdat$sector) <- substr(levels(bcdat$sector), 1, 6)

#############################################
######### radial tree graph
#############################################

pdf("./plots/sbi_all.pdf", width=7, height=7)
drawtree(sbi[,3:6], color=sbi$color, vertex.size=5)
dev.off()

pdf("plots/sbi_F.pdf", width=7, height=7)
drawtree(sbiSel[,4:6], color=sbiSel$color, vertex.size=8, show.labels=TRUE, rootlabel="F", vertex.label.dist=.3, vertex.label.cex=1)
dev.off()


#############################################
######### treemap
#############################################

pdf("plots/treemap_all.pdf", width=10, height=6)
    treemap(sbi_SBI4, index=c("name1", "name2", "name3"), vSize="x", type="index",title="")
dev.off()

pdf("plots/treemap_F.pdf", width=10, height=6)
    treemap(sbiSel2, index=c("name2", "name3", "name4"), vSize="x", type="index",title="", position.legend="none")
dev.off()


#############################################
######### stacked bar chart
#############################################


(g <- ggplot(bcdat, aes(x=sector, y=y, fill=sector)) +
    geom_bar(stat="identity") + 
    scale_x_discrete("") +
     scale_y_continuous("") +
     scale_fill_manual(values=rev(bcdat$color)) + coord_flip() + theme_bw() +
     theme(legend.position="none")
 )

ggsave("bar_chart.pdf", path="plots", plot=g, width=6, height=4, scale=1.5)

bcdat <- transform(bcdat, sbi2=factor(paste0("NACE ",substring(sector, 1, 2))))

(g <- ggplot(bcdat, aes(x=sbi2, y=y, fill=sector)) +
     geom_bar(stat="identity", position="stack", colour="#999999") + 
     scale_x_discrete("") +
     scale_y_continuous("") +
     #geom_text(aes(y=sector, x=0.5)) +
     scale_fill_manual(values=rev(bcdat$color)) + theme_bw() +
     theme(legend.position="none")
)

ggsave("stackedbar_chart.pdf", path="plots", plot=g, width=6, height=4, scale=1.5)

# stacked line chart
set.seed(20120624)
bcdat <- transform(bcdat, t=2012)

bcdats <- list(bcdat)


for (i in 2:10) {
    bcdat_temp <- bcdat
    old <- bcdats[[i-1]]$y
    
    bcdat_temp$y <- old * rlnorm(length(old), sdlog=.3) * rlnorm(length(old), meanlog=-.07, sdlog=.2)
    bcdat_temp$t <- 2013-i
    bcdats[[i]] <- bcdat_temp
}

bcdat2 <- do.call("rbind", bcdats)

bctext <- bcdat2[bcdat2$t==2012,]
bctext$t <- 2012.25
cums <- c(0, cumsum(bctext$y))
bctext$y <- (cums[-1] + head(cums, -1))/2

(g <- ggplot(bcdat2, aes(x = t, y = y, fill = sector)) + geom_area(position = 'stack', color="#999999") +
     scale_x_continuous("year", breaks=2003:2012) +
     scale_y_continuous("value") +
     geom_text(data = bctext, aes(label = sector, x=t, ymax=y, y=y), hjust = .3, vjust = .5, size=2.8) +
scale_fill_manual(values=rev(bcdat$color)) + theme_bw() +
    theme(legend.position="none")
)

ggsave("stackedline_chart.pdf", path="plots", plot=g, width=6, height=4, scale=1.5)


#############################################
######### show permutations
#############################################

for (i in 2:20) cat(i, ":", spread(i), "\n")


#############################################
######### plot for method description for dividing hue range
#############################################

dat <- sbiSel

dat$label <- with(dat, paste(as.character(SBI2), as.character(SBI3), as.character(SBI4), sep="."))
dat$label <- gsub(".NA", "", dat$label, fixed=TRUE)

dat$l <- as.numeric(dat$SBI.level) - 1

rem1 <- as.list(as.data.frame(apply(dat[dat$l==1, c("hue_lb", "hue_ub")], MARGIN=1,FUN=function(x)x)))
rem2 <- as.list(as.data.frame(apply(dat[dat$l==2, c("hue_lb", "hue_ub")], MARGIN=1,FUN=function(x)x)))
rem3 <- as.list(as.data.frame(apply(dat[dat$l==3, c("hue_lb", "hue_ub")], MARGIN=1,FUN=function(x)x)))

inverse <- function(x) {
    y <- c(0, sort(unlist(x)), 360)
    split(y, rep(1:(length(y)/2), each=2))
}

cuts1 <- inverse(rem1)
cuts2 <- inverse(rem2)
cuts3 <- inverse(rem3)
borders1 <- c(dat$hue_lb[dat$l==1], dat$hue_ub[dat$l==1])
borders2 <- c(dat$hue_lb[dat$l==2], dat$hue_ub[dat$l==2])
labs1 <- dat$H[dat$l==1]
names(labs1) <- dat$label[dat$l==1]
labs2 <- dat$H[dat$l==2]
names(labs2) <- dat$label[dat$l==2]
labs3 <- dat$H[dat$l==3]
names(labs3) <- dat$label[dat$l==3]
labs3 <- labs3[-c(14:16, 18:20, 12)]  ## to prevent overplotting

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
    drawHCL(gridsize=gridsize, marks=c(60, 157.5, 202.5, 285, 315, borders1), 
        marks.dashed=F, 
        cuts=cuts2, labels=labs2, labels.cex=0.7)
})
cellplot(4,1, e={
    grid.text("(c) Recursively divided among second layer nodes", x=0.05, y=unit(0.5, "lines"), just="left")
})

cellplot(3,2, e={
    drawHCL(gridsize=gridsize, 
        marks.dashed=T, 
        cuts=cuts3, labels=labs3, labels.cex=0.6)
})
cellplot(4,2, e={
    grid.text("(d) Recursively divided among third layer nodes", x=0.05, y=unit(0.5, "lines"), just="left")
})

dev.off()