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


# function to create font embedded pdf's
# make sure the fontfamily in e is set to a available family, such as URWHelvetica
PDF <- function(filename, ..., e) {
    pdf(filename, ...)
    e
    dev.off()
    #embedFonts(filename, outfile = filename)
}
#embedFonts is turn off to embed fonts in the final pdf (article)
windowsFonts(URWHelvetica=windowsFont("URWHelvetica"))


# show permutations
for (i in 2:20) cat(i, ":", spread(i), "\n")


#####################################################################################
#######
####### create data for method description
#######
#####################################################################################

set.seed(20140217)
hd <- random.hierarchical.data(method="random.arcs", nodes.per.layer=c(3, 12, 25))
# plot for method description for dividing hue range
palette.HCL.options <- list(hue_start=0, hue_end=360, 
                            hue_fraction=0.75, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)
dat <- treepalette(hd[,1:3], palette.HCL.options=palette.HCL.options, prepare.dat=TRUE, return.parameters=TRUE)
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

#####################################################################################
#######
####### method plots
#######
#####################################################################################


PDF("plots/hcl_method2.pdf", width=8, height=8.4, e={
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(nrow=4, ncol=2, heights = unit(rep(1,4), c("null", "lines", "null", "lines")))))
    
    cellplot(1,1, e={
        cat(convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE), "\n",
            convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE), "\n")
        drawHCL(gridsize=gridsize, marks=c(0, 120, 240),
                marks.dashed=c(rep(F, sum(dat$l==1))))
    })
    
    cellplot(2,1, e={
        grid.text("(a) Hue range equally split in three", x=0.05, y=unit(0.5, "lines"), just="left", gp=gpar(fontfamily="URWHelvetica"))
    })
    
    cellplot(1,2, e={
        drawHCL(gridsize=gridsize, marks=c(0, 120, 240, borders1),
                marks.dashed=c(rep(F, sum(dat$l==1)), rep(T, 2*sum(dat$l==1))),
                cuts=cuts1, labels=labs1)
    })
    cellplot(2,2, e={
        grid.text("(b) Middle fractions assigned to first layer nodes", x=0.05, y=unit(0.5, "lines"), just="left", gp=gpar(fontfamily="URWHelvetica"))
    })
    
    
    cellplot(3,1, e={
        drawHCL(gridsize=gridsize, marks=c(37.5, 60, 82.5, 135+(18*(0:4)), 285, 315, borders1), 
                marks.dashed=F, 
                cuts=cuts2, labels=labs2, labels.cex=0.7)
    })
    cellplot(4,1, e={
        grid.text("(c) Recursively applied to second layer nodes", x=0.05, y=unit(0.5, "lines"), just="left", gp=gpar(fontfamily="URWHelvetica"))
    })
    
    cellplot(3,2, e={
        drawHCL(gridsize=gridsize, 
                marks.dashed=T, 
                cuts=cuts3, labels=labs3, labels.cex=0.6)
    })
    cellplot(4,2, e={
        grid.text("(d) Recursively applied to third layer nodes", x=0.05, y=unit(0.5, "lines"), just="left", gp=gpar(fontfamily="URWHelvetica"))
    })
})

## replicate first subplot for color blindness 
png("plots/hcl_normal.png", width=840, height=840, res=600, pointsize=4)
cat(convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE), "\n",
    convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE), "\n")
drawHCL(gridsize=gridsize, marks=c(0, 120, 240),
        marks.dashed=c(rep(F, sum(dat$l==1))), lwd=.5)
dev.off()



## 
PDF("plots/HCPgraph.pdf", width=6, height=6, useDingbats=FALSE, e={
    set.seed(20140203)
    #treegraph(dat, index=c("index1", "index2", "index3"), show.labels=TRUE, vertex.layout=igraph::layout.auto,vertex.size=4, palette.HCL.options=palette.HCL.options)
    treegraph(dat, index=c("index1", "index2", "index3"), show.labels=TRUE, vertex.size=10, vertex.label.dist=.5, palette.HCL.options=palette.HCL.options, directed=FALSE, vertex.label.family="URWHelvetica")
})

# permutaions and reversals turned off
PDF("plots/HCPgraph2.pdf", width=6, height=3.5, useDingbats=FALSE, e={
    par(mfrow=c(1,2))
    set.seed(20140203)
    treegraph(dat, index=c("index1", "index2", "index3"), show.labels=TRUE, vertex.size=12, vertex.label.cex=.6, vertex.label.dist=.6, palette.HCL.options=c(palette.HCL.options, list(hue_perm=FALSE, hue_rev=FALSE)), directed=FALSE, vertex.label.family="URWHelvetica")
    
    treegraph(dat, index=c("index1", "index2", "index3"), show.labels=TRUE, vertex.size=12, vertex.label.cex=.6, vertex.label.dist=.6, palette.HCL.options=c(palette.HCL.options, list(hue_perm=FALSE)), directed=FALSE, vertex.label.family="URWHelvetica")
})


# additive and subtractive methods
PDF("plots/HCPgraph3.pdf", width=6, height=6, useDingbats=FALSE, e={
    set.seed(20140203)
    #treegraph(dat, index=c("index1", "index2", "index3"), show.labels=TRUE, vertex.layout=igraph::layout.auto,vertex.size=4, palette.HCL.options=palette.HCL.options)
    treegraph(dat, index=c("index1", "index2", "index3"), show.labels=TRUE, vertex.size=10, vertex.label.dist=.5, palette.HCL.options=c(palette.HCL.options, list(luminance=40, luminance_slope=10, chroma=75, chroma_slope=-5)), directed=FALSE, vertex.label.family="URWHelvetica")
})

#####################################################################################
#######
####### Staline applications
#######
#####################################################################################


## use Statline data for real world application
data(business)
source("./R/statline_business_data2.R")

str(business)


palette.HCL.optionsExp <- list(hue_start=0, hue_end=360, 
                               hue_fraction=.75, chroma=60, luminance=70, 
                               chroma_slope=5, luminance_slope=-10)

palette.HCL.optionsImp <- list(hue_start=0, hue_end=360, 
                               hue_fraction=.5, chroma=60, luminance=70, 
                               chroma_slope=5, luminance_slope=-10)


#itreemap(d, index=c("N1", "N2", "N3", "N4"))


PDF("plots/TMbusiness.pdf", width=9, height=7, e={
    treemap(subset(d, subset=N1 == "G Wholesale and retail trade"), index=c("N2", "N3", "N4"), vSize="emp2010", overlap.labels=.25, bg.labels=255, title="", palette.HCL.options=palette.HCL.optionsImp, ymod.labels=c(.1,0,0), fontfamily.labels="URWHelvetica") 
})


dG <- subset(d, subset=N1 == "G Wholesale and retail trade" & depth>1)
set.seed(20140307)
PDF("plots/Gbusiness_FR.pdf", width=9, height=7, e={
    treegraph(dG, index=c("n2", "n3", "n4", "n5"),  show.labels=TRUE, vertex.layout=igraph::layout.fruchterman.reingold, vertex.size=8, vertex.label.dist=.4, vertex.label.cex=1.1, palette.HCL.options=palette.HCL.optionsExp, vertex.label.family="URWHelvetica")
})

set.seed(20140307)
PDF("plots/Gbusiness_KK.pdf", width=9, height=7, e={
    treegraph(dG, index=c("n2", "n3", "n4", "n5"),  show.labels=TRUE, vertex.layout=igraph::layout.kamada.kawai, vertex.size=8, vertex.label.dist=.4, vertex.label.cex=1.1, palette.HCL.options=palette.HCL.optionsExp, vertex.label.family="URWHelvetica")
})



## statline population per province
library(ggplot2)
library(reshape2)
library(scales)

source("./R/statline_population_data.R")



# treegraph(nlpop, index=c("ld", "prov"), vertex.size=10, palette.HCL.options=palette.HCL.optionsExp)


nlpop2 <- melt(nlpop, na.rm=FALSE, value.name="x", id.vars=c("ld", "prov"), variable.name="year")
nlpop2$year <- as.integer(nlpop2$year) + 1959

nlpop2$x[is.na(nlpop2$x)] <- 0


palette.HCL.optionsBar <- list(hue_start=240, hue_end=500, 
                               hue_fraction=.75, chroma=60, luminance=70, 
                               chroma_slope=5, luminance_slope=-10)
nlpal <- treepalette(nlpop, index=c("ld", "prov"), palette.HCL.options=palette.HCL.optionsBar)
nlpal2 <- nlpal$HCL.color[match(levels(nlpop2$prov), nlpal$prov)]

# ggplot(nlpop2, aes(x=year, y=x, fill=prov)) + geom_area() + scale_fill_manual(values=nlpal2)

nlpop3 <- nlpop2[nlpop2$year==2012,]
nlpop3$pos <- addSpace(nlpop[,1:2])
nlpop3$pos <- max(nlpop3$pos) - nlpop3$pos

gbar <- ggplot(nlpop3, aes(x=pos, y=x, fill=prov)) + 
    geom_bar(stat="identity") +
    scale_x_continuous("", breaks=nlpop3$pos, labels=nlpop3$prov) +
    scale_y_continuous("", labels=comma) +
    scale_fill_manual(values=nlpal2) + coord_flip() + theme_bw() + theme(legend.position="none", text=element_text(family="URWHelvetica"))

ggsave("./plots/pop_bar.pdf", width=5, height=5)
embedFonts("./plots/pop_bar.pdf", outfile="./plots/pop_bar.pdf")

#####################################################################################
#######
####### hue fraction
#######
#####################################################################################


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

dat <- dat4childs

PDF("plots/Treemaps_hue.pdf", width=10, height=10, e={
    grid.newpage()
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    pushViewport(viewport(layout=grid.layout(nr,nc)))
    
    ir <- ic <- 1
    for (i in 1:length(fs)) {
        treemap(dat, index=names(dat)[1:(ncol(dat)-1)], vSize="x", palette.HCL.options=list(hue_fraction=fs[i]), vp=vplayout(ir,ic), title=paste("Fraction =", fs_format[i]), overlap=0.1, bg.labels=255, fontfamily.labels="URWHelvetica", fontfamily.title="URWHelvetica")
        ic <- ic + 1
        if (ic > nc) {
            ic <- 1
            ir <- ir + 1
        }
    }
})

dat2 <- dat3childs
PDF("plots/Graph_hue.pdf", width=8, height=8, useDingbats=FALSE, e={
    par(mfrow=c(2,2))
    for (i in 1:length(fs)) {
        set.seed(20140212)
        treegraph(dat2, index=c("index1", "index2", "index3"), directed=FALSE, show.labels=TRUE, vertex.layout=igraph::layout.auto, vertex.size=10, vertex.label.dist=0.6,palette.HCL.options=list(hue_fraction=fs[i]), mai=c(.2,.2,.2,.2), vertex.label.family="URWHelvetica")
        title(paste("\n                               Fraction =", fs_format[i]), font.main=1, family="URWHelvetica")
    }
})

#####################################################################################
#######
####### teaser
#######
#####################################################################################

## teaser
set.seed(20140216)
#dat2 <- random.hierarchical.data(method="random", number.children=3, value.generator=rnorm, value.generator.args=list(mean=3), labels.prefix=c("Main", "Sub", ""))
dat2 <- random.hierarchical.data(method="random.arcs", nodes.per.layer=c(5,20, 40), value.generator=rnorm, value.generator.args=list(mean=3), labels.prefix=c("Main", "Sub", ""))

PDF("plots/Treemap_teaser.pdf", width=8, height=8, e={
    treemap(dat2, index=c("index1", "index2", "index3"), vSize="x", palette.HCL.options=list(hue_fraction=.5), title="", overlap=.1, fontsize.labels=14, bg.labels=255, fontfamily.labels="URWHelvetica")
})


set.seed(20140303)
dat3 <- random.hierarchical.data(method="random.arcs", nodes.per.layer=c(3, 9, 27), value.generator=rnorm, value.generator.args=list(mean=3))

PDF("plots/Graph_teaser.pdf", width=8, height=8, useDingbats=FALSE, e={
    set.seed(20140301)
    p <- treegraph(dat3, index=c("index1", "index2", "index3"), directed=FALSE, show.labels=TRUE, vertex.size=10, vertex.label.dist=.5, vertex.label.cex=1.4, vertex.layout=igraph::layout.fruchterman.reingold, palette.HCL.options=list(hue_fraction=.95), vertex.label.family="URWHelvetica")
})

jpeg("plots/Graph_teaser.jpg", width=250, height=183, type = "cairo")
set.seed(20140301)
p <- treegraph(dat3, index=c("index1", "index2", "index3"), directed=FALSE, show.labels=TRUE, vertex.size=10, vertex.label.dist=.6, vertex.label.cex=.7, edge.width=2, vertex.layout=igraph::layout.fruchterman.reingold, palette.HCL.options=list(hue_fraction=.95), vertex.label.family="URWHelvetica", mai=c(0,0,.1,0))
dev.off()

#####################################################################################
#######
####### permutation
#######
#####################################################################################


## permutation order
n <- 3:12
nr <- 5
nc <- 18

palette.HCL.options2 <- list(hue_start=120, hue_end=240, 
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

PDF("plots/Permutations.pdf", width=6, height=2.5, e={
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(nr, nc)))
    
    ir <- ic <- 1
    for (i in 1:(nr*nc)) {
        cellplot(ir, ic, e={
            grid.rect(width=.9, height=.7, gp=gpar(col=NA,fill=colors[ir, ic]))        
            grid.text(texts[ir, ic], gp=gpar(fontfamily="URWHelvetica"))        
        })
        ic <- ic + 1
        if (ic > nc) {
            ic <- 1
            ir <- ir + 1
        }
    }
})

#####################################################################################
#######
####### tree depth
#######
#####################################################################################



depthColors <- matrix(hcl(l=rep(seq(20, 80, by=10), each=9), 
                        c=rep(seq(50, 90, by=5), times=7), 
                        h=rep(300, 36)), ncol=7)
depthColors <- rbind(NA, NA, depthColors)
depthColors <- cbind(NA, NA, depthColors)
depthText <- matrix("", ncol=9, nrow=11)
depthText[3:11,2] <- seq(50, 90, by=5)
depthText[2, 3:9] <- seq(20, 80, by=10)


PDF("plots/LC.pdf", width=6, height=3, e={
    grid.newpage()
    nr <- 11; nc <- 9
    pushViewport(viewport(layout=grid.layout(nr, nc)))
    
    ir <- ic <- 1
    for (i in 1:(nr*nc)) {
        cellplot(ir, ic, e={
            grid.rect(width=.9, height=.7, gp=gpar(col=NA,fill=depthColors[ir, ic]))        
            grid.text(depthText[ir, ic], gp=gpar(fontfamily="URWHelvetica"))
        })
        ic <- ic + 1
        if (ic > nc) {
            ic <- 1
            ir <- ir + 1
        }
    }
    cellplot(6, 1, e={
        grid.text("Chroma", gp=gpar(fontfamily="URWHelvetica"))
    })
    cellplot(1, 5, e={
        grid.text("Luminance", gp=gpar(fontfamily="URWHelvetica"))
    })
})




depthColors2 <- matrix(hcl(l=rep(c(70, 70, 70, 70, 70, 30, 30, 30, 30, 30), times=7), 
                          c=rep(c(50, 60, 70, 80, 90, 50, 60, 70, 80, 90), times=7), 
                          h=rep(seq(120, 360, length.out=7), each=10)), ncol=7)
depthColors2 <- rbind(NA, NA, depthColors2)
depthColors2 <- cbind(NA, NA, depthColors2)
depthText2 <- matrix("", ncol=9, nrow=12)
depthText2[3:12,2] <- c("C=50, L=70", "C=60, L=70", "C=70, L=70", "C=80, L=70", "C=90, L=70",
                        "C=50, L=30", "C=60, L=30", "C=70, L=30", "C=80, L=30", "C=90, L=30")
depthText2[2, 3:9] <- seq(120, 360, length.out=7)

PDF("plots/LC2.pdf", width=6, height=3.5, e={

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
            grid.text(depthText2[ir, ic], gp=gpar(fontfamily="URWHelvetica"))
            #if (depthSel[ir, ic]) grid.rect(width=.9, height=.7, gp=gpar(col="black", lwd=3, fill=NA))
        })
        ic <- ic + 1
        if (ic > nc) {
            ic <- 1
            ir <- ir + 1
        }
    }
    
    cellplot(8, 1, e={
        grid.text("Chroma,\nLuminance", just="left", x=.1, gp=gpar(fontfamily="URWHelvetica"))
    })
    	
    cellplot(1, 5, e={
        grid.text("Hue", gp=gpar(fontfamily="URWHelvetica"))
    })

})




depthColors3 <- matrix(hcl(l=rep(c(70, 60, 50, 40, 30), times=7), 
                           c=rep(c(60, 65, 70, 75, 80), times=7), 
                           h=rep(seq(120, 360, length.out=7), each=5)), ncol=7)
depthColors3 <- rbind(NA, NA, depthColors3)
depthColors3 <- cbind(NA, NA, depthColors3)
depthText3 <- matrix("", ncol=9, nrow=7)
depthText3[3:7,2] <- c("i=1, C=60, L=70", "i=2, C=65, L=60", "i=3, C=70, L=50", "i=4, C=75, L=40", "i=5, C=80, L=30")
depthText3[2, 3:9] <- seq(120, 360, length.out=7)


PDF("plots/LC3.pdf", width=6, height=2, e={

    grid.newpage()
    nr <- 7; nc <- 9
    
    pushViewport(viewport(layout=grid.layout(nr, nc, widths=unit(c(.15, .25, rep(1,nc-2)), c("npc", "npc", rep("null", nc-2))))))
    
    # cellplot(3:9, 3:9, e={
    #     grid.lines(x=c(.105,.895), y=c(.1,.9), gp=gpar(col="grey", lwd=5))
    # })
    ir <- ic <- 1
    for (i in 1:(nr*nc)) {
        cellplot(ir, ic, e={
            grid.rect(width=.9, height=.7, gp=gpar(col=NA,fill=depthColors3[ir, ic]))        
            grid.text(depthText3[ir, ic], gp=gpar(fontfamily="URWHelvetica"))
            #if (depthSel[ir, ic]) grid.rect(width=.9, height=.7, gp=gpar(col="black", lwd=3, fill=NA))
        })
        ic <- ic + 1
        if (ic > nc) {
            ic <- 1
            ir <- ir + 1
        }
    }
    
    cellplot(5, 1, e={
        grid.text("Layer,\nChroma,\nLuminance", just="left", x=.1, gp=gpar(fontfamily="URWHelvetica"))
    })
    
    cellplot(1, 5, e={
        grid.text("Hue", gp=gpar(fontfamily="URWHelvetica"))
    })

})


#####################################################################################
#######
####### user survey
#######
#####################################################################################

library(igraph)
library(grid)

source("./R/survey_data.R")
dats_tm <- generateRandomHierData(2, seeds=c(20140114, 20140115), prefices=c("Main labels", "Sub-label"))
dats_gr <- generateRandomHierData(2, seeds=c(20140116, 20140117), levs=c(3,6,17), addText=FALSE)
dats_bar <- generateRandomHierData(2, seeds=c(20140127, 20140128), levs=c(3, 9, 22))

hue_start <- c(90, 0)
hue_end <- c(450, 360)

branches_tm1 <- list(c("HZ", "AF", "SD", "KL", "SX"),
                     c("JM", "HD", "EP", "EN", "PQ"))

branches_tm2 <- c("Main code AJ", "Main code SA")
branches_gr1 <- list(c("J", "K", "S", "C"),
                     c("K", "J", "A", "L"))
branches_gr2 <- c("G", "B")
branches_bar1 <- list(c("OS","JF","RG","PG","LH"),c("MX","DR","CG","IV","QT"))

seeds_gr <- c(20140101, 20140102)

group <- 1
PDF("plots/Graph_survey_FC.pdf", width=4, height=4, useDingbats=FALSE, e={
    par(mai=c(0,0,0,0))
    plotGraph(dats_gr[[group]], method="firstcat", seed=seeds_gr[[group]], vertex.label.family="URWHelvetica")
})
PDF("plots/Graph_survey_TC.pdf", width=4, height=4, useDingbats=FALSE, e={
    par(mai=c(0,0,0,0))
    plotGraph(dats_gr[[group]], method="HCP", seed=seeds_gr[[group]], hue_fraction=0.75, vertex.label.family="URWHelvetica")
})

group <- 2
PDF("plots/Treemap_survey_TC.pdf", width=6, height=6, useDingbats=FALSE, e={

    tm <- treemap(dats_tm[[group]], index=c("h1", "h2", "h3"), vSize="value", title="", bg.labels=255, overlap.labels=0.1, palette.HCL.options=list(hue_start=hue_start[[group]], hue_end=hue_end[[group]], hue_fraction=.6), fontfamily.labels="URWHelvetica", fontfamily.title="URWHelvetica")
    
    addSymbols(tm, branches_tm1[[group]], symbols=c("+", "*", "*", "*", "*"))
})

PDF("plots/Treemap_survey_FC.pdf", width=6, height=6, useDingbats=FALSE, e={
    tm <- treemap(dats_tm[[group]], index=c("h1", "h2", "h3"), vSize="value", title="", vColor="h1", type="categorical", position.legend="none", palette="Set1", overlap.labels=0.1, fontfamily.labels="URWHelvetica", fontfamily.title="URWHelvetica")
    addSymbols(tm, branches_tm1[[group]], symbols=c("+", "*", "*", "*", "*"))
})

group <- 1
g1 <- plotBar(dats_bar[[group]], method="firstcat") + theme(text=element_text(family="URWHelvetica"))
g2 <- plotBar(dats_bar[[group]], method="HCP", hue_fraction=0.6) + theme(text=element_text(family="URWHelvetica"))
ggsave("plots/Bar_survey_FC.pdf", g1, width=4, height=4, scale=1.5)
ggsave("plots/Bar_survey_TC.pdf", g2, width=4, height=4, scale=1.5)


embedFonts("./paperVisweek2014/hcp.pdf", outfile="./paperVisweek2014/hcp.pdf")
