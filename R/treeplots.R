drawtree <- function(dat, color, show.labels=FALSE, rootlabel="", vertex.size=3, vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.family="sans", vertex.label.color="black") {
    require(igraph)
    k <- ncol(dat)
    dat$color <- color
    dat <- unique(dat)
    dat <- cbind(dat, treeid(dat[,1:k]), stringsAsFactors=FALSE)
    vdat <- dat[,c("current", "color")]
    vdat <- unique(vdat)
    rootname <- dat$parent[which(substr(dat$parent, 1, 2)=="NA")][1]
    vdat <- rbind(list(current=rootname, color="#FFFFFF"), vdat)
    
    g <- graph.data.frame(dat[,c("current", "parent")], vertices=vdat, directed=FALSE)
    #color <- color[match(get.vertex.attribute(g, "name"), dat$current)]
    #V(g)$color <- color[match(get.vertex.attribute(g, "name"), dat$current)]
    
    
    if (show.labels) {
        vdat_names <- gsub(".NA", "", vdat$current, fixed=TRUE)
        vdat_names[1] <- rootlabel
    
        ## equal string lengths
        lengths <- nchar(vdat_names)
        heads <- floor((max(lengths)-lengths)/2)
        tails <- max(lengths) - lengths - heads
        hs <- sapply(heads, function(x)paste(rep(" ", x), collapse=""))
        ts <- sapply(tails, function(x)paste(rep(" ", x), collapse=""))
        
        vdat_names <- mapply(paste0, hs, vdat_names, ts)
    } else {
        vdat_names <- NA
    }
    
    
    par(mai=c(0,0,0,0))
    plot(g, vertex.size=vertex.size, vertex.frame.color=NA, vertex.label=vdat_names, layout=
             layout.reingold.tilford(g, circular=T, root=1), vertex.label.dist=vertex.label.dist, vertex.label.cex=vertex.label.cex, vertex.label.family=vertex.label.family, vertex.label.color=vertex.label.color, vertex.label.degree=-pi/2.5)
    invisible(g)
}


drawHCL <- function(gridsize=1e3, C=60, L=70, r=c(0.6, 0.8), cuts=NULL, marks=NULL, marks.dashed=FALSE, labels=NULL, marks.r=c(0.4, 0.8), labels.r=0.5, marks.labels.r=0.88, marks.labels.cex=0.8, labels.cex=0.8) {
    #grid.newpage()
    n <- gridsize
    
    xm <- matrix(rep(seq(-1,1, length.out=n), each=n), ncol=n)
    ym <- matrix(rep(seq(-1,1, length.out=n), times=n), ncol=n)
    
    rm <- atan2(xm, ym)
    rm[rm<0] <- rm[rm<0]+2*pi
    
    cm <- matrix(hcl(rm/pi*180, C, L), ncol=n)
    dm <- sqrt((xm^2 + ym^2))
    cm[dm<r[1] | dm >r[2]] <- NA
    
    if (!missing(cuts)) {
        for (ct in cuts) {
            ct2 <- ct/180*pi
            cm[rm>=ct2[1] & rm<=ct2[2]] <- NA
        }
    }
    
    s <- seq(0,1, length.out=2*n+1)[seq(2, 2*n, by=2)]
    
    xcm <- matrix(rep(s, each=n), ncol=n)
    ycm <- matrix(rep(s, times=n), ncol=n)
    
    wh <- (1/n)
    grid.rect(x=xcm, y=ycm, width=wh, height=wh, gp=gpar(col=NA,fill=cm))
    
    if (!missing(marks)) {
        lbls <- marks
        lr <- marks / 180 * pi
        
        x1 <- 0.5 + sin(lr)*marks.labels.r/2
        y1 <- 0.5 + cos(lr)*marks.labels.r/2
        grid.text(lbls, x1, y1, gp=gpar(cex=marks.labels.cex))
        
        
        mr <- marks / 180 * pi
        
        x1 <- 0.5 + sin(mr)*marks.r[2]/2
        y1 <- 0.5 + cos(mr)*marks.r[2]/2
        
        x2 <- 0.5 + sin(mr)*marks.r[1]/2
        y2 <- 0.5 + cos(mr)*marks.r[1]/2
        
        x <- rep(x1, each=2)
        x[seq(2, length(x), by=2)] <- x2
        
        y <- rep(y1, each=2)
        y[seq(2, length(y), by=2)] <- y2
        
        id <- rep(1:length(mr), each=2)
        lty <- ifelse(marks.dashed, "dashed", "solid")
        grid.polyline(x=x, y=y, id=id, gp=gpar(lty=lty))
        
    }
    if (!missing(labels)) {
        lbls <- names(labels)
        lr <- labels / 180 * pi
        
        x1 <- 0.5 + sin(lr)*labels.r/2
        y1 <- 0.5 + cos(lr)*labels.r/2
        grid.text(lbls, x1, y1, gp=gpar(cex=labels.cex))
    }
    
}

cellplot <- function(x,y, vp=NULL, e){
    name <- paste("(", deparse(substitute(x)),",",deparse(substitute(y)), ")", sep="")
    pushViewport(viewport( name=name, layout.pos.row=x, layout.pos.col=y))
    n <- 1
    if (!is.null(vp)){ 
        pushViewport(vp)
        n <- n + 1
    }
    e
    popViewport(n=n)
}