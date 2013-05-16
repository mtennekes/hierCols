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

treedepth <- function(data) {
	k <- ncol(data)
	apply(data, MARGIN=1, FUN=function(x)k-sum(is.na(x)))
}

treeapply <- function(dat, values, depth=NULL, fun, ...) {
	if (is.data.table(dat)) {
		dat[, names(dat):=lapply(.SD,as.factor)]
	} else {
		dat <- lapply(dat, as.factor)
		require(data.table)
		dat <- as.data.table(dat)
	}
	
	dt <- dat[!duplicated(dat), ]
	
	k <- ncol(dt)
	
	# hierarchical depth
	if (missing(depth)) {
		dt[, l:=treedepth(dt)]
	} else {
		dt[, l:=depth]
	}
	
	args <- list(...)
	vars <- names(values)
	
	#setkeyv(dt, index)
	
	#browser()
	if (length(values[[1]])==1) {
		## apply function on first layer
		dt[, eval(vars):=values]
		dt[dt$l==1, eval(vars):=do.call(fun, 
										  args=c(list(as.list(dt[dt$l==1,vars,with=FALSE]), depth=1), args))]
	} else {
		## assign values for levels of  first layer
		ind1 <- dt[[1]]
		lvls <- levels(ind1)
		dt[, eval(vars):=1]
		for (l in 1:length(lvls)) {
			lv <- lvls[l]
			vls <- lapply(values, function(v)v[l])
			vls <- as.data.frame(vls)
			dt[ind1==lv, eval(vars):=vls]
		}
	}
	
	#browser()
	if (k>1) for (d in 2:k) {
		id <- names(dt)[1:(d-1)]
		# assign values of layer above to parent values of current layer
		#dt[dt$l==d, eval(parents):=children, by=id]
		fn <- function(x) {
			x <- copy(x)
			x[x$l==d, eval(vars):=as.list(x[x$l==(d-1), vars, with=FALSE])]
			x[x$l==d, eval(vars):=do.call(fun, args=c(list(as.list(x[x$l==d,vars,with=FALSE]), depth=d), args))]
			x
		}
		res <- as.list(dt[, fn(.SD), by=id][, vars, with=FALSE])
		dt[, eval(vars):=res]
	}
	
	id1 <- treeid(dat)$current
	id2 <- treeid(dt[,1:k, with=FALSE])$current
	
	as.list(dt[match(id1,id2),(k+1):ncol(dt), with=FALSE])
}

########## method 1: split hcl circle
addRange <- function(x, depth, frc = 0.3) {
	LB <- x[[1]][1]
	UB <- x[[2]][1]
	
	nr <- length(x[[1]])
	#browser()
	sq <- seq(LB, UB, length.out=nr+1)
	spacer <- (sq[2] - sq[1]) * frc *.5

 	### determine order of brothers/sisters in tree
    ### to prevent that neighbouring brothers/sisters have similar colors, they are spread based on quintiles
    #for (nr in 1:50) {
	if (nr<5) {
		spread <- 1:nr
	} else {
		spread.step <- floor(nr/(2.5))
		spread <- seq(1, by=spread.step, length.out=nr)
		spread <- spread %% nr
 		spread[spread==0] <- nr
		dup <- which(duplicated(spread))[1]
		if (!is.na(dup)) spread <- spread + 
			rep(0:(spread.step-1), each=(dup-1), length.out=nr)
	}		
# 		print(nr)
#  		print(setequal(spread, 1:nr))
# 		print(spread)
# 	}		
	
	start <- sq[1:nr][spread]
	end <- sq[2:(nr+1)][spread]
	
	list(lb=start+spacer, ub=end-spacer)
}


hsvs <- function(x, depth) {
	H <- x[[1]]
	S <- x[[2]]
	V <- x[[3]]
	
	nr <- length(H)
	hrng <- 10
	#srng <- .15
	
	if (nr > 1) {
		# circle
		#             r <- seq(0, 2*pi, length.out=nr+1)[1:nr]
		#             H <- H + sin(r)*hrng
		#             S <- S + cos(r)*srng
		# grid
		ncol <- ceiling(sqrt(nr))
		nrow <- ceiling(nr/ncol)
		H <- H + rep(seq(-hrng, hrng,length.out=nrow), times=ncol)[1:nr]
		#S <- S + rep(seq(-srng, srng,length.out=ncol), each=nrow)[1:nr]
		S <- S * 1.1
		
		H[H<0] <- H[H<0] + 360
		H[H>360] <- H[H>360] - 360
		S[S<0] <- 0
		S[S>1] <- 1
		V <- V * .9
		#V[maxdepth==depth] <- V[maxdepth==depth] * .75
	}
	list(H=H, S=S, V=V)
}

luvs <- function(x, depth) {
	L <- x[[1]]
	U <- x[[2]]
	V <- x[[3]]
	
	nr <- length(L)
	urng <- 0.1
	vrng <- 0.1
	
	if (nr > 1) {
		# circle
		#             r <- seq(0, 2*pi, length.out=nr+1)[1:nr]
		#             H <- H + sin(r)*hrng
		#             S <- S + cos(r)*srng
		# grid
		ncol <- ceiling(sqrt(nr))
		nrow <- ceiling(nr/ncol)
		#U <- U + rep(seq(-urng, urng,length.out=nrow), times=ncol)[1:nr]
		#V <- V + rep(seq(-vrng, vrng,length.out=ncol), each=nrow)[1:nr]
		
		
		#H[H<0] <- H[H<0] + 360
		#H[H>360] <- H[H>360] - 360
		#S[S<0] <- 0
		#S[S>1] <- 1
		L <- L * 0.75
		#V[maxdepth==depth] <- V[maxdepth==depth] * .75
	}
	list(L=L, U=U, V=V)
}

treepalette <- function(dat, method="HCL", palette=NULL,...) {
	k <- ncol(dat)
	if (method=="HCL") {
		res <- treeapply(dat, list(lb=0, ub=1), fun="addRange", ...)
		
		point <- with(res, (lb+ub)/2)
		chr <- 100 - (k-res$l) * 10 #75
		lum <- 90 - res$l * 10 #95
		color <- hcl(point*360,c=chr, l=lum)
	} else if (method=="HSV") {
		#browser()
		require(colorspace)
		co <- coords(as(hex2RGB(palette), "HSV"))
		nl <- length(unique(dat[[1]])) #nlevels(dat[[1]])
		value <- lapply(as.list(as.data.frame(co)), function(x)x[1:nl])
		
		res <- treeapply(dat, value, fun="hsvs")
		color <- with(res, hex(HSV(H, S, V)))
	} else if (method=="LUV") {
		require(colorspace)
		co <- coords(as(hex2RGB(palette), "LUV"))
		nl <- length(unique(dat[[1]])) #nlevels(dat[[1]])
		value <- lapply(as.list(as.data.frame(co)), function(x)x[1:nl])
		
		res <- treeapply(dat, value, fun="luvs")
		color <- with(res, hex(LUV(L, U, V)))
	}
	color
	## test
	#plot(dt$H, dt$S)
}


treeid <- function(dat) {
	current <- apply(dat, MARGIN=1, paste, collapse=".")
	parent <- apply(dat, MARGIN=1, function(x){
		n <- length(x)-sum(is.na(x))-1
		y <- rep(NA, length(x))
		if (n>0) y[1:n] <- x[1:n]
		paste(y,collapse=".")
	})
	list(current=current, parent=parent)
}

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
	
    vdat_names <- gsub(".NA", "", vdat$current, fixed=TRUE)
	vdat_names[1] <- rootlabel
    
    ## equal string lengths
    lengths <- nchar(vdat_names)
	heads <- floor((max(lengths)-lengths)/2)
	tails <- max(lengths) - lengths - heads
    hs <- sapply(heads, function(x)paste(rep(" ", x), collapse=""))
	ts <- sapply(tails, function(x)paste(rep(" ", x), collapse=""))
	
    vdat_names <- mapply(paste0, hs, vdat_names, ts)
	
	par(mai=c(0,0,0,0))
	plot(g, vertex.size=vertex.size, vertex.frame.color=NA, vertex.label=vdat_names, layout=
		 	layout.reingold.tilford(g, circular=T, root=1), vertex.label.dist=vertex.label.dist, vertex.label.cex=vertex.label.cex, vertex.label.family=vertex.label.family, vertex.label.color=vertex.label.color, vertex.label.degree=-pi/2.5)
	invisible(g)
}


drawHCL <- function(gridsize=1e3, C=90, L=80, r=c(0.6, 0.8), cuts=NULL, marks=NULL, marks.dashed=FALSE, labels=NULL, marks.r=c(0.4, 0.8), labels.r=0.5, marks.labels.r=0.88, marks.labels.cex=0.8, labels.cex=0.8) {
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

