library(devtools); load_all("../treemap/pkg")
library(RColorBrewer)
pal <- c(brewer.pal(9, "Set1"), brewer.pal(12, "Set3"))
source("treefunctions.R")
require(data.table)

### simple example
test <- data.frame(a=c("A", "A", "A", "A", "B", "C", "C", "C", 
					   "D", "D", "D", "D", "D", "D", "D", "E"),
				   b=c(NA, "1", "2", "3", NA, NA, "1", "2", 
				   	NA, "1", "1", "1", "2", "3", "4", NA),
				   c=c(NA, NA, NA, NA, NA, NA, NA, NA, 
				   	NA, NA, "a", "b", NA, NA, NA, NA))
test$x <- 1
# tmPlot(test, index=c("a", "b", "c"), vSize="x", type="index")


test$color <- treepalette(test[,1:3])
tmPlot(test, index=c("a", "b", "c"), vSize="x", vColor="color", type="color", palette=pal)
drawtree(test[,1:3], color=test$color)



### sbi example

sbi <- read.csv("./data/sbi_all.csv")
sbi$x <- rnorm(1412, mean=1000, sd=200)
# tmPlot(sbi, index=c("SBI1", "SBI2", "SBI3", "SBI4"), vSize="x", type="index", palette=pal)

sbi$color <- treepalette(sbi[,3:6], frc=0.5)
sbi$color <- treepalette(sbi[,3:6], method="HSV", palette=pal)
sbi$color <- treepalette(sbi[,3:6], method="LUV", palette=pal)

pdf("./plots/sbi_all.pdf", width=7, height=7)
drawtree(sbi[,3:6], color=sbi$color)
dev.off()

tmPlot(sbi, index=c("SBI1", "SBI2", "SBI3", "SBI4"), vSize="x", vColor="color", type="color", palette=pal)


### sbi (selection) example
sbiSel <- sbi[sbi$SBI1=="F" & !is.na(sbi$SBI2), ]
sbiSel$color <- treepalette(sbiSel[,4:6], frc=0.5)
#sbiSel$color <- treepalette(sbiSel[,4:6], method="HSV", palette=pal)

drawtree(sbiSel[,4:6], color=sbiSel$color)

tmPlot(sbiSel, index=c("SBI2", "SBI3", "SBI4"), vSize="x", vColor="color", type="color", palette=pal)



# ## plot luv
# co <- coords(as(hex2RGB(palette), "LUV"))
# 
# co2 <- matrix(c(0,0,0,0,60,60,60,60,
# 				-60,-60,60,60,-60,-60,60,60,
# 				-60,60,-60,60,-60,60,-60,60), ncol=3)
# 
# hex(LUV(co2))
# 
# RGB(co)
# 
# par(mfrow=c(3,2), mar=c(1,1,1,1))
# 
# for (i in 1:4) {
# 
# 	k <- 200
# 	U <- rep(seq(-100, 100, length.out=k), each=k)
# 	V <- rep(seq(-100, 100, length.out=k), times=k)
# 	col <- hex(LUV(co[i,1], U, V))
# 	m <- matrix(1:length(col), ncol=k)
# 	
# 	image(1:k,1:k,m, col=col,axes=FALSE, xlab="",ylab="")
# 	points((co[i,3]+101)/201*k, (co[i,2]+101)/201*k)
# }




