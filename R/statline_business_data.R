# delete first and last line in csv file:
d <- read.csv2("./data/Bedrijfsleven__finan_300114085919.csv", stringsAsFactors=FALSE)


d1 <- d[1:306, c(3,5,6)]
names(d1) <- c("SBI", "emp2010", "emp2011")
d2 <- d[307:612, c(3,5,6)]
names(d2) <- c("SBI", "turn2010", "turn2011")

d <- cbind(d1, d2[,2:3])

d$emp2010[d$emp2010 %in% c("x", "-", ".")] <- NA
d$emp2011[d$emp2011 %in% c("x", "-", ".")] <- NA
d$turn2010[d$turn2010 %in% c("x", "-", ".")] <- NA
d$turn2011[d$turn2011 %in% c("x", "-", ".")] <- NA

d$emp2010 <- as.numeric(d$emp2010)
d$emp2011 <- as.numeric(d$emp2011)
d$turn2010 <- as.numeric(d$turn2010)
d$turn2011 <- as.numeric(d$turn2011)

spaces <- sapply(gregexpr(" ", d$SBI), function(x)x[[1]][1])
d$sbi <- substr(d$SBI, 1, spaces-1)
unique(d$sbi)

## remove merged sbi cells
d <- d[-grep("-", d$sbi, fixed=TRUE), ]
d <- d[-grep("+", d$sbi, fixed=TRUE), ]
d <- d[-grep(",", d$sbi, fixed=TRUE), ]
d <- d[d$sbi!="Traditionele", ]

# remove internal aggregates
d <- d[!(d$sbi %in% LETTERS),]

isheadstr <- function(x, y) {
    sapply(x, FUN=function(z){
        any((substr(y, 1, nchar(z)) == z) & nchar(z) < nchar(y))
    })
}

d <- d[!isheadstr(d$sbi, d$sbi), ]

# truncate at SBI4
d$sbi <- substr(d$sbi, 1, 4)

d <- aggregate(d[,2:5], by=list(sbi=d$sbi), sum)

business$n2 <- substr(as.character(business$NACE2), 1, 2)
business$n3 <- paste0(substr(as.character(business$NACE3), 1, 2),
                      substr(as.character(business$NACE3), 4, 4))
business$n4 <- paste0(substr(as.character(business$NACE4), 1, 2),
                      substr(as.character(business$NACE4), 4, 4),
                      substr(as.character(business$NACE4), 6, 6))

d$level <- nchar(d$sbi)

d$sbi[d$level==2] %in% business$n2
d$sbi[d$level==3] %in% business$n3
d$sbi[d$level==4] %in% business$n4

d$n1 <- NA
d$n2 <- NA
d$n3 <- NA
d$n4 <- NA

d[d$level==2, c("n1", "n2")] <- lapply(business[match(d$sbi[d$level==2], business$n2), c("NACE1", "NACE2")], as.character)
d[d$level==3, c("n1", "n2", "n3")] <- lapply(business[match(d$sbi[d$level==3], business$n3), c("NACE1", "NACE2", "NACE3")], as.character)
d[d$level==4, c("n1", "n2", "n3", "n4")] <- lapply(business[match(d$sbi[d$level==4], business$n4), c("NACE1", "NACE2", "NACE3", "NACE4")], as.character)

d$n1 <- factor(d$n1, levels=levels(business$NACE1))
d$n2 <- factor(d$n2, levels=levels(business$NACE2))
d$n3 <- factor(d$n3, levels=levels(business$NACE3))
d$n4 <- factor(d$n4, levels=levels(business$NACE4))

