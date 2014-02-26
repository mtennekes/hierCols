# source: statline table: http://statline.cbs.nl/StatWeb/publication/?VW=T&DM=SLEN&PA=81156ENG&D1=3,5&D2=a&D3=1-2&HD=140226-1411&LA=EN&HDR=G2&STB=T,G1
# delete first and last line in downloaded csv file:
d <- read.csv2("./data/Trade_and_industry___260214140944.csv", stringsAsFactors=FALSE)


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
#d <- d[!(d$sbi %in% LETTERS),]


d$depth <- ifelse(d$sbi %in% LETTERS, 1, nchar(d$sbi))


## get sbi codes per layer
d[, paste0("n", 1:5)] <- NA
for (i in 1:5) {
    d[[paste0("n", i)]][d$depth==i] <- d$sbi[d$depth==i] 
}


letters2d <- list(A=1:4, B=5:9, C=10:34, D=35, E=36:39, F=40:44,
                  G=45:48, H=49:54, I=55:57, J=58:63, K=64:67, L=68, 
                  M=69:76, N=77:83, O=84, P=85, Q=86:89, R=90:93, S=94:96,
                  T=97:98, U=99)
letters2d <- unlist(letters2d)
letters2d <- substr(names(letters2d), 1, 1)

for (i in 5:3) {
    sel <- !is.na(d[[paste0("n", i)]])
    d[[paste0("n", i-1)]][sel] <- substr(d[[paste0("n", i)]][sel], 1, i-1) 
}
d$n1[d$depth>=2] <- letters2d[as.integer(d$n2[d$depth>=2])]

# for (i in 1:5) {
#     print(setdiff(d[[paste0("n", i)]], d$sbi))
# }


business$n1 <- substr(as.character(business$NACE1), 1, 1)
business$n2 <- substr(as.character(business$NACE2), 1, 2)
business$n3 <- paste0(substr(as.character(business$NACE3), 1, 2),
                      substr(as.character(business$NACE3), 4, 4))
business$n4 <- paste0(substr(as.character(business$NACE4), 1, 2),
                      substr(as.character(business$NACE4), 4, 4),
                      substr(as.character(business$NACE4), 6, 6))


## assign sbi names
d[, paste0("N", 1:5)] <- NA
for (i in 1:4) {
    d[[paste0("N", i)]] <- d$SBI[match(d[[paste0("n", i)]], d$sbi)]
    miss <- which(!is.na(d[[paste0("n", i)]]) & is.na(d[[paste0("N", i)]]))
    d[[paste0("N", i)]][miss] <- as.character(business[[paste0("NACE", i)]][match(d[[paste0("n", i)]][miss], business[[paste0("n", i)]])])
        
}

d$N5[d$depth==5] <- d$SBI[d$depth==5]


## remove higher aggregates
isheadstr <- function(x, y) {
    sapply(x, FUN=function(z){
        any((substr(y, 1, nchar(z)) == z) & nchar(z) < nchar(y))
    })
}
d <- d[!isheadstr(d$sbi, d$sbi), ]

