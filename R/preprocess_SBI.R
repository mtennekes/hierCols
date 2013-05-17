sbi <- read.csv("./data/sbi_all.csv")
sbi <- sbi[!duplicated(sbi[,3:6,]),]

### add nace code names
nace <- readLines("./data/nace_names.txt")
nace <- strsplit(nace,split=" - ")

nace <- data.frame(code=sapply(nace, function(x)x[1]), name=sapply(nace, function(x)x[2]), stringsAsFactors=FALSE)
nace_letters <- nchar(nace$code)>1

# remove letter in front of code
nace$code[nace_letters] <- substr(nace$code[nace_letters], 2, nchar(nace$code[nace_letters]))

# add 0's
nace_sel <- substr(nace$code, 2,2)=="." | (nchar(nace$code)==1 & nace_letters)
nace$code[nace_sel] <- paste0("0", nace$code[nace_sel])

nace$name <- paste(nace$code, nace$name, sep=" - ")

# remove second dot
gr <- gregexpr(".", nace$code, fixed=TRUE)
gr <- sapply(gr, function(x)ifelse(length(x)>1, x[2], NA))
nace$code[!is.na(gr)] <- paste0(substr(nace$code[!is.na(gr)],1, gr[!is.na(gr)]-1), 
                                substr(nace$code[!is.na(gr)],gr[!is.na(gr)]+1, nchar(nace$code[!is.na(gr)])))



sbi <- cbind(sbi, treeid(sbi[,3:6]))

sbi$name <- nace$name[match(as.character(sbi$code), nace$code)]

sbi$name1 <- nace$name[match(as.character(sbi$SBI1), nace$code)]
sbi$name2 <- nace$name[match(sprintf("%02d", sbi$SBI2), nace$code)]
sbi$name3 <- NA

sbi$name3[!is.na(sbi$SBI3)] <- nace$name[match(as.character(sbi$code)[!is.na(sbi$SBI3)], nace$code)]

sbi$name3[sbi$SBI.level=="SBI4"] <- sbi$name3[match(sbi$parent[sbi$SBI.level=="SBI4"], sbi$current)]

sbi$name4[!is.na(sbi$SBI4)] <- nace$name[match(as.character(sbi$code)[!is.na(sbi$SBI4)], nace$code)]
