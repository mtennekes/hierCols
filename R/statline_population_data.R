# http://statline.cbs.nl/StatWeb/publication/?VW=T&DM=SLEN&PA=37259ENG&D1=0&D2=0&D3=5-16&D4=a&HD=140301-1637&LA=EN&HDR=T,G3&STB=G1,G2
# delete top 4 and last line from csv

nlpop <- read.csv2("./data/Population_dynamics__010314163822.csv", header=FALSE, stringsAsFactors=FALSE)
names(nlpop) <- c("x", "prov", paste0("y", 1960:2012))

nlpop$ld <- factor(c("North", "North", "North", "East", "East", "East", "West", "West", "West", "West", "South", "South"), levels=c("North", "East", "West", "South"))

nlpop <- nlpop[,c("ld", "prov", paste0("y", 1960:2012))]
nlpop$prov <- substr(nlpop$prov, 1, nchar(nlpop$prov)-5)

nlpop$prov <- factor(nlpop$prov, levels=unique(nlpop$prov))
