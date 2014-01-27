#library(treemap)
devtools::load_all("../treemap/pkg")

data(business)

#treegraph(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), show.labels=FALSE, vertex.layout=igraph::layout.auto)

## mooi voorbeeld voor in paper (meerdere evenwichten)
treegraph(business, index=c("NACE1", "NACE2", "NACE3"), show.labels=FALSE, vertex.layout=igraph::layout.fruchterman.reingold)

treegraph(business, index=c("NACE1", "NACE2", "NACE3"), show.labels=FALSE, vertex.layout=igraph::layout.auto)


source("./R/survey_data.R")

#pdf(file="../plots/tm1hcp.pdf", width=9, height=7)
tm <- treemap(dat, index=c("h1", "h2", "h3"), vSize="value", title="", bg.labels=255)  
#dev.off()


str(tm)






#pdf(file="../plots/tm1ref.pdf", width=9, height=7)
treemap(dat, index=c("h1", "h2", "h3"), vSize="value", title="", vColor="h1", type="categorical", position.legend="none", palette="Set1")  
#dev.off()

## bar chart
source("./R/survey_data.R")
dats_bar <- generateRandomHierData(2, seeds=c(20140127, 20140128), levs=c(3, 9, 22))


plotBar(dats_bar[[2]], method="")




#ggsave("bar_chart.pdf", path="plots", plot=g, width=3, height=4, scale=1.5)


