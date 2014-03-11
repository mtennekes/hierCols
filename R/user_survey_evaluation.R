### survey evaluation
us <- read.csv("user_survey/survey_evaluation.txt", header=TRUE, sep="\t", stringsAsFactors=FALSE)


s <- list(
    cb = us[1:3,-1],
    g1.a = us[4:8, -1],
    g1.b = us[9:10, -1],
    g1.eval = us[11:15, -1],
    g2.a = us[16:20, -1],
    g2.b = us[21:22, -1],
    g2.eval = us[23:27, -1],
    g.mooi = us[28:30, -1],
    g.inter = us[31:33, -1],
    g.over = us[34:36, -1],
    t1.a = us[37:42, -1],
    t1.b = us[43:46, -1],
    t1.eval = us[47:51, -1],
    t2.a = us[52:57, -1],
    t2.b = us[58:61, -1],
    t2.eval = us[62:66, -1],
    t.mooi = us[67:69, -1],
    t.inter = us[70:72, -1],
    t.over = us[73:75, -1],
    b1.a = us[76:81, -1],
    b1.eval = us[82:86, -1],
    b2.a = us[87:92, -1],
    b2.eval = us[93:97, -1],
    b.mooi = us[98:100, -1],
    b.inter = us[101:103, -1],
    b.over = us[104:106, -1])
    
s <- lapply(s, function(x){
    x$antwoord <- factor(x$antwoord, levels=unique(x$antwoord))
    x
})

# reverse version 2 answers
q <- c("g.mooi", "g.inter", "g.over", "t.mooi", "t.inter", "t.over", "b.mooi", "b.inter", "b.over")
s[q] <- lapply(s[q], )

