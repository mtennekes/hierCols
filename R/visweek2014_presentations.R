library(ggplot2)
new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")


## HCL pie
r  <- seq(0,1,length=1000)
th <- seq(-.65*pi, 1.1*pi, length=1000)
d  <- expand.grid(r=r,th=th)

map <- function(x, inner, outer) {
    xnorm <- (x-min(r))/(max(r)-min(r))
    inner + (xnorm * (outer-inner))
}

gg <- with(d,data.frame(d,x=r*sin(th),y=r*cos(th)/2,
                        h=360*th/(2*pi),c=map(r, 0, 90), l=70))
ggg <- gg[(gg$r==1 &  gg$y<=0) | (gg$th==-.65*pi),]

glist <- list()
for (i in 120:1) {
    gt <- ggg
    gt$y <- gt$y - i/175
    gt$l <- 80-(i/2)
    glist[[121-i]] <- gt
}

gg <- do.call("rbind", c(glist, list(gg)))
gg <- within(gg, z <- hcl(h, c, l))

t1 <- seq(-.5*pi, -.65*pi, length.out=100)
t2 <- seq(1.1*pi, .5*pi, length.out=900)
gg_edge1 <- data.frame(x=c(1.005*sin(t1), 0), y=1.005*c(cos(t1)/2, 0))
gg_edge2 <- data.frame(x=1.005*sin(t2), y=1.005*cos(t2)/2)
    
g <- ggplot(gg) +
    geom_point(aes(x,y, color=z), size=1)+
    scale_color_identity()+labs(x="",y="") +
    geom_line(data=gg_edge1, aes(x,y), color="black") +
    geom_line(data=gg_edge2, aes(x,y), color="black") +
    coord_fixed() +
    new_theme_empty

ggsave("./ff_presentation/hcl_pie.png", width=4, height=4, dpi = 96)

