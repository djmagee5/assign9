library(lattice)
library(latticeExtra)
library(ggplot2)
library(grid)
data(diamonds)

# Exercise I
# create a subset of diamonds of only the ideal cut
# and a subset containing the rest, excluding those of color "J"
ideal.subset <- subset(diamonds, cut=="Ideal")
not.ideal.minus.j <- subset(diamonds, cut!="Ideal" & color!="J")

# Exercise II
# aggregate first dataframe from Ex.1 as mean carat per clarity
# and the second one as median carat per clarity
ideal.mean.per.clarity <- aggregate(ideal.subset$carat, by=list(ideal.subset$clarity), FUN=mean)
not.ideal.median.per.clarity <- aggregate(not.ideal.minus.j$carat, by=list(not.ideal.minus.j$clarity), FUN=median)

# Exercise III
# sort the first data frame from exercise 2 according to carat
names(ideal.mean.per.clarity) <- c("carat","mean")
ideal.mean.per.clarity <- ideal.mean.per.clarity[order(ideal.mean.per.clarity$carat),]

# Exercise IV
# merge the two data frames from exercise II
names(not.ideal.median.per.clarity) <- c("carat","median")
data.merged <- merge(ideal.mean.per.clarity, not.ideal.median.per.clarity, by.x="carat")

# Exercise VI
# using one of the data sets from exercise I save to your local drive
# a plot showing the lattice and ggplot2 versions of a histogram
# of price conditioned according to both cut and color utilizing
# the grid package to set up the plotting device

hist.lattice <- histogram(~ price | cut + color, data=not.ideal.minus.j,as.table=TRUE,plot.points=FALSE,between=list(x=0.2,y=0.2),scales=list(x=list(rot=45)))
lattice.hist <- useOuterStrips(hist.lattice)
hist.ggplot  <- ggplot(data=not.ideal.minus.j, aes(x=price)) + geom_histogram() + facet_grid(color~cut) + theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1))

pdf("fig-vi.pdf",width=7,height=5)
par(mar=c(2,2,0.2,0.2))
lattice.hist
hist.ggplot
#grid.arrange(lattice.hist,hist.ggplot)
invisible(dev.off())
embedFonts("fig-v.pdf",options="-DPDFSETTINGS=/prepress")
