library(lattice)
library(ggplot2)
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

# Exercise V
# create a lattice and a ggplot2 scatterplot of mean(y) vs. median(x)
# carat using the merged data set from exercise IV both with
# standard confidence intervals
se.mean = sd(data.merged[1,]) / sqrt(nrow(data.merged))
gg.scatter <- ggplot(data=data.merged,aes(x=median, y=mean, ymin=mean-se.mean,ymax=mean+se.mean))
gg.sc <- gg.scatter + geom_point() + ggtitle("Carat per Clarity") + geom_pointrange()

my.theme <- trellis.par.get()
my.theme$dot.symbol$col <- "black"
my.theme$dot.symbol$cex <- 1.5
my.theme$plot.line$col <- "black"
my.theme$plot.line$lwd <- 1.5
scatter.lattice <- xyplot(mean~median,data=data.merged,main="Carat per Clarity",par.settings=my.theme)


pdf("fig-v.pdf",width=3,height=2)
par(mar=c(2,2,0.2,0.2))
gg.sc
scatter.lattice
invisible(dev.off())
embedFonts("fig-v.pdf",options="-DPDFSETTINGS=/prepress")

# Exercise VI
# using one of the data sets from exercise I save to your local drive
# a plot showing the lattice and ggplot2 versions of a histogram
# of price conditioned according to both cut and color utilizing
# the grid package to set up the plotting device

