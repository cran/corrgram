# misc.r
# Time-stamp: <06 Nov 2012 14:13:17 c:/x/rpack/corrgram/tests/misc.r>

# ----------------------------------------------------------------------------

# Region colors used to default to the function "col.corrgram", but when the namespace
# was forced on this package, the panel functions would look for "col.corrgram" inside
# the namespace first, and never look in the global environment.
# Bug report by Rob Kabacoff.

# Use green -> brown colors
col.earth <- function(ncol){  
  colorRampPalette(c("darkgoldenrod4", "burlywood1", "darkkhaki", "darkgreen"))(ncol)}
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie,
         text.panel=panel.txt, main="A Corrgram of a Different Color",
         col.regions=col.earth)
 
# ----------------------------------------------------------------------------

# Print diagonal text unclipped.
# This has a slight quirk...the red box is only drawn the first time.  Calling
# corrgram a 2nd time doesn't draw the red box.

require('gridBase')
unclipped.txt <- function(x=0.5, y=0.5, txt, cex, font, srt){
  vps <- baseViewports()
  vps$figure$clip <- NA # Hack. Do NOT clip text that falls outside the ploting region
  pushViewport(vps$inner) # Figure region
  grid.rect(gp=gpar(lwd=3, col="red"))
  pushViewport(vps$figure) # The diagonal box region
  grid.rect(gp=gpar(lwd=3, col="blue"))
  grid.text(txt, x=x,y=y, just='center', gp=gpar(cex=cex))
  popViewport(2)
}
corrgram(mtcars[2:6], order=TRUE,
         labels=c('Axle ratio','Weight','Displacement','Cylinders','Horsepower'),
         cex.labels=2,
         upper.panel=panel.conf, lower.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=unclipped.txt)

# ----------------------------------------------------------------------------

# Test "labels" argument

corrgram(mtcars[2:6], order=TRUE,
         labels=c('Axle ratio','Weight','Displacement','Cylinders','Horsepower'),
         cex.labels=1.5,
         upper.panel=panel.conf, lower.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

# ----------------------------------------------------------------------------

# Bug with negative correlation

set.seed(123)
a = seq(1,100)
b = jitter(seq(1,100), 80)
cor(a,b) # r about .95
ab=as.data.frame(cbind(a,b))
ab$c = -1 * ab$b # flip direction of correlation
cor(ab$a, ab$c) # r now about -.95
corrgram(ab, order=NULL, lower.panel=panel.pie, upper.panel=NULL,
         text.panel=panel.txt)

corrgram(ab)

# ----------------------------------------------------------------------------

# Test 'order' argument

corrgram(mtcars)
corrgram(mtcars, order=NULL)
corrgram(mtcars, order=FALSE)
corrgram(mtcars, order=TRUE)
corrgram(mtcars, order="PC")
corrgram(mtcars, order="OLO")
corrgram(mtcars, order="PC", abs=TRUE)
corrgram(mtcars, order="OLO", abs=TRUE)

# ----------------------------------------------------------------------------

# Test 'type'
corrgram(vote)
corrgram(vote, type='corr')
corrgram(vote, type='data') # Warn user
corrgram(vote, lower.panel=panel.conf)

corrgram(auto)
corrgram(auto, type='data')
# corrgram(auto, type='corr') # Generates error

# ----------------------------------------------------------------------------

# Test that non-numeric columns in data.frame are ignored.

mt2 <- mtcars
mt2$model <- rownames(mt2)
corrgram(mt2)
# ----------------------------------------------------------------------------

# Test all the panel functions
corrgram(auto, lower.panel=panel.conf, upper.panel=panel.pts)
corrgram(auto, lower.panel=panel.pie, upper.panel=panel.shade)
corrgram(auto, lower.panel=panel.ellipse, upper.panel=panel.ellipse)
corrgram(auto, order=TRUE, main="Auto data (PC order)", upper.panel=panel.bar)
corrgram(auto, lower.panel=panel.shade, upper.panel=NULL)

# Test the diagonal direction.  Reverse diagonal, use points in lower part
corrgram(auto, order=TRUE, dir="right",
         upper.panel=panel.ellipse, lower.panel=panel.pts, diag.panel=panel.minmax)
corrgram(auto, order=TRUE, dir="/",
         upper.panel=panel.ellipse, lower.panel=panel.pts, diag.panel=panel.minmax)

# ----------------------------------------------------------------------------

# Missing value in a correlation matrix.
# Use white (transparent?) color for the shading

vote2 <- vote
vote2[2:6,2:6] <- NA
corrgram(vote2)

# ----------------------------------------------------------------------------

# Missing combinations of data could cause cor( , use="pair") to
# give NAs

dat <- data.frame(E1=c(NA,NA,NA,NA,NA,6,7,8,9,10),
                  E2=c(1,2,3,4,5,NA,NA,NA,NA,NA),
                  E3=c(1,2,3,4,5,6,7,8,9,10)+.1,
                  E4=c(2,1,5,6,8,7,9,4,5,3))
cor(dat, use="pair")
corrgram(dat)

# ----------------------------------------------------------------------------

# Manually add a legend for coloring points

panel.colpts <- function(x, y, corr=NULL, col.regions, ...){  
  # For correlation matrix, do nothing
  if(!is.null(corr)) return()
  plot.xy(xy.coords(x, y), type="p", ..., col=1:2)
  box(col="lightgray")
}
corrgram(auto, lower.panel=panel.conf, upper.panel=panel.colpts)

require(grid)
grid.clip()
pushViewport(viewport(.5, .95, width=stringWidth("Group1"),
                      height=unit(2,"lines"),
                      name="pagenum", gp=gpar(fontsize=8)))
grid.legend(pch=1:2, labels=c("Group1","Group2"), gp=gpar(col=c('red')))
popViewport()

# ----------------------------------------------------------------------------

# How can I put the variable names outside the plot?  This shows one way...
# horribly ugly hack.
side.txt <- function (x = 0.5, y = 0.5, txt, cex, font, srt) {
  NULL
}

corrgram(mtcars[2:6], order=TRUE,
         labels=c('Axle ratio','Weight','Displacement','Cylinders','Horsepower'),
         cex.labels=1.5,
         upper.panel=panel.conf, lower.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=side.txt)

require("grid")
grid.clip()
lab <- "Displacement"

pushViewport(viewport(.04, .5, width = stringWidth(lab), angle=90, 
                      height = unit(2, "lines"), name = "pagenum", gp = gpar(fontsize = 8)))
grid.text(lab, gp=gpar(srt=45), just = c("left", "bottom"))
popViewport()

pushViewport(viewport(.5, .04, width = stringWidth(lab),
                      height = unit(2, "lines"), name = "pagenum", gp = gpar(fontsize = 8)))
grid.text(lab, gp=gpar(srt=45), just = c("left", "bottom"))
popViewport()

