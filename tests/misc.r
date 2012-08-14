# misc.r
# Time-stamp: <14 Aug 2012 13:44:06 c:/x/rpack/corrgram/tests/misc.r>

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

# Test labels

corrgram(mtcars[2:6], order=TRUE,
         labels=c('Axle ratio','Weight','Displacement','Cylinders','Horsepower'),
         cex.labels=1.5,
         upper.panel=panel.conf, lower.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

# ----------------------------------------------------------------------------

# From a bug reported 2011-06

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

# Test panel functions
corrgram(auto, lower.panel=panel.conf, upper.panel=panel.pts)
corrgram(auto, lower.panel=panel.pie, upper.panel=panel.shade)
corrgram(auto, lower.panel=panel.ellipse, upper.panel=panel.ellipse)
# Reverse diagonal, use points in lower part
corrgram(auto, order=TRUE, dir="right",
         upper.panel=panel.ellipse, lower.panel=panel.pts, diag.panel=panel.minmax)

corrgram(auto, order=TRUE, main="Auto data (PC order)", upper.panel=panel.bar)
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
