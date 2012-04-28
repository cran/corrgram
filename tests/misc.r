# misc.r
# Time-stamp: <28 Mar 2012 15:26:12 c:/x/rpack/corrgram/tests/misc.r>

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
