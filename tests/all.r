# all.r
# Time-stamp: <01 Jul 2011 17:15:39 c:/x/rpack/corrgram/tests/all.r>

# ----------------------------------------------------------------------------

# From bug reported 2011-06

set.seed(123)
a = seq(1,100)
b = jitter(seq(1,100), 80)
cor(a,b) # r about .95
ab=as.data.frame(cbind(a,b))
ab$c = -1 * ab$b # flip direction of correlation
cor(ab$a, ab$c) # r now about -.95
corrgram(ab, order=NULL, lower.panel=panel.pie,upper.panel=NULL, text.panel=panel.txt)

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

corrgram(auto[, -c(1:2)])
corrgram(auto[, -c(1:2)], type='data')
# corrgram(auto, type='corr') # Generates error

# ----------------------------------------------------------------------------

# Test panel functions
corrgram(auto[, -c(1:2)], lower.panel=panel.conf, upper.panel=panel.pts)
corrgram(auto[, -c(1:2)], lower.panel=panel.pie, upper.panel=panel.shade)
corrgram(auto[, -c(1:2)], lower.panel=panel.ellipse, upper.panel=panel.ellipse)

# ----------------------------------------------------------------------------

if(FALSE){
# Load datasets first
data(baseball)
data(auto)

# Figure 2
vars2 <- c("Assists","Atbat","Errors","Hits","Homer","logSal",
           "Putouts","RBI","Runs","Walks","Years")
corrgram(baseball[,vars2], order=TRUE,
         main="Baseball data PC2/PC1 order",
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

# Figure 3
baseball.cor <- cor(baseball[,vars2], use='pair')
baseball.eig <- eigen(baseball.cor)$vectors[,1:2]
e1 <- baseball.eig[,1]
e2 <- baseball.eig[,2]
plot(e1,e2,col='white', xlim=range(e1,e2), ylim=range(e1,e2))
text(e1,e2, rownames(baseball.cor), cex=1)
arrows(0, 0, e1, e2, cex=0.5, col="red", length=0.1)

# Figure 4a
corrgram(baseball[,vars2],
         main="Baseball data (alphabetic order)",
         panel=panel.shade, text.panel=panel.txt)

# Figure 4b
corrgram(baseball[,vars2], order=TRUE,
         main="Baseball data (PC order)",
         panel=panel.shade, text.panel=panel.txt)

# Figure 5
vars5 <- setdiff(colnames(base), c("Name","League","Team","Position"))
corrgram(baseball[, vars5], order=TRUE,
         main="Baseball data (PC order)",
         panel=panel.shade, text.panel=panel.txt)

# Figure 6. Arrangement is slightly different from Friendly.
vars6 <- setdiff(colnames(auto), c("Model", "Origin"))
corrgram(auto[, vars6], order=TRUE,
         main="Auto data (PC order)",
         panel=panel.shade, text.panel=panel.txt)

# Figure 11
corrgram(baseball[,vars2], order=TRUE, 
         main="Baseball correlation ellipses",
         panel=panel.ellipse, 
         text.panel=panel.txt, diag.panel=panel.minmax)

}
