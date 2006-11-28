# corrgram.r
# Time-stamp: <30 Nov 2006 13:32:49 c:/X/Rpkgs/corrgram/R/corrgram.r>

# Author: Kevin Wright
# Code for plotting ellipses was derived from the ellipse package.

# 2006.11.28 : Package version 0.1 created.
# 2006.04.16 : First work

# ----------------------------------------------------------------------------

if(FALSE) {

  # Read baseball data from Micheal Friendly's site

  base <- read.fwf(file="http://euclid.psych.yorku.ca/ftp/sas/sssg/data/baseball.sas",
                   widths=c(14,1,3,2,3,3,3,3,3,3,3,5,4,4,4,4,4,4,3,3,4),
                   skip=105,n=322)
  # Add variable names
  names(base) <- c("Name","League","Team","Position","Atbat","Hits","Homer",
                   "Runs","RBI","Walks","Years","Atbatc","Hitsc","Homerc",
                   "Runsc","RBIc","Walksc","Putouts","Assists","Errors","Salary")
  # Convert salary from factor to numeric
  base$Salary <- as.numeric(as.character(base$Salary))
  # log of salary
  base$logSal <- log10(base$Salary)
  # Merge and recode position levels
  levels(base$Position) <-  c("1B","1B","1B","2B","2B","2B","3B","3B","3B","3B",
                              "C ","C ","OF","C ","DH","DH","OF","OF","OF","OF",
                              "OF","OF","SS","SS","UT")
  # baseball <- base
  # dump("baseball", file="c:/x/Rpkgs/corrgram/data/baseball.R")
}

# ----------------------------------------------------------------------------

if(FALSE){

  # Read the auto data

  auto <- read.fwf(file="http://euclid.psych.yorku.ca/ftp/sas/sssg/data/auto.sas",
                   widths=c(17,2,1,8,3,2,2,4,5,3,5,4,3,4,5),
                   skip=39, n=74)
  # Drop column 2
  auto <- auto[,-2]
  # Specify variable names
  names(auto) <- c("Model", "Origin", "Price", "MPG", "Rep78", "Rep77", "Hroom",
                   "Rseat", "Trunk", "Weight", "Length", "Turn", "Displa", "Gratio")
  # Missing values caused some inadvertent factors.  Change to numeric
  auto$Rep78 <- as.numeric(as.character(auto$Rep78))
  auto$Rep77 <- as.numeric(as.character(auto$Rep77))
  auto$Hroom <- as.numeric(as.character(auto$Hroom))
  
  # dump("auto", file="c:/x/Rpkgs/corrgram/data/auto.R")
  # promptData(auto, "c:/x/Rpkgs/corrgram/man/auto.Rd")
}

# ----------------------------------------------------------------------------

col.corrgram <- function(ncol){
  # Colors to use for the corrgram
  # Red > White > Blue
  colorRampPalette(c("red","salmon","white","royalblue","navy"))(ncol)
}

panel.pts <- function(x, y, ...){
  plot.xy(xy.coords(x, y), type="p", ...)
  box(col="lightgray")
}

panel.pie <- function(x, y, ...){
#  box(col="gray70")
  # Coordinates of box
  usr <- par()$usr
  minx <- usr[1] #min(x, na.rm=TRUE)
  maxx <- usr[2] #max(x, na.rm=TRUE)
  miny <- usr[3] #min(y, na.rm=TRUE)
  maxy <- usr[4] #max(y, na.rm=TRUE)
  # Multiply the radius by .97 so the circles do not overlap
  rx <- (maxx-minx)/2 * .97
  ry <- (maxy-miny)/2 * .97
  centerx <- (minx+maxx)/2
  centery <- (miny+maxy)/2

  segments <- 60
  angles <- seq(0,2*pi,length=segments)
  circ <- cbind(centerx + cos(angles)*rx, centery + sin(angles)*ry)
  lines(circ[,1], circ[,2], col='gray30',...)

  # Overlay a colored polygon
  corr <- cor(x, y, use='pair')
  ncol <- 14
  pal <- col.corrgram(ncol)
  col.ind <- round(ncol*(corr+1)/2)
  col.pie <- pal[col.ind]
  
  segments <- round(60*abs(corr),0) # Watch out for the case with 0 segments
  if(segments>0){
    angles <- seq(pi/2, pi/2+(2*pi* -corr), length=segments)
    circ <- cbind(centerx + cos(angles)*rx, centery + sin(angles)*ry)
    circ <- rbind(circ, c(centerx, centery), circ[1,])
    polygon(circ[,1], circ[,2], col=col.pie)
  }
  
}

panel.shade <- function(x, y, ...){
  r <- cor(x, y, use='pair')
  ncol <- 14
  pal <- col.corrgram(ncol)
  col.ind <- round(ncol*(r+1)/2)
  usr <- par("usr")
  # Solid fill
  rect(usr[1], usr[3], usr[2], usr[4], col=pal[col.ind], border=NA)
  # Add diagonal lines
  rect(usr[1], usr[3], usr[2], usr[4], density=5,
         angle=ifelse(r>0, 45, 135), col="white")
  # Boounding box needs to plot on top of the shading, so do it last.
  box(col='lightgray')
}

panel.txt <- function(x=0.5, y=0.5, txt, cex, font){
#  box(col="lightgray")
  text(x, y, txt, cex = cex, font = font)
}

panel.minmax <- function(x, ...){
  # Put the minimum in the lower-left corner and the
  # maximum in the upper-right corner
  minx <- round(min(x, na.rm=TRUE),2)
  maxx <- round(max(x, na.rm=TRUE),2)
  text(minx, minx, minx, cex=1, adj=c(0,0))
  text(maxx, maxx, maxx, cex=1, adj=c(1,1))
}

panel.ellipse <- function(x,y, ...){
  # Draw an ellipse

#  box(col="white")
  dfn <- 2
  dfd <- length(x)-1
  shape <- var(cbind(x,y),na.rm=TRUE)
  keep <- (!is.na(x) & !is.na(y))
  center <- c(mean(x[keep]),mean(y[keep]))
  radius <- sqrt(dfn*qf(.68,dfn,dfd))
  segments <- 75
  angles <- seq(0,2*pi,length=segments)
  unit.circle <- cbind(cos(angles),sin(angles))
  ellipse.pts <- t(center+radius*t(unit.circle%*%chol(shape)))
  ellx <- ellipse.pts[,1]
  elly <- ellipse.pts[,2]
  # Truncate ellipse at min/max or at bounding box
  usr <- par()$usr
  minx <- usr[1] #min(x, na.rm=TRUE)
  maxx <- usr[2] #max(x, na.rm=TRUE)
  miny <- usr[3] #min(y, na.rm=TRUE)
  maxy <- usr[4] #max(y, na.rm=TRUE)
  ellx <- ifelse(ellx < minx, minx, ellx)
  ellx <- ifelse(ellx > maxx, maxx, ellx)
  elly <- ifelse(elly < miny, miny, elly)
  elly <- ifelse(elly > maxy, maxy, elly)
  lines(ellx, elly, col='gray30',...)

  # Filled ellipse
  # polygon(ellx, elly, col="blue", ...)
  
  # Add a lowess line through the ellipse
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = 2/3, iter = 3), 
          col = "red", ...)  
}

# End of panel functions
# ----------------------------------------------------------------------------

# The corrgram function is derived from the pairs.default function of R
corrgram <- 
  function (x, order=NULL, labels, panel = panel.shade, ...,
          lower.panel = panel, upper.panel = panel,
          diag.panel = NULL, text.panel = textPanel,
          label.pos = 0.5,
          cex.labels = NULL, font.labels = 1,
          row1attop = TRUE, gap = 0)
{

  # Order the variables by PCA of correlation matrix
  if(!is.null(order)){
    x.cor <- cor(x, use="pair")
    x.eigen <- eigen(x.cor)$vectors[,1:2]
    e1 <- x.eigen[,1]
    e2 <- x.eigen[,2]
    alpha <- ifelse(e1>0,
                    atan(e2/e1), atan(e2/e1)+pi)
    x <- x[,order(alpha)]
  }
  
    textPanel <-
        function(x = 0.5, y = 0.5, txt, cex, font)
            text(x, y, txt, cex = cex, font = font)

    localAxis <- function(side, x, y, xpd, bg, col=NULL, main, oma, ...) {
      ## Explicitly ignore any color argument passed in as
      ## it was most likely meant for the data points and
      ## not for the axis.
        if(side %%2 == 1) Axis(x, side=side, xpd=NA, ...)
        else Axis(y, side=side, xpd=NA, ...)
    }

    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localLowerPanel <- function(..., main, oma, font.main, cex.main)
        lower.panel(...)
    localUpperPanel <- function(..., main, oma, font.main, cex.main)
        upper.panel(...)

    localDiagPanel <- function(..., main, oma, font.main, cex.main)
        diag.panel(...)

    dots <- list(...); nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for(i in seq(along=names(x))) {
            if(is.factor(x[[i]]) || is.logical(x[[i]]))
               x[[i]] <- as.numeric(x[[i]])
            if(!is.numeric(unclass(x[[i]])))
                stop("non-numeric argument to 'pairs'")
        }
    } else if (!is.numeric(x)) stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
        lower.panel <- match.fun(lower.panel)
    if((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
        upper.panel <- match.fun(upper.panel)
    if((has.diag  <- !is.null( diag.panel)) && !missing( diag.panel))
        diag.panel <- match.fun( diag.panel)

    if(row1attop) {
        tmp <- lower.panel; lower.panel <- upper.panel; upper.panel <- tmp
        tmp <- has.lower; has.lower <- has.upper; has.upper <- tmp
    }

    nc <- ncol(x)
    if (nc < 2) stop("only one column in the argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels)) labels <- paste("var", 1:nc)
    }
    else if(is.null(labels)) has.labs <- FALSE
    oma <- if("oma" %in% nmdots) dots$oma else NULL
    main <- if("main" %in% nmdots) dots$main else NULL
    if (is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main)) oma[3] <- 6
    }
    opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))

    for (i in if(row1attop) 1:nc else nc:1)
        for (j in 1:nc) {
            localPlot(x[, j], x[, i], xlab = "", ylab = "",
                      axes = FALSE, type = "n", ...)
            if(i == j || (i < j && has.lower) || (i > j && has.upper) ) {
                #box()
              # Remove the tick marks and tick labels (unlike the pairs function)
              # tick=FALSE, labels=FALSE in the call to localAxis
              # We might WANT to use ticks if the panel function is 'points'
#                if(i == 1  && (!(j %% 2) || !has.upper || !has.lower ))
#                    localAxis(1 + 2*row1attop, x[, j], x[, i], tick=FALSE, labels=FALSE, ...)
#                if(i == nc && (  j %% 2  || !has.upper || !has.lower ))
#                    localAxis(3 - 2*row1attop, x[, j], x[, i], ...)
#                if(j == 1  && (!(i %% 2) || !has.upper || !has.lower ))
#                    localAxis(2, x[, j], x[, i], ...)
#                if(j == nc && (  i %% 2  || !has.upper || !has.lower ))
#                    localAxis(4, x[, j], x[, i], ...)
                mfg <- par("mfg")
                if(i == j) {
                    if (has.diag) localDiagPanel(as.vector(x[, i]), ...)
                    if (has.labs) {
                        par(usr = c(0, 1, 0, 1))
                        if(is.null(cex.labels)) {
                            l.wid <- strwidth(labels, "user")
                            cex.labels <- max(0.8, min(2, .9 / max(l.wid)))
                        }
                        text.panel(0.5, label.pos, labels[i],
                                   cex = cex.labels, font = font.labels)
                    }
                } else if(i < j)
                    localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
                else
                    localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
                if (any(par("mfg") != mfg))
                    stop("the 'panel' function made a new plot")
            } else par(new = FALSE)

        }
    if (!is.null(main)) {
        font.main <- if("font.main" %in% nmdots) dots$font.main else par("font.main")
        cex.main <- if("cex.main" %in% nmdots) dots$cex.main else par("cex.main")
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
}


# Right now we have to specify both lower.panel and upper.panel instead
# of just panel.  This is probably some matching problem as we call pairs()

# Might be nice to have a key for the colors.

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

# Figure 7, 8, 9
# To re-create figure 7, 8 and 9, we need to re-write corrgram to
# work with a correlation matrix instead of a data matrix

# Figure 11
corrgram(baseball[,vars2], order=TRUE, 
         main="Baseball correlation ellipses",
         panel=panel.ellipse, 
         text.panel=panel.txt, diag.panel=panel.minmax)

}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

if (FALSE) {
# Figure 6.  Arrangement is slightly different from Friendly.
vars6 <- c("price","mpg","rep78","rep77","hroom","rseat","trunk","weight",
          "length","turn","displa","gratio")
auto.cor <- cor(auto[,vars6], use='pair')
auto.eigen <- eigen(auto.cor)$vectors[,1:2]
e1 <- auto.eigen[,1]
e2 <- auto.eigen[,2]
alpha <- ifelse(e1>0, atan(e2/e1), atan(e2/e1)+pi)
ord <- order(alpha)
pairs(auto[,vars6[ord]], gap=0, row1attop=FALSE,tick=FALSE, col.axis="white",
      label.pos=0.5, main="Auto data",
      panel=panel.shade, text.panel=panel.txt)

# Why is the arrangement different?  See this picture for clue.
plot(e1,e2,col='white')
text(e1,e2, rownames(auto.cor), cex=0.5)
arrows(0, 0, e1, e2, col="red")
rownames(auto.cor)[ord]

# Figure 7
base7.cor <- cor(base[,rev(vars4b)], use='pair')
rinv <- solve(base7.cor)
dia <- diag(1/sqrt(diag(rinv)))
part <- -dia %*% rinv %*% dia
# Replace diagonal elements with zero
diag(part) <- 0
image(part, col=paletteRedWhiteBlue2(11), zlim=c(-1,1), xaxt='n', yaxt='n',
      main="Baseball data: R inv")
axis(1, seq(from=0, to=1, length=ncol(part)), 
     labels=colnames(base7.cor),las=2, cex.axis=0.75)
axis(2, seq(from=0, to=1, length=ncol(part)), 
     labels=colnames(base7.cor),las=2, cex.axis=0.75)

# Figure 8
pcor <- function(x,y){
  # Canonical correlation of x given y
  Rx.y <- cov(x,use='pair') - cov(x,y, use='pair') %*%
    solve(cov(y, use='pair')) %*% cov(y,x, use='pair')
  # Partial correlation of x given y
  Dx.y <- diag(1/sqrt(diag(Rx.y)))
  # Partial correlations
  Rpart <- Dx.y %*% Rx.y %*% Dx.y
  return(Rpart)                                  
}
vars8a <- c("turn","displa","weight","length","rseat","trunk","hroom","rep77","rep78","gratio")
vars8b <- c("price","mpg")
vars8 <- c(vars8b, vars8a)
auto.pcor1 <- pcor(auto[,vars8a], auto[,vars8b])
auto.pcor2 <- cor(auto[,vars8b])
# Assemble together
na <- length(vars8a)
nb <- length(vars8b)
auto.pcor <- matrix(0, nrow=nb+na, ncol=nb+na)
auto.pcor[1:nb, 1:nb] <- auto.pcor2
auto.pcor[(nb+1):(na+nb),(nb+1):(na+nb) ] <- auto.pcor1
diag(auto.pcor) <- 0 # So the diagonal will appear white
colnames(auto.pcor) <- vars8
rownames(auto.pcor) <- vars8
image(auto.pcor, col=paletteRedWhiteBlue2(11), zlim=c(-1,1), xaxt='n', yaxt='n',
      main="Auto data: Partialling Price, MPG")
axis(1, seq(from=0, to=1, length=ncol(auto.pcor)), 
     labels=vars8,las=2, cex.axis=0.75)
axis(2, seq(from=0, to=1, length=ncol(auto.pcor)), 
     labels=vars8,las=2, cex.axis=0.75)

}
