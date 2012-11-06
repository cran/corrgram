\name{corrgram}
\alias{corrgram}
\alias{panel.bar}
\alias{panel.conf}
\alias{panel.density}
\alias{panel.ellipse}
\alias{panel.minmax}
\alias{panel.pie}
\alias{panel.pts}
\alias{panel.shade}
\alias{panel.txt}

\title{Draw a correlogram}

\description{
  The corrgram function produces a graphical display of a correlation
  matrix, called a correlogram.  The cells of the matrix can be shaded
  or colored to show the correlation value.
}

\usage{
corrgram(x, type=NULL, order = FALSE, labels, panel=panel.shade,
  lower.panel=panel, upper.panel=panel, diag.panel=NULL,
  text.panel=textPanel, label.pos=0.5, label.srt=0, cex.labels=NULL,
  font.labels=1, row1attop=TRUE, dir="", gap=0, abs=FALSE,
  col.regions=colorRampPalette(c("red","salmon","white","royalblue","navy")),
  ...)
}

\arguments{
  \item{x}{A \emph{tall} data frame with one observation per row, or a
    correlation matrix.}
  \item{type}{Use 'data' or 'cor'/'corr' to explicitly specify that 'x' is
    data or a correlation matrix.  Rarely needed.}
  \item{order}{Should variables be re-ordered?  Use TRUE/"PCA" for
    PCA-based re-ordering, "OLO" for optimal leaf ordering.
  }
  \item{labels}{Labels to use (instead of data frame variable names) for diagonal panels}
  \item{panel}{Function used to plot the contents of each panel}
  \item{lower.panel, upper.panel}{Separate panel functions used
    below/above the diagonal}
  \item{diag.panel, text.panel}{Panel function used on the diagonal}
  \item{label.pos}{Vertical placement of label in diagonal panels}
  \item{label.srt}{String rotation for diagonal labels}
  \item{cex.labels, font.labels}{Graphics parameter for diagonal panels}
  \item{row1attop}{TRUE for diagonal like " \ ", FALSE for diagonal like
    " / ".}
  \item{dir}{Use \code{dir="left"} instead of 'row1attop'}
  \item{gap}{Distance between panels}
  \item{abs}{Use absolute value of correlations for clustering?  Default
    FALSE}
  \item{col.regions}{A \emph{function} returning a vector of colors}
  \item{...}{Additional arguments passed to plotting methods.}
}

\details{

  Note: Use the 'col.regions' argument to specify colors.
  Earlier versions used a function 'col.corrgram' to specify colors.
  
  Non-numeric columns in the data will be ignored.
  
  The off-diagonal panels are specified with \code{panel.pts},
  \code{panel.pie}, \code{panel.shade}, \code{panel.bar},
  \code{panel.ellipse}, \code{panel.conf}.

  Diagonal panels are specified with
  \code{panel.txt}, \code{panel.minmax}, \code{panel.density}.

  This function is basically a modification of the \code{pairs.default}
  function with the use of customized panel functions.

  You can create your own panel functions by starting with one of the
  included panel functions and making suitable modifications.
  
  TODO: legend, grid graphics version.
}

\value{
  No value is returned.  A plot is created.
}

\references{
  Friendly, Michael.  2002.  Corrgrams: Exploratory Displays for Correlation
  Matrices.  \emph{The American Statistician}, 56, 316--324.
  url{http://www.math.yorku.ca/SCS/Papers/corrgram.pdf}
  
  A SAS macro by Michael Friendly is at
  \url{http://www.math.yorku.ca/SCS/sasmac/corrgram.html}.
}

\author{Kevin Wright}

\seealso{The \code{plotcorr} function in the \code{ellipse} package has
  some similarities.
}

\examples{

# Demonstrate density panel, confidence panel
corrgram(iris, lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)

# Most of the following are from the paper by Michael Friendly.

# Figure 2
vars2 <- c("Assists","Atbat","Errors","Hits","Homer","logSal",
           "Putouts","RBI","Runs","Walks","Years")
corrgram(baseball[,vars2], order=TRUE, main="Baseball data PC2/PC1 order",
         lower.panel=panel.shade, upper.panel=panel.pie,
         text.panel=panel.txt)

# Figure 3
baseball.cor <- cor(baseball[,vars2], use='pair')
baseball.eig <- eigen(baseball.cor)$vectors[,1:2]
e1 <- baseball.eig[,1]
e2 <- baseball.eig[,2]
plot(e1,e2,col='white', xlim=range(e1,e2), ylim=range(e1,e2))
arrows(0, 0, e1, e2, cex=0.5, col="red", length=0.1)
text(e1,e2, rownames(baseball.cor), cex=0.75)

# Figure 4b
corrgram(baseball[,vars2], order=TRUE,
         main="Baseball data (PC order)",
         panel=panel.shade, text.panel=panel.txt)

# Figure 5
corrgram(baseball, order=TRUE, main="Baseball data (PC order)")

# Figure 6. Slightly different from Friendly.
corrgram(auto, order=TRUE, main="Auto data (PC order)")

# For figure 7, 8, see this package's test suite

# Figure 11.  
corrgram(baseball[,vars2], order=TRUE,
         main="Baseball correlation ellipses",
         panel=panel.ellipse, text.panel=panel.txt, diag.panel=panel.minmax)

}

\keyword{hplot}
