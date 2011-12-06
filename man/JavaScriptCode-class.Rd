\name{JavaScriptCode-class}
\Rdversion{1.1}
\docType{class}
\alias{JavaScriptCode-class}
\alias{JavaScriptFunction-class}
\alias{show,JavaScriptCode-method}

\title{Simple classes for representing JavaScript code}
\description{
  These classes provide a simple extension of a character vector
  to allow us to label a string as containing JavaScript code.
}
\section{Objects from the Class}{
  One creates instances simple with
  \code{new("JavaScriptCode", "javascript code")}
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"character"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{character}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "character", distance 2.
Class \code{"\linkS4class{data.frameRowLabels}"}, by class "character", distance 2.
Class \code{"\linkS4class{EnumerationValue}"}, by class "character", distance 2.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "JavaScriptCode")}: displays
      the JavaScript code.}
	 }
}
\author{Duncan Temple Lang}


\seealso{
  \code{\link{jsTranslate}}
  \code{jsRewrite}
}
\examples{
showClass("JavaScriptCode")
}
\keyword{classes}
