\name{jsTranslate}
\Rdversion{1.1}
\alias{jsTranslate}
\alias{jsTranslate,character-method}
\alias{jsTranslate,ANY-method}
\alias{jsTranslate,function-method}
\title{Translate R code to JavaScript}
\description{
  This function takes an R language object or a file containing
  R code and attempts to convert it to equivalent JavaScript code.
  It provides ways to merge R objects into the code (via the
  \code{substitute} parameter). 
}
\usage{
jsTranslate(code, substitute = list(), jsGlobals = character(), addSemiColon = NA, ...)
}
\arguments{
  \item{code}{either a language object (e.g. an expression, call, if
    statement, etc.) or the name of  file containing R code.}
  \item{substitute}{a list of R objects that can be inlined within the
    generated JavaScript code.
    These are serialized to JSON format and any reference to these
    values in the code causes this JSON representation to be substituted.
  }
  \item{jsGlobals}{the names of variables that are to be considered
    global in the JavaScript code.}
  \item{addSemiColon}{currently ignored.}
  \item{\dots}{for methods to add parameters that they understand.}
}
\details{

}
\value{
  A string giving the JavaScript code.
  This is of class "JavaScriptCode".
  We may introduce additional sub-classes, e.g. JavaScriptFunction
}
\references{
JavaScript: The Definitive Guide, David Flanagan, O'Reilly \&
Associates, Inc.
}
\author{
Duncan Temple Lang
}
\note{
  We might do more work to identify the global variables
  and also to identify where JavaScript variables should be declared,
  i.e. where to place the "var" declaration.
}

\seealso{
  See the \code{jsRewrite} function and its methods for how the
  translation is done.
}
\examples{

 jsTranslate(quote(if(is(x, "numeric")) x))

 jsTranslate(quote( x <- x + 1))

   # Rewrite a function.
 f = system.file("sampleCode", "function.R", package = "RJavaScript")
 jsTranslate(f)

   # Replace references to R variables.
 f = system.file("sampleCode", "assign.R", package = "RJavaScript")
 jsTranslate(f, list(init = c(1, 2, 3)))
 jsTranslate(f, list(init = c(1, 2, 3), w = list(abc = 1, xyz = "foo")))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{programming}


