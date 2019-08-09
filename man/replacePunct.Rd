\name{punctReplace}
\alias{replacePunct}
\alias{punctReplace}
\title{Remove All Non-Word Punctiation}
\usage{
punctReplace(txt)
replacePunct(txt) ## old wrapper - use punctReplace() for convention
}
\arguments{
\item{txt}{a character or character vector}
}
\value{
A character or character vector without non-midword punctuation
}
\description{
Remove All Non-Word Punctiation
}
\note{
This function will not corrupt words containing punctuation, like "don't," but will ensure that there remain spaces between words that are separated by punctuation. It is imperfect but helpful.
}
