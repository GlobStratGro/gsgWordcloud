\name{freqForWordCloud}
\alias{freqForWordCloud}
\title{Stem and Source-Matched and Create Frequency Table}
\usage{
freqForWordCloud(txt)
}
\arguments{
\item{txt}{a character or character vector to be stemmed and aggregated}
\item{mergekeep}{a character vector of words to force merge into specific groups}
\item{mergereplace}{a character vector of destinations for the force-merged words}
\item{bestkeep}{a character vector of identified best sources to force to a new name}
\item{bestreplace}{a character vector of new names to send the specified sources}
}
\value{
A dataframe with the following fields:
\item{stem}{Each stem in the provided text}
\item{freq}{The number of appearances of each stem}
\item{sources}{Every word that appeared in the input that was stemmed to this stem}
\item{best.source}{The source word with the greatest pre-stemming frequency}
}
\description{
Returns a stemmed and source matched frequency table
}
