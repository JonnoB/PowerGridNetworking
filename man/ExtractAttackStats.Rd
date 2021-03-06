% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExtractAttackStats.R
\name{ExtractAttackStats}
\alias{ExtractAttackStats}
\title{Extracts Attack stats from saved}
\usage{
ExtractAttackStats(
  RootFolder,
  NewFolderPath,
  Generation = "BalencedPower",
  EdgeName = "name",
  PowerFlow = "PowerFlow",
  Link.Limit = "Link.Limit"
)
}
\arguments{
\item{RootFolder}{:The folder where the attack files are saved}

\item{NewFolderPath}{: The folder where the summary files will be saved to}

\item{Generation}{The name of the variable that stores the net generation data. character string}

\item{EdgeName}{A character string. The default is "name".}

\item{PowerFlow}{A character String. The default is "PowerFlow".}

\item{Link.Limit}{A character string. The default is "Link.Limit".}
}
\description{
Extracts the attack statistics for each network in a folder and combines it into a single file that is also saved.
This function only works on a folder of folders...It needs to be changed
}
