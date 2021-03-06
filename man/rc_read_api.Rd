% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_redcap.R
\name{rc_read_api}
\alias{rc_read_api}
\title{Read in record and metadata from REDCap using redcapAPI}
\usage{
rc_read_api(url, token)
}
\arguments{
\item{url}{URL for a REDCap database API. Check your institution's REDCap
documentation for this address.}

\item{token}{REDCap API token}
}
\value{
A named list containing four dataframes: dd = metadata, evnt =
  Events, inst = Instrument mappings, rcrd = Records.
}
\description{
A wrapper for importing data Records, data dictionary, events, and instrument
designation mappings from REDCap. Uses the redcapAPI package.
}
