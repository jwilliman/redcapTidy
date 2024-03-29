% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_redcap.R
\name{rc_read_csv}
\alias{rc_read_csv}
\title{Imported REDCap csv files and combine into single list.}
\usage{
rc_read_csv(
  folder,
  yesno = "logical",
  longitudinal = NULL,
  tidy_formats = TRUE
)
}
\arguments{
\item{folder}{The folder containing the files downloaded from the REDCap
database. The four files required include the Data Dictionary, Events
(under the 'Define my events' tab), Instrument Mappings (under the
'Designate Instruments for My Events' tab), and the raw Record data (under
the 'My Reports & Exports' tab). All files should be saved as .csv.}

\item{yesno}{Determine how to return REDCap 'Yes - No' fields; options include 'factor' (default), 'numeric', or 'logical'.}

\item{longitudinal}{Is the study longitudinal or not. If longitudinal, then event and instrument mapping files should be provided as well.}

\item{tidy_format}{Tidy variable formats or not?}
}
\value{
A named list containing four data frames: dd = metadata, evnt = Events, inst = Instrument mappings, rcrd = Records.
}
\description{
Clean raw .csv data exported from a REDCap database, and export a list of data.frames
}
