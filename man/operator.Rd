% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CDO.R
\name{operator}
\alias{operator}
\title{Append a CDO Operator}
\usage{
operator(cdo_command, op, params = NULL)
}
\arguments{
\item{cdo_command}{The command structure for CDO operations.}

\item{op}{The CDO operator to be appended.}

\item{params}{Parameters for the CDO operator (optional).}
}
\value{
The updated command structure with the appended operator.
}
\description{
Appends a specified CDO operator to the command structure.
}
\examples{
\dontrun{
read_cdo("/path/to/infile.nc") \%>\%
  operator("maskregion", "/path/to/region.txt")


read_cdo(infile) \%>\%
  operator("maskregion", myregion) \%>\%
  operator("sellonlatbox", lonlatbox) \%>\%
  write_cdo(outfile)
}

}
