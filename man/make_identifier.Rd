% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated2.R
\name{make_identifier}
\alias{make_identifier}
\title{Make a unique id}
\usage{
make_identifier()

make_identifier()
}
\description{
This is an alternative to converting the timestamp into an integer to serve as an identifgier, which requires using packages such as gmp to manage within R and then adds the trouble of having to figure out how to manage it as a bigint in a database. Using DatabaseConnector's dbWriteTable function, identifiers are converted from bigz to character and this also makes it difficult to use this data for joins with OMOP vocabularies where the concept_ids are int and what homegrown identifiers are primary required for. To avoid the trouble of dealing with large integers, the timestamp is used and the "202" in the year "2020" is removed. This means that the identifier will be unique to this decade.

This is an alternative to converting the timestamp into an integer to serve as an identifgier, which requires using packages such as gmp to manage within R and then adds the trouble of having to figure out how to manage it as a bigint in a database. Using DatabaseConnector's dbWriteTable function, identifiers are converted from bigz to character and this also makes it difficult to use this data for joins with OMOP vocabularies where the concept_ids are int and what homegrown identifiers are primary required for. To avoid the trouble of dealing with large integers, the timestamp is used and the "202" in the year "2020" is removed. This means that the identifier will be unique to this decade.
}
\keyword{internal}
