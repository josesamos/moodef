% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/support.R
\name{vector_to_string}
\alias{vector_to_string}
\title{Transforms a vector of strings into a string}
\usage{
vector_to_string(vector)
}
\arguments{
\item{vector}{A vector of strings.}
}
\value{
A string.
}
\description{
Insert the separator that we consider to later perform the reverse operation.
}
\examples{

s <- vector_to_string(c('Addition', '+'))

}
\seealso{
Other support functions: 
\code{\link{create_question_csv}()},
\code{\link{create_question_data_frame}()},
\code{\link{create_question_excel}()},
\code{\link{read_question_csv}()},
\code{\link{read_question_excel}()}
}
\concept{support functions}
