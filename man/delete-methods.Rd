% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-files.R, R/class-task.R
\docType{methods}
\name{delete}
\alias{delete}
\alias{delete,SimpleList-method}
\alias{delete,Files-method}
\alias{delete,Task-method}
\title{Delete files or folders}
\usage{
delete(obj)

\S4method{delete}{SimpleList}(obj)

\S4method{delete}{Files}(obj)

\S4method{delete}{Task}(obj)
}
\arguments{
\item{obj}{single File or FileList}
}
\value{
system message
}
\description{
Delete files or folders
}
\examples{
\dontrun{
a$project("demo")$file("omni")$delete()
# or
delete(a$project("demo")$file("omni"))}
}
