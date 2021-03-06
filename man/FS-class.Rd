% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-filesystem.R
\docType{class}
\name{FS-class}
\alias{FS-class}
\alias{FS}
\title{FS class}
\arguments{
\item{server_address}{placehoder}

\item{api_address}{placehoder}

\item{vsfs_jar}{placehoder}

\item{cache_dir}{placehoder}

\item{cache_size}{placeholder}

\item{project_id}{placeholder}
}
\description{
FS class
}
\section{Methods}{

\describe{
\item{\code{file(id = NULL)}}{given project id, show all files in it}

\item{\code{mount(
  mount_point = NULL,
  project_id = NULL,
  ignore.stdout = TRUE,
  sudo = TRUE,
  ...
)}}{mount a specific project if project_id is provided, otherwise mount all projects}

\item{\code{path(id = NULL)}}{List path for all mounted projects, for easy copy/paste of file path. If project id is provoded, show project path and files path.}

\item{\code{unmount(mount_cmd = NULL, project_id = NULL, ...)}}{unmount a project if project_id is provided, otherwise unmount all}
}}

