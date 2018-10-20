library("sevenbridges")

in.lst <- list(
  input(
    id = "number",
    description = "number of observations",
    type = "integer",
    label = "number",
    prefix = "--n",
    default = 1,
    required = TRUE,
    cmdInclude = TRUE
  ),
  input(
    id = "min",
    description = "lower limits of the distribution",
    type = "float",
    label = "min",
    prefix = "--min",
    default = 0
  ),
  input(
    id = "max",
    description = "upper limits of the distribution",
    type = "float",
    label = "max",
    prefix = "--max",
    default = 1
  ),
  input(
    id = "seed",
    description = "seed with set.seed",
    type = "float",
    label = "seed",
    prefix = "--seed",
    default = 1
  )
)

# the same method for outputs
out.lst <- list(
  output(
    id = "random",
    type = "file",
    label = "output",
    description = "random number file",
    glob = "*.txt"
  ),
  output(
    id = "report",
    type = "file",
    label = "report",
    glob = "*.html"
  )
)

rbx <- Tool(
  id = "runif",
  label = "Random number generator",
  hints = requirements(docker(pull = "tengfei/runif"), cpu(1), mem(2000)),
  baseCommand = "runif.R",
  inputs = in.lst, # or ins.df
  outputs = out.lst
)

fl <- "inst/docker/sevenbridges/rabix/runif.json"
write(rbx$toJSON(pretty = TRUE), fl)
