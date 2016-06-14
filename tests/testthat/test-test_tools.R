library(sevenbridges)

#context("utils")

in.df <- data.frame(id = c("number", "min", "max", "seed"),
                    description = c("number of observation", 
                                    "lower limits of the distribution",
                                    "upper limits of the distribution",
                                    "seed with set.seed"),
                    type = c("integer", "float", "float", "float"),
                    label = c("number" ,"min", "max", "seed"), 
                    prefix = c("--n", "--min", "--max", "--seed"),
                    default = c(1, 0, 10, 123), 
                    required = c(TRUE, FALSE, FALSE, FALSE))
out.df <- data.frame(id = c("res"),
                     type = c("file"),
                     glob = c("res.txt"))
rbx <- Tool(id = "runif",
            label = "Random number generator",
            hints = requirements(docker(pull = "ubuntu:14.04"), 
                                 cpu(1), mem(2000)),
            baseCommand = "echo 'radi!' > res.txt",
            inputs = in.df, 
            outputs = out.df)
params <- list(number=33)

# test setting the env - docker-in-docker
test_that("Docker-in-docker set env", {
    dind <- set_test_env("dind", "tengfei/testenv", getwd())
    expect_that(stringr::str_length(dind), equals(64))
})

# test setting the env - docker-beside-docker
test_that("Docker-besides-docker set env", {
    host <- set_test_env("host", "tengfei/testenv", getwd())
    expect_that(stringr::str_length(host), equals(64))
})

#test execution
#test_tool_bunny(rbx, params)
#test_tool_cwlrun(rbx, params)
#test_tool_rabix(rbx, params)

