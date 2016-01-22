## For internal testing only, don't run
## missing token
message("loading ...")
library(sbgr)

if(clean){
    p <- a$project("API")
    message("cleaning ...")
    if(is(p, "Project"))
        p$delete()
}
## get billing info
message("billing info ...")
b <- a$billing()
## create project
message("creating new project 'API'")
a$project_new(name = "API", description = "API tutorial",
              billing_group_id = b[[1]]$id)
p <- a$project("API")

## get data
fl <- system.file("extdata", "sample1.fastq", package = "sbgr")
## create meta data
fl.meta <- list(file_type = "fastq",
                seq_tech = "Illumina",
                sample = "sample1",
                author = "tengfei")
## upload data with metadata
message("uploading file ...")
p$upload(fl, metadata = fl.meta)
## check uploading success
f.file <- p$file(basename(fl))
## get the pipeline from public repos
message("copying pipelines ...")
f.pipe <- a$pipeline(pipeline_name = "FastQC")
## copy the pipeline to your poject
p$pipeline_add(pipeline_name = f.pipe$name)
## get the pipeline from your project not public one
f.pipe <- p$pipeline(name = "FastQC")
## check the inputs needed for running tasks
f.pipe$details()
## Ready to run a task? go
message("running task ...")
f.task <- p$task_run(name = "my task",
                      description = "A text description",
                      pipeline_id = f.pipe$id,
                      inputs = list(
                          "177252" = list(f.file$id)
                          ))

## or you can just run with Task constructor
f.task2 <- Task(auth = a,
               name = "my task2",
               description = "A text description",
               pipeline_id = f.pipe$id,
               project_id = p$id,
               inputs = list(
                   "177252" = list(f.file$id)
                   ))
f.task2$run()
## Abort the task
f.task2$abort()
## Monitor you task
f.task$monitor(30)

## download a task output files
f.task$download("~/Desktop/")


