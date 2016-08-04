#' Build workflow
#'
#' Build workflow
#' 
#' @param graph if add graph coordinates or not, used for visualization on seven bridges platform.
#' @param x.width x scale width
#' @param y.width y scale width
#' @param x.start node x start point for a flow
#' @param y.start node y start point for a flow
#' @param canvas_zoom zoom factor
#' @param canvas_x canvas x 
#' @param canvas_y canvas y
#' @param batchInput batch input node id. note: Only one input per workflow can 
#' be configured as batch. Setting grouping criteria on this node will reset previous 
#' batch settings.
#' @param batchBy a list of type and crateria. For more information, please check manual for \code{batch}
#' @param ... extra arguments passed to SBGWorkflow
#'
#' @rdname Flow
#' @return a SBGWorkflow object.
#' @export SBGWorkflow
#' @exportClass SBGWorkflow
#' @aliases SBGWorkflow
#' @examples 
#' f1 = system.file("extdata/app", "flow_star.json", package = "sevenbridges")
#' f1 = convert_app(f1)
#' ## flow inputs
#' f1$input_type()
#' ## flow outouts
#' f1$output_type()
#' ## flow input id
#' f1$input_id()
#' ## linked input id
#' f1$linked_input_id()
#' ## flow output id
#' f1$output_id()
#' ## linked output id
#' f1$linked_output_id()
#' ## link_map
#' f1$link_map()
#' ## all step input id
#' f1$step_input_id()
#' ## all step input full id with type
#' f1$step_input_id(TRUE)
#' ## all step output id
#' f1$step_output_id()
#' ## all step output full id with type
#' f1$step_output_id(TRUE)
#' ## get inputs objects
#' f1$get_input("#clip3pNbases")
#' f1$get_input(c("#clip3pNbases", "#chimScoreMin"))
#' f1$get_input(c("#clip3pNbases", "#chimScoreMin", "#STAR.outFilterMismatchNoverLmax"))
#' ## get outputs objects
#' f1$get_output("#log_files")
#' f1$get_output(c("#log_files", "intermediate_genome"))
#' f1$get_output(c("#log_files", "intermediate_genome", "#STAR.unmapped_reads"))
#' f1$get_output("#log_files")
#' ## set flow input
#' f1$set_flow_input("#SBG_FASTQ_Quality_Detector.fastq")
#' f1$set_flow_output(c("#log_files", "intermediate_genome"))
#' ## get required node
#' f1$get_required()
#' ## set required node
#' f1$steps[[1]]$run$set_required("genomeChrBinNbits")
#' f1$get_required()
#' f1$steps[[1]]$run$set_required("genomeChrBinNbits", FALSE)
#' f1$get_required()
#' ## get Tool object from Flow by id and name
#' f1$list_tool()
#' ## return two
#' f1$get_tool("STAR")
#' ## return one
#' f1$get_tool("^STAR$")
#' ## get included input ports
#' f1$get_input_port()
#' ## set included input ports
#' f1$set_input_port(c("#STAR.alignSJDBoverhangMin", "chimScoreSeparation"))
#' f1$get_input_port()
#' f1$set_input_port(c("#STAR.alignSJDBoverhangMin", "chimScoreSeparation"), FALSE)
#' f1$get_input_port()
#' f1$get_input_node()
#' f1$get_output_node()
#' f1$get_input_exposed()
#' f1$step_input_id(TRUE)
#' f1$input_id()
#' f1$set_flow_input("#STAR.reads")
#' f1$input_id()
#' ## batch
#' f1$set_batch("sjdbGTFfile", c("metadata.sample_id", "metadata.library_id"))
#' f1$set_batch("sjdbGTFfile", type = "ITEM")
SBGWorkflow <- setRefClass("SBGWorkflow", contains = c("Workflow", "SBG"),
                           fields = list(
                               "sbg:update" = "characterORNULL",
                               "sbg:canvas_zoom" = "numericORNULL",
                               "sbg:canvas_y" =  "numericORNULL",
                               "sbg:canvas_x" = "numericORNULL", 
                               "sbg:batchInput" = "characterORNULL",
                               "sbg:batchBy" = "listORNULL"
                           ),
                           methods = list(
                               initialize = function(id = NULL,
                                                     label = NULL,
                                                     canvas_zoom = 1,
                                                     canvas_y = NULL,
                                                     canvas_x = NULL,
                                                     batchInput = NULL,
                                                     batchBy = NULL,
                                                     steps = list(),
                                                    
                                                     ...){
                                   
                                   stopifnot(!is.null(id))
                                   
                                   stopifnot(!is.null(label))
                                   
                                   args <- mget(names(formals()),
                                                sys.frame(sys.nframe()))
                                   
                                   nms <- c("canvas_x", "canvas_y", "canvas_zoom", "update")
                                   for(nm in nms){
                                       .self$field(paste0("sbg:", nm), args[[nm]])
                                   }
                                   steps <<- steps
                                  
                                   callSuper(id = id, label = label, ...)
                               },
                               
                               set_flow_output = function(oid = NULL, add = TRUE){
                             
                                   ## just add to output node
                                   if(!length(oid)){
                                       return(NULL)
                                   }
                                   ## get exist
                                   if(add){
                                       old.id <- output_id()
                                       oid <- unique(c(old.id, oid))
                                   }
                                   oi <- sapply(oid, .id_only)
                                   ol <- get_output(oid, TRUE)
                                   
                                   
                                   sol <- as(ol, "SBGWorkflowOutputParameterList")
                                   lst <- lapply(sol, function(ol){
                              
                                       .id <- ol$id 
                                       nm <- names(oi[oi == .id])
                                       ## should be full name
                                       ol$source <- set_box(nm)
                                       ol
                                   })
                                   sol <- do.call("SBGWorkflowOutputParameterList",
                                                  lst)
                                   for(i in 1:length(sol)){
                                       sol[[i]]$'sbg:includeInPorts' <- TRUE
                                   }
                                   
                                   outputs <<- sol
                                   
                                   addGraph(.self)
                               },
                               set_flow_input = function(iid = NULL, add = TRUE){
                                   ## validate the input is File or File...

                                   .id = step_input_id(TRUE)
                                   if(!all(iid %in% .id)){
                                       stop("input id doesn't exist")
                                   }
                                   
                                   
                                   
                                   if(add){
                                       oid = input_id()
                                       nid = iid[which(is.na(match(sapply(iid, .id_only), oid)))]
                                       .nnl = get_input(id = nid) 
                                       if(length(.nnl) == 1){
                                           .nnl = list(.nnl)
                                       }else{
                                           .nnl = as.list(.nnl)
                                       }
                                       
                                       inl = do.call("IPList", c(.nnl, as.list(inputs)))
                                       
                                   }else{
                                       nid = iid
                                       inl = get_input(id = nid, TRUE)
                                       
                                   }
                                   ## get connected from step list
                              
                                   sapply(nid, function(x){
                                       tool.id = get_tool_id_from_full(x)
                                       sins = get_step(id = tool.id)$inputs
                                       idx = which(x == sapply(sins, function(i) i$id))
                                       input.id = get_input_id_from_full(x)
                                       sins[[idx]]$source = set_box(input.id)
                                  })
                                   
                                   
                                   for(i in 1:length(inl)){
                                       ## all exposed
                                       inl[[i]]$'sbg:includeInPorts' <- TRUE
                                   }
                                   inputs <<- inl
                                   addGraph(.self)
                               },
                               linked_output_id = function(){
                                   'output id that linked to an input' 
                                   res = unlist(sapply(steps, function(s){
                                       if(length(s$inputs)){
                                           sapply(s$inputs, function(i){
                                               res <- i$source
                                               if(length(res)){
                                                   return(unname(i$source))
                                               }else{
                                                   return(NULL)
                                               }
                                              
                                               
                                           })
                                       }else{
                                           return(NULL)
                                       }
                                   }))
                                   res[is_full_name(res)]
                               },
                               linked_input_id = function(){
                                   'input id that linked to an output'
                                   res = unlist(sapply(steps, function(s){
                                       if(length(s$inputs)){
                                           sapply(s$inputs, function(i){
                                               res <- i$source
                                               if(length(res)){
                                                   return(unname(i$id))
                                               }else{
                                                   return(NULL)
                                               }
                                           })}else{
                                               return(NULL)
                                           }
                                   }))
                                   res[is_full_name(res)]
                               },
                               link_map = function(){
                                   ## input nodes
                                   
                                   ## linked
                                   ## output nodes
                                   
                               },
                               list_tool = function(){
                                   res = lapply(steps, function(x){
                                       data.frame(label = x$run$label,
                                                  sbgid = x$run$id,
                                                  id = x$id)
                                   })
                                   res = do.call(rbind, res)
                                   rownames(res) <- NULL
                                   res
                                   
                               },
                               get_tool = function(name = NULL, id = NULL){
                                   if(is.null(name) && is.null(id)){
                                       stop("please provide name or id")
                                   }
                                   dt = list_tool()
                                   if(!is.null(name)){
                                       idx = which(grepl(name, dt$label))
                                   }
                                   if(!is.null(id)){
                                       idx = which(id == dt$id)
                                   }
                                   if(length(idx) == 0){
                                       return(NULL)
                                   }else if(length(idx) == 1){
                                       return(steps[[idx]]$run)
                                   }else if(length(idx) >1){
                                       return(lapply(steps[idx], function(x) x$run))
                                   }
                                   
                               },
                               get_step = function(name = NULL, id = NULL){
                                   if(is.null(name) && is.null(id)){
                                       stop("please provide name or id")
                                   }
                                   dt = list_tool()
                                   if(!is.null(name)){
                                       idx = which(grepl(name, dt$label))
                                   }
                                   if(!is.null(id)){
                                       idx = which(id == dt$id)
                                   }
                                   if(length(idx) == 0){
                                       return(NULL)
                                   }else if(length(idx) == 1){
                                       return(steps[[idx]])
                                   }else if(length(idx) >1){
                                       return(steps[idx])
                                   }
                                   
                               },
                               get_input = function(ids, force = FALSE){
                                   'get input by pure input id from all steps'
                                
                                   ids = addIdNum(ids)
                                   idx = is_full_name(ids)
                                   res = NULL
                                   if(sum(idx)){
                                       ids.full = ids[idx]
                                       tool.name = get_tool_id_from_full(ids[idx])
                                       input.name = get_input_id_from_full(ids[idx])
                                       for(i in 1:length(tool.name)){
                                           res = c(res, get_tool(id = tool.name[i])$get_input(id = input.name[i]))
                                       }
                                   }
                                   
                                 
                                   
                                  if(sum(!idx)){
                                   for(.id in ids[!idx]){
                                       for(i in seq_len(length(steps))){
                                           r = steps[[i]]$run
                                           idx = which(.id == r$input_id())
                                           if(length(idx)){
                                               res = c(res, r$inputs[[idx]])
                                           }
                                        }}
                                  }
                                   if(force){
                                       if(length(res)){
                                           
                                           return(do.call("InputParameterList", res))
                                       }else{
                                           return(NULL)
                                       } 
                                   }else{
                                       if(length(res) > 1){
                                           
                                           return(do.call("InputParameterList", res))
                                       }else if(length(res) == 1){
                                           return(res[[1]] )
                                       }else{
                                           return(NULL)
                                       } 
                                   }
                                   
                               },
                               get_output = function(ids, force = FALSE){
                                   'get output by pure output id from all steps'
                                   res = NULL
                                   
                                   ids = addIdNum(ids)
                                   idx = is_full_name(ids)
                                   if(sum(idx)){
                                       ids.full = ids[idx]
                                       tool.name = get_tool_id_from_full(ids[idx])
                                       output.name = get_input_id_from_full(ids[idx])
                                       for(i in 1:length(tool.name)){
                                           res = c(res, get_tool(id = tool.name[i])$get_output(id = output.name[i]))
                                       }
                                   }
                                   if(sum(!idx)){
                                   for(.id in ids[!idx]){
                                       for(i in seq_len(length(steps))){
                                           
                                           
                                           r = steps[[i]]$run
                                          
                                           idx = which(.id == r$output_id())
                                           if(length(idx)){
                                               
                                               res = c(res, r$outputs[[idx]])
                                               
                                           }
                                           
                                       }}
                                   }
                                   if(force){
                                       if(length(res)){
                                           return(do.call("OutputParameterList", res))
                                       }else{
                                           return(NULL)
                                       }  
                                   }else{
                                       if(length(res) > 1){
                                           return(do.call("OutputParameterList", res))
                                       }else if(length(res) == 1){
                                           return(res[[1]])
                                       }else{
                                           return(NULL)
                                       }
                                   }
                                   
                               },
                               
                               input_type = function(){
                                   getInputType(toList())
                                   
                               },
                               output_type = function(){
                                   getOutputType(toList())
                               },
                          
                               input_id = function(){
                                   res = unname(sapply(inputs, function(i) i$id))
                                   if(length(res)){
                                       return(res)
                                   }else{
                                       return(NULL)
                                   }
                               },
                               output_id = function(){
                                   res = unname(sapply(outputs, function(o) o$id))
                                   if(length(res)){
                                       return(res)
                                   }else{
                                       return(NULL)
                                   }
                               },
                               step_input_id = function(full = FALSE){
                                   
                                   unlist(sapply(steps, function(x){
                                       x$run$input_id(full = full)
                                   }))
                               },
                               step_output_id = function(full = FALSE){
                                   unlist(sapply(steps, function(x){
                                       x$run$output_id(full = full)
                                   }))
                               },
                               get_required = function(){
                                   unlist(sapply(steps, function(x){
                                       x$run$get_required()
                                   }))
                               },
                               set_required = function(ids, required = TRUE){
                                   'require full input id (with tool id prefix)'
                                   tool.name = get_tool_id_from_full(ids)
                                   input.name = get_input_id_from_full(ids)
                                   
                                   tools = get_tool(tool.name)
                                   for(i in 1:length(tool.name)){
                                       tools[[i]]$set_required(input.name, required)
                                   }
                                   
                               },
                               get_input_port = function(){
                                 unlist(sapply(steps, function(x){
                                     x$run$get_input_port()
                                 }))
                               },
                               set_input_port = function(ids, include = TRUE){
                                   ids = add_sharp(ids)
                                   idx = is_full_name(ids)
                                   if(sum(idx)){
                                       ids.full = ids[idx]
                                       tool.name = get_tool_id_from_full(ids[idx])
                                       input.name = get_input_id_from_full(ids[idx])
                                       for(i in 1:length(tool.name)){
                                           get_tool(id = tool.name[i])$set_input_port(id = input.name[i], include = include)
                                       }
                                   }  
                                   if(sum(!idx)){
                                       for(.id in ids[!idx]){
                                           get_input(id = .id)$field('sbg:includeInPorts', include)
                                       }
                                   }
                                 
                               },
                               get_input_node = function(){
                                   'get input file nodes id'
                                   it = input_type()
                                   add_sharp(names(it[it %in% c("File", "File...")]))
                               },
                               get_output_node = function(){
                                   'get output file nodes id'
                                   it = output_type()
                                   add_sharp(names(it[it %in% c("File", "File...")]))
                               },
                           
                               get_input_exposed = function(){
                                   'exposed input id other than file'
                                   it = input_type()
                                   add_sharp(names(it[!it %in% c("File", "File...")]))
                               },
                               set_batch = function(input = NULL,
                                                criteria = NULL,
                                                type = c("ITEM", "CRITERIA")){
                                   
                                   lst = batch(input = input, criteria = criteria, type = type)
                                   lst$batch_by$type = tolower(lst$batch_by$type)
                                   ## flow level
                                   .self$field("sbg:batchInput", addIdNum(lst$batch_input))
                                   .self$field("sbg:batchBy", lst$batch_by)
                                   
                                   ## step level 
                                   
                                   
                                   idx = which(sapply(.self$inputs, function(x) x$id) == addIdNum(input))
                                   .self$inputs[[idx]]$field("batchType", tail(criteria, 1))
                                   .self
                                   
                               },
                               run = function(run_inputs = list(), engine = c("bunny", "rabix", "cwlrun")){
                                   'run this tool with inputs locally. engine supported: bunny, rabix, cwlrun. 
                                   inputs accept list or json'
                                   engine = match.arg(engine)
                                   run_inputs = lapply(run_inputs, asTaskInput)
                                   switch(engine, 
                                          bunny = {
                                              test_tool_bunny(.self, run_inputs)
                                          },
                                          cwlrun = {
                                              test_tool_cwlrun(.self, run_inputs)
                                          },
                                          rabix = {
                                              test_tool_rabix(.self, run_inputs)
                                          })
                               }
      ))

#' @rdname Flow
#' 
#' 
#' @export Flow
#' @aliases Flow
Flow <- function(..., graph = TRUE,
                 x.width = 1000, 
                 y.width = 400,
                 x.start = 100,
                 y.start = 200,
                 canvas_zoom = 1,
                 canvas_x = 40,
                 canvas_y = 130){
    obj <- SBGWorkflow(...)
    if(graph){
        obj <- addGraph(obj, x.width = x.width,
                        y.width = y.width,
                        x.start = x.start,
                        y.start = y.start,
                        canvas_zoom = canvas_zoom,
                        canvas_x = canvas_x,
                        canvas_y = canvas_y)    
    }
    obj
}

#' @rdname Flow
#' @export
#' @param e1 either Tool App or Workflow object
#' @param e2 either Tool App or Workflow object
#' @docType methods
#' @aliases "+",Tool,Tool-method
setMethod("+", c("Tool", "Tool"), function(e1, e2){
    ## find the x1
    o <- getOutputType(e1)
    i <- getInputType(e2)
    idx <- match(o, i)
    
    if(!all(is.na(idx))){
        if(sum(!is.na(idx)) == 1){
            ## return a Workflow
            .out.id <- getOutputId(e1)[[which(!is.na(idx))]]
            .in.id <- getInputId(e2)[[idx[!is.na(idx)]]]
            .out1 <- getOutputId(e1)
            .out2 <- getOutputId(e2)
            o1 <- do.call("WorkflowStepOutputList",
                          lapply(.out1, function(oid) WorkflowStepOutput(id = oid)))
            o2 <- do.call("WorkflowStepOutputList",
                          lapply(.out2, function(oid) WorkflowStepOutput(id = oid)))
            
            ## insert int id
            steplst <- SBGStepList(SBGStep(id = getId(e1), 
                                           run = e1,
                                           outputs = o1),
                                   SBGStep(id = getId(e2),
                                           run = e2,
                                           outputs = o2,
                                           inputs = WorkflowStepInputList(
                                               WorkflowStepInput(id = .in.id,
                                                                 source = c(.out.id))
                                           )))
            ## make rest inports
            return(steplst)
        }else{
            stop("multiple matching found, no sure which one to connect")
        }
    }else{
        stop("no input match ouput types")
    }
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",WorkflowStepList,Tool-method
setMethod("+", c("WorkflowStepList", "Tool"), function(e1, e2){
    t.last <- e1[[length(e1)]]$run
    t.new <- (t.last + e2)[2]
    do.call(SBGStepList, c(as.list(e1), as.list(t.new)))
    
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",WorkflowStepList,WorkflowStepList-method
setMethod("+", c("WorkflowStepList", "WorkflowStepList"), function(e1, e2){
    do.call("WorkflowStepList", c(as.list(e1), as.list(e2)))
})


#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",App,App-method
setMethod("+", c("App", "App"), function(e1, e2){
    convert_app(e1) + convert_app(e2)
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "+",WorkflowStepList,App-method
setMethod("+", c("WorkflowStepList", "App"), function(e1, e2){
    t.last <- e1[[length(e1)]]$run
    t.new <- (t.last + convert_app(e2))[2]
    do.call(SBGStepList, c(as.list(e1), as.list(t.new)))
})




#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>>%"
setGeneric("%>>%", function(e1, e2) standardGeneric("%>>%"))

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>>%",Tool,Tool-method
setMethod("%>>%", c("Tool", "Tool"), function(e1, e2){
    .id <- paste(parseLabel(e1$label), parseLabel(e2$label), sep = "_")
    .label <- paste(parseLabel(e1$label), parseLabel(e2$label))
    lst <- e1 + e2
    Flow(id = .id, label = .label, steps = lst)
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>>%",Workflow,Tool-method
setMethod("%>>%", c("Workflow", "Tool"), function(e1, e2){
    ## fix
    slist <- e1$steps + e2
    new.flow <- Flow(id = e1$id, label = e1$label, steps = slist)
    e1$steps <- slist
    e1$outputs <- new.flow$outputs
    e1
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>>%",Workflow,Workflow-method
setMethod("%>>%", c("Workflow", "Workflow"), function(e1, e2){
    e1$steps <- e1$steps + e2$steps
    e1
})

#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>>%",App,App-method
setMethod("%>>%", c("App", "App"), function(e1, e2){
    convert_app(e1) %>>% convert_app(e2)
})


#' @rdname Flow
#' @export
#' @docType methods
#' @aliases "%>>%",Workflow,App-method
setMethod("%>>%", c("Workflow", "App"), function(e1, e2){
    slist <- e1$steps + convert_app(e2)
    new.flow <- Flow(id = e1$id, label = e1$label, steps = slist)
    ## udpate step list
    e1$steps <- slist
    ## update workflow
    e1$outputs <- new.flow$outputs
    e1
})

## assign coordinates
## add output port

## coersion
setAs("CommandOutputParameter", "WorkflowOutputParameter", 
      function(from){
          WorkflowOutputParameter(
              type = from$type, 
              label = from$label,
              description = from$description,
              streamable = from$streamable,
              default = from$default,
              id = from$id
          )
      })

setAs("CommandOutputParameter", "SBGWorkflowOutputParameter", 
      function(from){
          SBGWorkflowOutputParameter(
              type = from$type, 
              label = from$label,
              description = from$description,
              streamable = from$streamable,
              default = from$default,
              id = from$id
          )
      })

setAs("OutputParameterList", "WorkflowOutputParameterList", 
      function(from){
          lst <- lapply(from, function(x) as(x, "WorkflowOutputParameter"))
          do.call("WorkflowOutputParameterList", lst)
      })

setAs("OutputParameterList", "SBGWorkflowOutputParameterList", 
      function(from){
          lst <- lapply(from, function(x) as(x, "SBGWorkflowOutputParameter"))
          do.call("SBGWorkflowOutputParameterList", lst)
      })


getOutputById <- function(sl, id){
    lst <- lapply(sl, function(s){
        ol <- s$run$outputs
        
        lapply(ol, function(o){
            if(o$id %in% id){
                return(o)
            }else{
                return(NULL)
            }
        })
    })
    
    lst <- do.call(c, lst)
    lst <- lst[!sapply(lst, is.null)]
    do.call("OutputParameterList", lst)
}


## asign step list coordinates
setGeneric("addGraph", function(obj, ...) standardGeneric("addGraph"))


setMethod("addGraph", "SBGStepList", function(obj, 
                                              x.width = 1000, 
                                              y.width = 400,
                                              x.start = 100,
                                              y.start = 200){
    N <- length(obj)
    if(N){
        x.step <- x.width/(N+1)
        ## x from 100
        ## y from 200
        for(i in 1:N){
            obj[[i]]$field("sbg:x", x.start + x.step * (i - 1))
            obj[[i]]$field("sbg:y", y.start)
        }}
    obj
})

setMethod("addGraph", "SBGWorkflow", function(obj, 
                                              x.width = 1000, 
                                              y.width = 400,
                                              x.start = 100,
                                              y.start = 200,
                                              canvas_zoom = 1,
                                              canvas_x = 40,
                                              canvas_y = 130){
    
    obj$steps <- addGraph(obj$steps)
    slst <- obj$steps
    x.step <- x.width/(length(slst)+1)
    ## outputs
    os <- obj$outputs
    N <- length(os)
    if(N){
        y.step <- y.width/(N + 1)
        for(i in 1:N){
            x = obj$outputs[[i]]$field("sbg:x")
            y = obj$outputs[[i]]$field("sbg:y")
            if(is.null(x) || is.null(y)){
                obj$outputs[[i]]$field("sbg:x", x.width - x.step)
                obj$outputs[[i]]$field("sbg:y", y.start + y.step * (i-1))  
            }
            
        }
    }
    ## inputs
    ins <- obj$inputs
    N <- length(ins)
    if(N){
        y.step <- y.width/(N + 1)
        for(i in 1:N){
            x = obj$inputs[[i]]$field("sbg:x")
            y = obj$inputs[[i]]$field("sbg:y")
            if(is.null(x) || is.null(y)){
            obj$inputs[[i]]$field("sbg:x", 0 + x.step)
            obj$inputs[[i]]$field("sbg:y", y.start + y.step * (i-1))
            }
        }
    }
    if(is.null( obj$field("sbg:canvas_zoom"))){
        obj$field("sbg:canvas_zoom", canvas_zoom)
    }
    if(is.null(obj$field("sbg:canvas_x"))){
        obj$field("sbg:canvas_x", canvas_x)
    }
    if(is.null(obj$field("sbg:canvas_y"))){
        obj$field("sbg:canvas_y", canvas_y)
    }
    
    
    
    obj
})


## link Generic
setClassUnion("ToolORWorkflow", c("Tool", "Workflow"))

#' link two nodes to form a new Workflow
#' 
#' link two nodes to form a new Workflow
#' 
#' Flexible enought to allow users to connect two objects by ids
#' 
#' @rdname link
#' @export
#' @docType methods
#' @aliases link
#' @param from either Tool App or Workflow object
#' @param to either Tool App or Workflow object
#' @param ... more auguments
#' @return A Workflow object
setGeneric("link", function(from, to, ...) standardGeneric("link"))

#' @rdname link
#' @param id1 id to be connected from the ouput of the first node
#' @param id2 id id to be connected from the input of the second first node
#' @param flow_id workflow id, if ignored, going to create one by joning tool id.
#' @param flow_label workflow label, if ignored, going to create one by joning tool labels.
#' @param flow_input full flow input id, e.g. "#SBG_Unpack_FASTQs.input_archive_file"
#' @param flow_output full flow output id, e.g. "#STAR.log_files"
#' @export
#' @docType methods
#' @aliases link,Tool,Tool-method
#' @examples 
#' t1 = system.file("extdata/app", "tool_unpack_fastq.json", package = "sevenbridges")
#' t2 = system.file("extdata/app", "tool_star.json", package = "sevenbridges")
#' t1 = convert_app(t1)
#' t2 = convert_app(t2)
#' ## check possible link
#' link_what(t1, t2)
#' ## link
#' f = link(t1, t2, "output_fastq_files", "reads")
#' ## link
#' f = link(t1, t2, "output_fastq_files", "reads",
#'          flow_input = "#SBG_Unpack_FASTQs.input_archive_file",
#'          flow_output = "#STAR.log_files")
setMethod("link", c("Tool", "Tool"), function(from, to, id1, id2, 
                                              flow_id = NULL, 
                                              flow_label = NULL,
                                              flow_input = NULL, 
                                              flow_output = NULL){
    if(is.null(flow_id)){
        flow_id <- paste(parseLabel(from$label), parseLabel(to$label), sep = "_")
    }
    if(is.null(flow_label)){
        flow_label <- paste(parseLabel(from$label), parseLabel(to$label))
    }
   
    .out.id <- id1
    .in.id <- id2
    
    
    .out1 <- unname(from$output_id(TRUE))
    .out2 <- unname(to$output_id(TRUE))
    o1 <- do.call("WorkflowStepOutputList",
                  lapply(.out1, function(oid) WorkflowStepOutput(id = oid)))
    o2 <- do.call("WorkflowStepOutputList",
                  lapply(.out2, function(oid) WorkflowStepOutput(id = oid)))
    
    .in1 <- from$input_id(TRUE)
    .in2 <- to$input_id(TRUE)
   
    in.lst1 <- do.call(WorkflowStepInputList, lapply(1:length(.in1), function(i){
        
        WorkflowStepInput(id = unname(.in1[i]))
    }))
    
    in.lst2 <- do.call(WorkflowStepInputList, lapply(1:length(.in2), function(i){
       
        idx = which(get_input_id_from_full(.in2[i]) == add_sharp(id2))
        if(length(idx)){
           
            if(is_full_name(id1[i])){
                .s = id1[i]
            }else{
                .s = paste(get_tool_id_from_full(.in1[1]), id1[i], sep = ".")
            }
            
            WorkflowStepInput(id = unname(.in2[i]),
                              source = set_box(unname(add_sharp(.s))))
        }else{
            WorkflowStepInput(id = unname(.in2[i]))
        }
        
    }))
    
    steplst <- SBGStepList(SBGStep(id = getId(from), 
                                   run = from,
                                   outputs = o1,
                                   inputs = in.lst1),
                           SBGStep(id = getId(to),
                                   run = to,
                                   outputs = o2,
                                   inputs = in.lst2))
    res = Flow(id = flow_id, label = flow_label, steps = steplst)
    
    if(is.null(flow_input)){
        ## auto parse input file
       
        i.f = c(from$input_id(TRUE), to$input_id(TRUE))
        flow_input.all = addIdNum(i.f[names(i.f) %in% c("File", "File...")])
        flow_input.linked = res$linked_input_id()
        flow_input = setdiff(flow_input.all, flow_input.linked)
        message("flow_input: ", paste0(flow_input ,collapse = " / "))
    }else{
        # need to get required file.. input 
        i.f = c(from$input_id(TRUE, TRUE), to$input_id(TRUE, TRUE))
        flow_input.all = addIdNum(i.f[names(i.f) %in% c("File", "File...")])
        flow_input.linked = res$linked_input_id()
        flow_input = unique(c(flow_input, setdiff(flow_input.all, flow_input.linked)))
        message("flow_input: ", paste0(flow_input ,collapse = " / "))
    }
    
    res$set_flow_input(flow_input) 
    
    if(is.null(flow_output)){
        ## auto parse output file
        o.f = c(from$output_id(TRUE), to$output_id(TRUE))
        flow_output.all = addIdNum(o.f[names(o.f) %in% c("File", "File...")])
        flow_output.linked = res$linked_output_id()
        flow_output = setdiff(flow_output.all, flow_output.linked)
        
    }
    
    message("flow_output: ", paste0(flow_output ,collapse = " / "))
    res$set_flow_output(flow_output)
   
    
    
    
    ## make rest inports
    res
})

#' @rdname link
#' @export
#' @docType methods
#' @aliases link,Tool,Workflow-method
setMethod("link", c("Tool", "Workflow"), function(from, to, id1, id2){
    ## id2 has to be full
    stop("not implemented yet")
})

#' @rdname link
#' @export
#' @docType methods
#' @aliases link,Workflow,Tool-method
setMethod("link", c("Workflow", "Tool"), function(from, to, id1, id2){
    stop("not implemented yet")
})

#' @rdname link
#' @export
#' @docType methods
#' @aliases link,Workflow,Workflow-method
setMethod("link", c("Workflow", "Workflow"), function(from, to, id1, id2){
    stop("not implemented yet")
})

#' @rdname link
#' @export
#' @docType methods
#' @aliases link,App,ToolORWorkflow-method
setMethod("link", c("App", "ToolORWorkflow"), function(from, to, id1, id2){
    stop("not implemented yet")
})

#' @rdname link
#' @export
#' @docType methods
#' @aliases link,ToolORWorkflow,App-method
setMethod("link", c("ToolORWorkflow", "App"), function(from, to, id1, id2){
    stop("not implemented yet")
})

#' shows potential link methods by providing grouped inputs/ouputs
#' 
#' @rdname link
#' @export link_what
#' @aliases link_what
#' @examples 
#' tool.in = system.file("extdata/app", "tool_unpack_fastq.json", package = "sevenbridges")
#' flow.in = system.file("extdata/app", "flow_star.json", package = "sevenbridges")
#' t1 = convert_app(tool.in)
#' f2 = convert_app(flow.in)
#' lw = link_what(t1, f2)
#' lw
link_what = function(from, to){
    from.in = from$output_type()
    to.in = to$input_type()
    .t = unique(intersect(from.in, to.in))
    res = lapply(.t, function(i){
        list(from = names(from.in)[from.in == i],
             to = names(to.in)[to.in == i])
    })
    names(res) = .t
    for(i in .t){
        message("Type: ", i)
        message("  From:")
        message("  ", paste(res[[i]]$from, collapse = "  "))
        message("  To:")
        message("  ", paste(res[[i]]$to, collapse = "  "))
        ## suggest link
        message("  Suggested:")
        .s = data.frame()
        for(f in res[[i]]$from){
            idx = grepl(f, res[[i]]$to)
            if(sum(idx)){
                for(id in idx){
                    .s = rbind(data.frame(from = f, to = res[[i]]$to[id]))
                    message("  ", f, " --> ", " ", res[[i]]$to[id])
                }
            }
        }
        for(.t in res[[i]]$to){
            idx = grepl(.t, res[[i]]$from)
            if(sum(idx)){
                for(id in idx){
                    .s = rbind(data.frame(from = res[[i]]$from[id], to = .t))
                    message("  ", res[[i]]$from[id], " --> ", " ", .t)
                }
            }
        }
        if(length(.s)){
            res$suggest = .s
        }
        
    }
    if(!length(res)){
        message("no type matching")
    }
    invisible(res)
}


## utils
.id_only = function(x){
    
    if(grepl("[.]", x)){
        res <- unlist(strsplit(x, "[.]"))
        paste0("#", res[length(res)])
    }else{
        unname(addIdNum(x))
    }
}


