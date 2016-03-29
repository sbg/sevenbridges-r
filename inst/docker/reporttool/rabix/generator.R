library(sevenbridges)
rbx <- Tool(id = "reporttool",
            label = "reporttool",
            description = "Reporiting tools support you pass shiny app and knitr Rmakrdown template",
            hints = requirements(docker(pull = "tengfei/reporttool")),
            baseCommand = "report.R",
            inputs = list(
                input(id = "shinyTemplate",
                      description = "Shinay app template as zipped(.zip) or tar(tar.gz) file.",
                      type = ItemArray("File"),
                      prefix = "--shinyTemplate",
                      itemSeparator = ",",                      
                      cmdInclude = TRUE),
                input(id = "knitrTemplate",
                      description = "Rmarkdown file template will be rendered by knitr",
                      type = ItemArray("File"),
                      prefix = "--knitrTemplate",
                      itemSeparator = ",",
                      cmdInclude = TRUE                      
                      ),
                input(id = "data",
                      description = "Files to be included in data folder of app",
                      type = "File...", # the same as ItemArray("File")
                      prefix = "--data",
                      itemSeparator = ",",                     
                      cmdInclude = TRUE                      
                      ),
                input(id = "www",
                      description = "Files to be included in www folder of app",
                      type = ItemArray("File"),
                      prefix = "--www",
                      itemSeparator = ",",                   
                      cmdInclude = TRUE                      
                      ),
                input(id = "src",
                      description = "Files to be included in src folder of app",
                      type = ItemArray("File"),
                      prefix = "--src",
                      cmdInclude = TRUE                      
                      ),
                input(id = "appFiles",
                      description = "Files to be included in root of app folder",
                      type = ItemArray("File"),
                      prefix = "--appFiles",
                      itemSeparator = ",",                    
                      cmdInclude = TRUE                      
                      ),
                input(id = "name",
                      description = "Name of account to save or remove, check shinyapps::setAccountInfo",
                      type = "string",
                      prefix = "--name",
                      cmdInclude = TRUE                      
                      ),
                input(id = "token",
                      description = "User token for the account, check shinyapps::setAccountInfo",
                      type = "string",
                      prefix = "--token",
                      cmdInclude = TRUE                      
                      ),
                input(id = "secret",
                      description = "User secret for the account, check shinyapps::setAccountInfo",
                      type = "string",
                      prefix = "--secret",
                      cmdInclude = TRUE                      
                      ),
                input(id = "contentCategory",
                      description = "Optional; the kind of content being deployed (e.g. 'plot', 'document', or 'application').",
                      type = "string",
                      prefix = "--contentCategory",
                      cmdInclude = TRUE                      
                      ),
                input(id = "account",
                      description = "Account to deploy application to. This parameter is only required for the initial deployment of an application when there are multiple accounts configured on the system (see accounts).",
                      type = "string",
                      prefix = "--account",
                      cmdInclude = TRUE                      
                      ),
                input(id = "server",
                      description = "Server name. Required only if you use the same account name on multiple servers.",
                      type = "string",
                      prefix = "--server",
                      cmdInclude = TRUE                      
                      ),
                input(id = "quiet",
                      description = "Request that no status information be printed to the console during the deployment.",
                      type = "boolean",
                      prefix = "--quiet",
                      cmdInclude = TRUE                      
                      ),
                input(id = "engine",
                      description = "packrat or liftr (docker in docker) or NA [default: packrat]",
                      type = enum("engine", c("packrat", "liftr")),
                      prefix = "--engine",
                      cmdInclude = TRUE
                      )),
            outputs = list(
                output(id = "shinyapp",
                       description = "compressed shiny app folder",
                       type = "file",
                       glob = "*.tar.gz"),
                output(id = "knitrreport",
                       description = "report rendered from knitr template",
                       type = "file",
                       glob = "{*.pdf, *.html}")
            ))



fl <- "inst/docker/reporttool/rabix/reporttool.json"
write(rbx$toJSON(pretty = TRUE), fl)
