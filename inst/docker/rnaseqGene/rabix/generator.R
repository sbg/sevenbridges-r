library(sevenbridges)

rbx <- Tool(id = "rnaseqGene", 
            label = "rnaseqgene",
            description = "A RNA-seq Differiencial Expression Flow and Report",
            hints = requirements(docker(pull = "tengfei/rnaseqgene"), cpu(1), mem(2000)), 
            baseCommand = "performDE.R", 
            inputs = list(
                input(
                    id = "bamfiles", label = "bam files",
                    description = "a list of bam files",
                    type = "File...",  ## or type = ItemArray("File")
                    prefix = "--bamfiles",
                    itemSeparator = ","
                ), 
                input(
                    id = "design", label = "design matrix",
                    type = "File",
                    prefix = "--design"
                ),
                input(
                    id = "gtffile", label =  "gene feature files",
                    type = "File",
                    prefix = "--gtffile"
                ),
                input(
                    id = "format", label =  "report foramt html or pdf",
                    type = enum("format", c("pdf", "html")),
                    prefix = "--format"
                )
            ),
            outputs = list(
                output(id = "report", label = "report", 
                       description = "A reproducible report created by Rmarkdown",
                       glob = Expression(engine = "#cwl-js-engine",
                                         script = "x = $job[['inputs']][['format']];
                                                  if(x == 'undefined' || x == null){
                                                   x = 'html';
                                                    };
                                                  'rnaseqGene.' +  x")),
                output(id = "heatmap", label = "heatmap", 
                       description = "A heatmap plot to show the Euclidean distance between samples",
                       glob = "heatmap.pdf"),
                output(id = "count", label = "count", 
                       description = "Reads counts matrix",
                       glob = "count.csv"),
                output(id = "de", label = "Differential expression table", 
                       description = "Differential expression table",
                       glob = "de.csv")
                ))

fl <- "inst/docker/rnaseqGene/rabix/rnaseqGene.json"
write(rbx$toJSON(pretty = TRUE), fl)

