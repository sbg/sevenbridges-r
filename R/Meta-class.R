## TODO: set single ENUM automatically

## Meta shema v2
## I use all lower case for field
## courtercy to SBG team, the python code
uuid_regex = c('^$|[0-9a-f]{8}-',
               '[0-9a-f]{4}-',
               '[1-5][0-9a-f]{3}-',
               '[89ab][0-9a-f]{3}-',
               '[0-9a-f]{12}')

max_length = function(n) {
    return(paste0("^.{0,", as.character(n),"}$"))
}

max_chars = 128

regErrFun = function(x, max = max_chars) {
    sprintf(paste(x,"can contain maximum %d characters"), max)
}

regErrFun("asdf")

metadata_groups_order <- list(
    File = c("experimental_strategy", "library_id", "platform",
             "platform_unit_id", "file_segment_number", "quality_scale",
             "paired_end", "data_format", "file_extension",
             "reference_genome", "data_type", "data_subtype",
             "analysis_uuid", "gdc_file_uuid", "access_level"),
    General = "investigation",
    Case = c("case_id", "case_uuid"),
    Case_Demographic = c("gender", "race", "ethnicity"),
    Case_Diagnosis = c("primary_site", "disease_type", "age_at_diagnosis"),
    Case_Status = "vital_status",
    Case_Prognosis = c("days_to_death"),
    Sample = c("sample_id", "sample_uuid", "sample_type"),
    Aliquot = c("aliquot_id", "aliquot_uuid")
)

find_meta_group <- function(meta) {
    res <- sapply(metadata_groups_order, function(x) {
        meta %in% x
    })
    names(which(res))
}

key_order = c("experimental_strategy", "library_id", "platform",
              "platform_unit_id", "file_segment_number", "quality_scale",
              "paired_end", "data_format", "file_extension", "reference_genome",
              "data_type", "data_subtype", "analysis_uuid", "gdc_file_uuid",
              "access_level", "investigation", "case_id", "case_uuid", "gender",
              "race", "ethnicity", "primary_site", "disease_type",
              "age_at_diagnosis", "vital_status", "days_to_death", "sample_id",
              "sample_uuid", "sample_type", "aliquot_id", "aliquot_uuid")

Meta <- setRefClass("Meta",

                    fields = list(
                        data             = "ANY",
                        name             = "characterORNULL",
                        description      = "characterORNULL",
                        category         = "characterORNULL",
                        locked           = "logicalORNULL",
                        type             = "characterORNULL",
                        suggested_values = "characterORNULL",
                        regex            = "characterORNULL",
                        regexErrMsg      = "characterORNULL"),

                    methods = list(
                        initialize = function(
                            data             = NA_character_,
                            name             = NULL,
                            description      = NULL,
                            category         = NULL,
                            locked           = FALSE,
                            type             = NULL,
                            suggested_values = NULL,
                            regex            = NULL,
                            regexErrMsg      = NULL) {

                            data <<- transformData(data)

                            # FIXME: validation
                            if(!is.null(type)) stopifnot(is(data, type))

                            name             <<- name
                            description      <<- description
                            category         <<- category
                            locked           <<- locked
                            type             <<- type
                            suggested_values <<- suggested_values
                            regex            <<- regex
                            regexErrMsg      <<- regexErrMsg

                        },

                        transformData = function(x) {
                            x
                        },
                        # asList = function(){
                        #
                        # },
                        show = function(short = FALSE, full = TRUE) {
                            if (short) {
                                .nms <- "data"
                            } else {
                                .nms <- c("data", "name", "description",
                                          "category",
                                          "locked", "type", "suggested_values",
                                          "regex", "regexErrMsg")
                            }
                            if (is.null(name)) {
                                x <- "== Meta =="
                            } else {
                                x <- paste("--", name, "--")
                            }
                            .showFields(.self, x,  .nms, full = full)
                        }
                    ))

setMetaClass <- function(className     = NULL,
                         type          = "ANY",
                         transformData = NULL,
                         contains      = NULL, ...) {

    stopifnot(!is.null(className))

    cls <- setRefClass(className, contains = c(contains, "Meta"),
                       fields = list(data = type))

    if (!is.null(transformData)) cls$methods(transformData = transformData)

    # constructor
    x <- list(...)
    category <- find_meta_group(className)
    .gen <- function(x, category) {
        function(data = NA_character_, ...) {
            lst <- .update_list(x, c(list(data = data,
                                          category = category), list(...)))
            do.call(cls,  lst)
        }
    }
    .gen(x, category)
}

file_extension <- setMetaClass(
    "file_extension",
    type = "characterORNULL",
    name = "File extension",
    description = "Values computationally indicated by the file extension \
    (suffix), which determines the layout for encoding file's data.",
    locked = TRUE)

platform <- setMetaClass(
    "platform",
    type = "characterORNULL",
    name = "Platform",
    description = "The version (manufacturer, model, etc.) of the \
    technology that was used sequencing or assaying. \
    See NCI Thesaurus Code: C45378.",
    locked = FALSE,
    suggested_values = c(
        "Affymetrix SNP Array 6.0",
        "Illumina HiSeq",
        "Illumina Human Methylation 450",
        "Illumina GA",
        "MDA_RPPA_Core",
        "BCR Record",
        "Hospital Record",
        "Illumina Human Methylation 27",
        "ABI capillary sequencer",
        "AgilentG4502A_07_3",
        "HG-CGH-244A",
        "HG-CGH-415K_G4124A",
        "CGH-1x1M_G4447A",
        "Illumina MiSeq",
        "HT_HG-U133A",
        "Illumina Human 1M Duo",
        "H-miRNA_8x15Kv2",
        "Illumina HumanHap550",
        "H-miRNA_8x15K",
        "AgilentG4502A_07_2",
        "HuEx-1_0-st-v2",
        "ABI SOLiD",
        "Complete Genomics",
        "HG-U133_Plus_2",
        "Illumina DNA Methylation OMA003 CPI",
        "Illumina DNA Methylation OMA002 CPI",
        "AgilentG4502A_07_1",
        "Ion Torrent PGM",
        "Affymetrix U133 Plus 2",
        "LS 454",
        "HiSeq X Ten",
        "Mixed platforms",
        "Illumina",
        "Helicos",
        "PacBio"),
    regex = max_length(max_chars),
    regexErrMsg =
        sprintf("The Platform can contain maximum %d characters", max_chars))

sample_id <- setMetaClass(
    "sample_id",
    type = "characterORNULL",
    name =  "Sample ID",
    description = "A human readable identifier for a sample or specimen, \
    which could contain some metadata information. A \
    sample or specimen is material taken from a \
    biological entity for testing, diagnosis, \
    ropagation, treatment, or research purposes, \
    including but not limited to tissues, body fluids, \
    cells, organs, embryos, body excretory products, etc.\
    See NCI Thesaurus Code: C19157.",
    locked = FALSE,
    regex = max_length(max_chars),
    regexErrMsg =
        sprintf("Sample ID can contain maximum %d characters", max_chars))

library_id <- setMetaClass(
    "library_id",
    type = "characterORNULL",
    name = "Library ID",
    description = "An identifier for the sequencing library preparation.",
    locked = FALSE,
    regex = max_length(max_chars),
    regexErrMsg =
        sprintf("Library ID can contain maximum %d characters", max_chars))

platform_unit_id <- setMetaClass(
    "platform_unit_id",
    type = "characterORNULL",
    name = "Platform unit ID",
    description = "An identifier for lanes (Illumina), or for slides \
    (SOLiD) in the case that a library was split and \
    ran over multiple lanes on the flow cell or slides. \
    The platform unit ID refers to the lane ID or the \
    slide ID",
    locked = FALSE,
    regex = max_length(max_chars),
    regexErrMsg =
        sprintf("Platform unit ID can contain maximum %d characters", max_chars))

paired_end_enum <- setSingleEnum("PairedEnd", c(NA, "1", "2"))

paired_end <- setMetaClass(
    "paired_end",
    type = "PairedEndSingleEnum",
    transformData = paired_end_enum,
    name = "Paired-end",
    suggested_values = c(NA, "1", "2"),
    description = "For paired-end sequencing, this value determines the \
    end of the fragment sequenced. For single-end \
    sequencing no value is needed.")

file_segment_number <- setMetaClass(
    "file_segment_number",
    type = "integer",
    name = "File segment number",
    description = "If the sequencing reads for a single library, \
    sample and lane are divided into multiple (smaller) \
    files, the File segment number is used to enumerate \
    these. Otherwise, this field can be left blank.",
    locked = FALSE,
    regex = "(^$)|(^[-+]?[0-9]*?[0-9]*$)",
    regexErrMsg = "The chunk number must be integer.")

quality_scale_enum <- setSingleEnum(
    "QualityScale",
    c(NA, "sanger", "illumina13", "illumina15", "illumina18", "solexa"))

quality_scale <- setMetaClass(
    "quality_scale",
    type = "QualityScaleSingleEnum",
    transformData = quality_scale_enum,
    name = "Quality scale",
    description =  "For raw reads, this value denotes the sequencing\"
    technology and quality format. For BAM and SAM\
    files, this value should always be 'Sanger'.",
    suggested_values = c(NA, "sanger", "illumina13", "illumina15", "illumina18", "solexa"),
    locked = FALSE)

investigation <- setMetaClass(
    "investigation",
    type = "characterORNULL",
    name = "Investigation",
    description = "A value denoting the project or study that generated\
    the data. See NCI Thesaurus Code: C41198.",
    locked = FALSE,
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Investigation"))

case_uuid <- setMetaClass(
    "case_uuid",
    type = "characterORNULL",
    name =  "Case UUID",
    description =  "A unique identifier for the subject who has taken\
    part in the investigation, such as a Universally\
    Unique Identifier (UUID). See NCI Thesaurus Code:\
    C15362.",
    locked =  TRUE,
    regex = uuid_regex,
    regexErrMsg = "Case UUID has to be valid UUID.")

case_id <- setMetaClass(
    "case_id",
    type = "characterORNULL",
    name =  "Case ID",
    description = "An identifier, such as a number or a string that may\
    contain metadata information, for a subject who has\
    taken part in the investigation or study. See NCI\
    Thesaurus Code: C54269.",
    locked = FALSE,
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Case ID"))

primary_site <- setMetaClass(
    "primary_site",
    type = "characterORNULL",
    name = "Primary site",
    description = "The anatomical site where the primary tumor is\
    located in the organism. See NCI Thesaurus Code:\
    C43761.",
    locked = FALSE,
    suggested_values = c(
        "Adrenal Gland",
        "Bile Duct",
        "Bladder",
        "Blood",
        "Brain",
        "Breast",
        "Cervix",
        "Colorectal",
        "Esophagus",
        "Eye",
        "Head And Neck",
        "Liver",
        "Lung",
        "Lymph Nodes",
        "Kidney",
        "Mesenchymal",
        "Mesothelium",
        "Nervous System",
        "Ovary",
        "Pancreas",
        "Prostate",
        "Skin",
        "Stomach",
        "Uterus",
        "Testis",
        "Thymus",
        "Thyroid"),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Primary site"))

disease_type <- setMetaClass(
    "disease_type",
    type = "characterORNULL",
    name =  "Disease type",
    description =  "The type of the disease or condition studied. \
    See NCI Thesaurus Code: C2991.",
    locked = FALSE,
    suggested_values = c(
        "Acute Myeloid Leukemia",
        "Adrenocortical Carcinoma",
        "Bladder Urothelial Carcinoma",
        "Brain Lower Grade Glioma",
        "Breast Invasive Carcinoma",
        "Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma",
        "Cholangiocarcinoma",
        "Chronic Myelogenous Leukemia",
        "Colon Adenocarcinoma",
        "Esophageal Carcinoma",
        "Glioblastoma Multiforme",
        "Head and Neck Squamous Cell Carcinoma",
        "Kidney Chromophobe",
        "Kidney Renal Clear Cell Carcinoma",
        "Kidney Renal Papillary Cell Carcinoma",
        "Liver Hepatocellular Carcinoma",
        "Lung Adenocarcinoma",
        "Lung Squamous Cell Carcinoma",
        "Lymphoid Neoplasm Diffuse Large B-cell Lymphoma",
        "Mesothelioma",
        "Ovarian Serous Cystadenocarcinoma",
        "Pancreatic Adenocarcinoma",
        "Pheochromocytoma and Paraganglioma",
        "Prostate Adenocarcinoma",
        "Rectum Adenocarcinoma",
        "Sarcoma",
        "Skin Cutaneous Melanoma",
        "Stomach Adenocarcinoma",
        "Testicular Germ Cell Tumors",
        "Thymoma",
        "Thyroid Carcinoma",
        "Uterine Carcinosarcoma",
        "Uterine Corpus Endometrial Carcinoma",
        "Uveal Melanoma"),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Disease type"))

gender <- setMetaClass(
    "gender",
    type = "characterORNULL",
    name = "Gender",
    description = "The collection of behaviors and attitudes that\
    distinguish people n the basis of the societal roles\
    expected for the two sexes. See NCI Thesaurus Code:\
    C17357.", # bug?
    locked = FALSE,
    suggested_values = c("Male", "Female"),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Gender type"))

age_at_diagnosis <- setMetaClass(
    "age_at_diagnosis",
    type = "integer",
    name = "Age at diagnosis",
    description = "The age in years of the case at the initial \
    pathological diagnosis of disease or cancer. \
    See NCI Thesaurus Code: C15220.",
    locked = FALSE,
    regex = "(^$)|(^[0-9]+$)",
    regexErrMsg =  "Age at diagnosis has to be non-negative integer.")

vital_status <- setMetaClass(
    "vital_status",
    type = "characterORNULL",
    name =  "Vital status",
    description =  "The state of being living or deceased for cases that\
    are part of the investigation. See NCI Thesaurus\
    Code: C25717.",
    locked = FALSE,
    suggested_values = c(
        "Alive",
        "Dead",
        "Lost to follow-up",
        "Unknown"),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Vital status"))

days_to_death <- setMetaClass(
    "days_to_death",
    type = "integer",
    name  = "Days to death",
    description =  "The number of days from the date of the initial \
    pathological diagnosis to the date of death for the \
    case in the investigation. ",
    locked = FALSE,
    regex =  "(^$)|(^[0-9]+$)",
    regexErrMsg =  "Days to death has to be non-negative integer.")

race <- setMetaClass(
    "race",
    type = "characterORNULL",
    name = "Race",
    description =  "A classification of humans characterized by certain \
    heritable traits, common history, nationality, or \
    geographic distribution. See NCI Thesaurus Code: \
    C17049.",
    locked = FALSE,
    suggested_values = c(
        "White",
        "American Indian or Alaska Native",
        "Black or African American",
        "Asian",
        "Native Hawaiian or other Pacific Islander",
        "Not reported",
        "NA"
    ),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Race"))

ethnicity <- setMetaClass(
    "ethnicity",
    type = "characterORNULL",
    name =  "Ethnicity",
    description = "A socially defined category of people based on \
    common ancestral, cultural, biological, and social \
    factors. See NCI Thesaurus Code: C29933.",
    locked = FALSE,
    suggested_values = c(
        "Hispanic or Latino",
        "Not Hispanic or Latino",
        "Not reported",
        "NA"),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Ethnicity"))

sample_uuid <- setMetaClass(
    "sample_uuid",
    type = "characterORNULL",
    name = "Sample UUID",
    description = "A unique identifier for the sample or specimen used \
    in the investigation, such as a Universally Unique \
    Identifier (UUID). A sample or specimen is material \
    taken from a biological entity for testing, \
    diagnosis, propagation, treatment, or research \
    purposes, including but not limited to tissues, body \
    fluids, cells, organs, embryos, body excretory \
    products, etc. See NCI Thesaurus Code: C19157.",
    locked = TRUE,
    regex = uuid_regex,
    regexErrMsg = "Sample UUID has to be valid UUID.")

sample_type <- setMetaClass(
    "sample_type",
    type = "characterORNULL",
    name = "Sample type",
    description = "The type of material taken from a biological entity \
    for testing, diagnosis, propagation, treatment, or \
    research purposes. This includes tissues, body \
    fluids, cells, organs, embryos, body excretory \
    products, etc. See NCI Thesaurus Code: C70713.",
    locked = FALSE,
    suggested_values = c(
        "Blood Derived Normal",
        "Buccal Cell Normal",
        "Primary Blood Derived Cancer - Peripheral Blood",
        "Recurrent Blood Derived Cancer - Peripheral Blood",
        "Primary Tumor",
        "Recurrent Blood Derived Cancer - Bone Marrow",
        "Recurrent Tumor",
        "Solid Tissue Normal",
        "Metastatic",
        "Additional - New Primary",
        "Additional Metastatic",
        "Human Tumor Original Cells",
        "Primary Blood Derived Cancer - Bone Marrow",
        "Cell Lines",
        "Xenograft Tissue",
        "Bone Marrow Normal",
        "Fibroblasts from Bone Marrow Normal"
    ),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Sample type"))

aliquot_uuid <- setMetaClass(
    "aliquot_uuid",
    type = "characterORNULL",
    name =  "Aliquot UUID",
    description = "The unique identifier for an aliquot, such as a \
    Universally Unique Identifier (UUID). The aliquot \
    is a product or unit extracted from a sample or \
    specimen and prepared for the analysis. See NCI \
    Thesaurus Code: C25414.",
    locked = TRUE,
    regex =  uuid_regex,
    regexErrMsg = "Aliquot UUID has to be valid UUID.")

aliquot_id <- setMetaClass(
    "aliquot_id",
    type = "characterORNULL",
    name = "Aliquot ID",
    description = "A human readable identifier for an aliquot, which \
    may contain metadata information. The aliquot is a \
    product or unit extracted from a sample or specimen \
    and prepared for the analysis. See NCI Thesaurus \
    Code: C25414.",
    locked = FALSE,
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Alliquot ID"))

experimental_strategy <- setMetaClass(
    "experimental_strategy",
    type = "characterORNULL",
    name = "Experimental strategy",
    description = "The method or protocol used to perform the \
    laboratory analysis. See NCI Thesaurus Code: C43622.",
    locked = FALSE,
    suggested_values = c(
        "DNA-Seq",
        "WXS",
        "WGS",
        "AMPLICON",
        "Bisulfite-Seq",
        "VALIDATION",
        "RNA-Seq",
        "miRNA-Seq",
        "Total RNA-Seq",
        "Genotyping Array",
        "Exon Array",
        "CGH Array",
        "Methylation Array",
        "Gene Expression Array",
        "miRNA Expression Array",
        "Protein Expression Array",
        "MSI- Mono- Dinucleotide Assa"))

data_type <- setMetaClass(
    "data_type",
    type = "characterORNULL",
    name = "Data type",
    description = "The classification of data used in or produced by \
    the analysis, based on its form and content. \
    See NCI Thesaurus Code: C42645.",
    locked = FALSE,
    suggested_values = c(
        "Clinical",
        "Raw Sequencing Data",
        "Raw Microarray Data",
        "Simple Nucleotide Variation",
        "Structural Rearrangement",
        "DNA Methylation",
        "Gene Expression",
        "Protein Expression",
        "Other",
        "NA"),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Data type"))

data_subtype <- setMetaClass(
    "data_subtype",
    type = "characterORNULL",
    name = "Data subtype",
    description = "A further, more specific classification of the data \
    type, based on the information that it contains.",
    locked = FALSE,
    suggested_values = c(
        "Clinical Data",
        "Biospecimen Data",
        "Unaligned Reads",
        "Aligned Reads",
        "Sequencing Tag",
        "Sequencing Tag Counts",
        "Simple Nucleotide Variation",
        "Simple Somatic Mutation",
        "Genotypes",
        "Copy Number Variation",
        "Copy Number Segmentation",
        "Copy Number Estimate",
        "Normalized Copy Numbers",
        "LOH",
        "Structural Variation",
        "Gene Expression Quantification",
        "Isoform Expression Quantification",
        "Exon Quantification",
        "Exon Junction Quantification",
        "miRNA Quantification",
        "Raw Intensities",
        "Intensities",
        "Normalized Intensities",
        "Intensities Log2 Ratio",
        "Bisulfite Sequence Alignment",
        "Methylation Beta Value",
        "Methylation Percentage",
        "Protein Expression Quantification",
        "Microsatellite Instability"),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Data subtype"))

reference_genome <- setMetaClass(
    "reference_genome",
    type = "characterORNULL",
    name = "Reference genome",
    description = "The reference assembly (such as HG19 or GRCh37) to \
    which the nucleotide sequence of a case can be \
    aligned.",
    locked = FALSE,
    suggested_values = c(
        "NCBI36_BCCAGSC_variant",
        "NCBI36_BCM_variant",
        "NCBI36_WUGSC_variant",
        "HG18",
        "HG18_Broad_variant",
        "GRCh37",
        "GRCh37-lite",
        "GRCh37_BI_Variant",
        "GRCh37-lite-+-HPV_Redux-build",
        "GRCh37-lite_WUGSC_variant_1",
        "GRCh37-lite_WUGSC_variant_2",
        "HG19",
        "HG19_Broad_variant",
        "HS37D5"),
    regex = max_length(max_chars),
    regexErrMsg = regErrFun("Reference genome"))

analysis_uuid <- setMetaClass(
    "analysis_uuid",
    type = "characterORNULL",
    name = "Analysis UUID",
    description = "The unique identifier for the analysis that \
    produced the files.",
    locked = TRUE,
    regex = uuid_regex,
    regexErrMsg = "Analysis UUID has to be valid UUID.")

gdc_file_uuid <- setMetaClass(
    "gdc_file_uuid",
    type = "characterORNULL",
    name = "GDC file UUID",
    description = "The unique identifier for a file, such as a \
    Universally Unique Identifier (UUID).",
    locked = TRUE,
    regex = uuid_regex,
    regexErrMsg = "GDC file UUID has to be valid UUID.")

data_format <- setMetaClass(
    "data_format",
    type = "characterORNULL" ,
    name  = "Data format",
    description = "Format that determines data content.",
    locked = FALSE,
    suggested_values = c(
        "TXT",
        "BAM",
        "BAI",
        "SAM",
        "Idat",
        "CEL",
        "XML",
        "VCF",
        "TARGZ",
        "TIF",
        "FSA",
        "TAR",
        "Dat",
        "FA",
        "MAF",
        "BED",
        "DGE-Tag",
        "FATSQ",
        "FASTA",
        "GTF",
        "GFF"), regex = max_length(max_chars),
    regexErrMsg = regErrFun("Data format"))

access_level_enum <- setSingleEnum(
    "AccessLevel",
    c(NA_character_, "Controlled", "Open"))

access_level <- setMetaClass(
    "access_level",
    type = "AccessLevelSingleEnum",
    transformData = access_level_enum,
    name =  "Access level",
    description  =  "Controlled data is the data from the public datasets \
    that has limitations on use and requires approval by \
    a data access committee or similar. Open data is data \
    from the public datasets that doesn't have \
    limitations on use.",
    suggested_values = c(NA_character_, "Controlled", "Open"),
    locked = FALSE)

# set classandnull
sapply(key_order, function(nm) {
    setClassUnion(paste0(nm, "ORNULL"), c(nm, "NULL"))
})

nm.cls <- sapply(key_order, function(x){paste0(x, "ORNULL")})
lst <- as.list(nm.cls)

#' Meta schema
#'
#' Meta schema
#'
#' V2 version for meta data schema
#'
#' @rdname Metadata
#' @aliases experimental_strategy library_id platform
#' platform_unit_id file_segment_number quality_scale
#' paired_end data_format file_extension reference_genome
#' data_type data_subtype analysis_uuid gdc_file_uuid
#' access_level investigation case_id case_uuid gender
#' race ethnicity primary_site disease_type
#' age_at_diagnosis vital_status days_to_death sample_id
#' sample_uuid sample_type aliquot_id aliquot_uuid
#' @export Metadata
#' @exportClass Metadata
#' @export experimental_strategy library_id platform
#' @export platform_unit_id file_segment_number quality_scale
#' @export paired_end data_format file_extension reference_genome
#' @export data_type data_subtype analysis_uuid gdc_file_uuid
#' @export access_level investigation case_id case_uuid gender
#' @export race ethnicity primary_site disease_type
#' @export age_at_diagnosis vital_status days_to_death sample_id
#' @export sample_uuid sample_type aliquot_id aliquot_uuid
#' @examples
#' ## show schema (you can still provide customized one)
#' ## empty beause they are all NULL
#' Metadata()
#' ## show schema
#' Metadata()$show(TRUE)
#' ## or
#' names(Metadata()$asList(TRUE))
#' ## returned meta field is actually define as function too, direclty
#' ## call them will give you details
#' platform()
#' paired_end()
#' quality_scale()
#' ## check their suggested value and construct your metadata
#' Metadata(platform  = "Affymetrix SNP Array 6.0", paired_end = 1, quality_scale = "sanger")
Metadata <- setRefClass("Metadata",
                        fields = c(lst, list(extra = "listORNULL")),
                        methods = list(
                            initialize = function(
                                # experimental_strategy = NULL,
                                # library_id = NULL,
                                # platform = NULL,
                                # platform_unit_id = NULL,
                                # file_segment_number = NULL,
                                # quality_scale = NULL ,
                                # paired_end = NULL,
                                # data_format = NULL,
                                # file_extension = NULL,
                                # reference_genome = NULL,
                                # data_type = NULL,
                                # data_subtype = NULL ,
                                # analysis_uuid = NULL,
                                # gdc_file_uuid = NULL,
                                # access_level = NULL,
                                # investigation = NULL,
                                # case_id = NULL,
                                # case_uuid = NULL,
                                # gender = NULL,
                                # race = NULL,
                                # ethnicity = NULL,
                                # primary_site = NULL,
                                # disease_type = NULL,
                                # age_at_diagnosis = NULL,
                                # vital_status = NULL,
                                # days_to_death = NULL,
                                # sample_id = NULL,
                                # sample_uuid = NULL,
                                # sample_type = NULL,
                                # aliquot_id = NULL,
                                # aliquot_uuid = NULL,
                                ...) {

                                args <- .dotargsAsList(...)

                                args.key <- args[names(args)[names(args) %in% key_order]]
                                args.extra <- args[names(args)[!names(args) %in% key_order]]
                                if (length(args.extra))
                                    extra <<- args.extra

                                for (nm in names(args.key)) {
                                    .self$field(nm, do.call(nm, list(data = args.key[[nm]])))
                                }

                            },

                            asList = function(full = FALSE) {
                                lst <- .getFields(.self, key_order)

                                res <- c(lst, extra)

                                res <- lapply(res, function(x){
                                    if(is(x, "Meta")){
                                        x$data
                                    }else{
                                        x
                                    }
                                })
                                if(!full){
                                    idx <- sapply(res, is.null)
                                    if(length(!idx)){
                                        res[!idx]
                                    }else{
                                        list()
                                    }

                                }else{
                                    res
                                }

                            },

                            show = function(full = FALSE, short = FALSE) {
                                l <- asList(full = full)
                                .showList(l, full = full)
                            }
                        ))

setClassUnion("MetadataORNULL", c("Metadata", "NULL"))

normalizeMeta <- function(x) {
    ## normalize it
    if (is.list(x)) {
        if (length(x) > 1) {
            res <- do.call(Metadata, x)
        } else if (length(x) == 1) {
            if (is.list(x[[1]])) {
                res <- do.call(Metadata, x[[1]])
            } else {
                res <- do.call(Metadata, x)
            }

        } else {
            res <- Metadata()
        }
    } else if (is(x, "Metadata")) {
        res <- x
    } else {
        stop("metadata has to be a list or Metadata object")
    }
    res
}
