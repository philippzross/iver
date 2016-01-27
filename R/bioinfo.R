#' @title
#' GFF3 to BED12 converter
#'
#' @description
#' Will convert a GFF3 GRanges object to a BED12 dataframe
#'
#' @details
#' Provide a GRanges object in GFF3 format or a file path to a GFF3 formatted
#' annotation file and return a dataframe that meets the specs of a BED12 file
#' as specified by UCSC
#'
#' @param obj GRanges object
#' @param file file path to GFF3 input file
#' @return out BED12 formatted dataframe
#'
#' @export
gff2bed12 <- function(obj, file = NULL) {

  # executable file paths (where they should be)
  gfftogenepred_exe <- "/usr/local/bin/gff3ToGenePred"
  genepredtobed_exe <- "/usr/local/bin/genePredToBed"

  # check to make sure the executables are installed in the right place
  if(!file.exists(gfftogenepred_exe)) {
    stop("Can't locate gff3ToGenePred...is it installed?")
  }
  if(!file.exists(genepredtobed_exe)) {
    stop("Can't locate genePredToBed...is it installed?")
  }

  # make sure inputs are there
  if(!is.null(file)) {
    if(!file.exists(file)) {
      stop(paste0(file, " does not exist. Abort"))
    }
    inputgff <- rtracklayer::import.gff3(file)
  } else if(!missing(obj)) {
    inputgff <- obj
  } else {
    stop("No input. Abort.")
  }

  # create temporary intermediate files
  gff_tmp      <- "/tmp/tmp.gff"
  genepred_tmp <- "/tmp/tmp.genePred"
  bed_tmp      <- "/tmp/tmp.bed"
  file.create(gff_tmp)
  file.create(genepred_tmp)
  file.create(bed_tmp)

  # write GFF file to temporary file
  rtracklayer::export.gff3(inputgff, gff_tmp)

  # create genePred intermediate file
  system(paste(
    gfftogenepred_exe,
    gff_tmp,
    genepred_tmp))
  # generate BED12 from genePred intermediate
  system(paste(
    genepredtobed_exe,
    genepred_tmp,
    bed_tmp))

  # import output file back into R
  out <- rtracklayer::import.bed(bed_tmp)

  # remove tmporary files
  file.remove(gff_tmp)
  file.remove(genepred_tmp)
  file.remove(bed_tmp)

  # make sure they're actually gone!
  if(file.exists(genepred_tmp)) {
    stop("Something's wrong! gff temporary file wasn't removed...")
  }
  if(file.exists(genepred_tmp)) {
    stop("Something's wrong! genPred temporary file wasn't removed...")
  }
  if(file.exists(bed_tmp)) {
    stop("Something's wrong! bed temporary file wasn't removed...")
  }

  # sort bed file
  out <- GenomicRanges::sort.GenomicRanges(x = out, decreasing = TRUE)

  # return sorted BED12 file
  return(out)
}
