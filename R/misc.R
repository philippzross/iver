#' Copy file to local dropbox directory
#'
#' @param from_file Local file to copy to Dropbox
#' @param to_file Output Dropbox file path
#'
#' @export
copy_to_dropbox <- function(from_file, to_file) {
  
  # where is drobox directory?
  dropbox_dir <- "~/Dropbox/"
  
  # does the dropbox directory exist?
  if(!dir.exists(dropbox_dir)) {
    stop(paste("Can't find", dropbox_dir, "... Abort."))
  }
  
  # check if file you want to move exists
  if(!file.exists(from_file)) {
    stop(paste(from_file, "does not exist ... Abort."))
  }
  
  # output file has same name as input file by default
  if(missing(to_file)) {
    out_file <- basename(from_file)
  } else {
    out_file <- to_file
  }
  
  # create output file path
  out_file <- paste0(dropbox_dir, out_file)
  
  cmd <- paste("cp", from_file, out_file)
  system(cmd)
  
}  

#' Copy file from local dropbox directory
#'
#' @param from_file Local Dropbox file to copy
#' @param to_file Output local file path
#'
#' @export
copy_from_dropbox <- function(from_file, to_file) {
  
  # where is drobox directory?
  dropbox_dir <- "~/Dropbox/"
  
  # does the dropbox directory exist?
  if(!dir.exists(dropbox_dir)) {
    stop(paste("Can't find", dropbox_dir, "... Abort."))
  }
  
  # file to copy is on Dropbox
  in_file <- paste0(dropbox_dir, from_file)
  
  # check if file you want to move exists
  if(!file.exists(in_file)) {
    stop(paste(in_file, "does not exist ... Abort."))
  }
  
  # output file has same name as input file by default
  if(missing(to_file)) {
    out_file <- paste0("./", basename(in_file))
  } else {
    out_file <- paste0("./", to_file)
  }
  
  cmd <- paste("cp", in_file, out_file)
  system(cmd)
  
}