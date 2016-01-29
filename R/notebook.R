#' Sanitise a String (URL/filename safe)
#'
#' Big thanks to https://github.com/brendan-R/brocks - copied directly from
#' his package of personal functions
#'
#' Sanitise a string (downcasing, and removing puctuation and spaces), so that
#' it can safely be used in a URL or file path. Note: For URLs, hyphens, as
#' opposed to underscores are preferred by search bots.
#'
#' @param x The string to be santised
#' @param sep_char The character to use in place of spaces/punctuation found in
#'   \code{x}
#' @param ext A file extenstion to be appended to the end of the result
#'
#' @return \code{character}
#'
#' @export
#' @aliases filenamize
filenamise <- function(x, sep_char = "_", ext = "") {
  paste0(
    gsub(
      paste0(sep_char, "$|^", sep_char), "",
      gsub(
        paste0(sep_char, "+"), sep_char,
        gsub("[[:space:]]|[[:punct:]]", sep_char, tolower(x))
      )
    ),
    ext
  )
}



#' Create a New Notebook Entry
#'
#' Thanks to https://github.com/brendan-R/brocks for inspiring this function
#'
#' This function will add a new entry within the '_rmd' directory under a
#' subdirectory by default and create a blank .R script to accompany it
#'
#' @param title The title of the entry
#' @param dir The directory all of your .Rmd entries are in
#' @param subdir The subdirectory with \code{dir} that you want your new entry
#'   to be placed it
#' @param skeleton The filepath of a skeleton of a new entry
#'
#' @details { \code{new_entry} will create a .R file, and a .Rmd file
#'   withinin a subdirectory, with names created by running \code{title} through
#'   \code{\link{filenamise}}. The .R file will contain a short note mentioning
#'   that it accompanies the .Rmd file, which will contain the same text as the
#'   file supplied by \code{skeleton} paramter. }
#'
#' @export
new_entry <- function(title    = "New entry",
                      dir      = "_rmd",
                      subdir   = "new_entry",
                      skeleton = "_build/skeleton_entry.Rmd") {

  # Sanitise the post title
  fname <- filenamise(title, sep_char = "-")

  # create post directory, if it doesn't already exist
  if(!dir.exists(dir)){
    stop(paste0(dir, " doesn't exist. Abort."))
  }
  fpath <- file.path(dir, subdir)
  dir.create(fpath)

  rmd_name <- file.path(fpath, paste0(Sys.Date(), "-", fname, ".Rmd"))
  r_name   <- file.path(fpath, paste0(fname, ".R"))

  # make sure the skeleton file exists
  if(!file.exists(skeleton)){
    stop(paste0("File ", skeleton, " does not exist. Abort"))
    #skeleton <- system.file("skeleton_entry", package = "")
  }

  # write skeleton post to empty file
  post <- readLines(skeleton)
  post[grepl("title: ", post)] <- paste0("title: ", title)
  writeLines(post, rmd_name)

  # write out an empty R file as well, in case that's useful
  writeLines(
    c("# This R file accomanies the .Rmd file", paste("#", rmd_name), ""),
    r_name
  )

}

#' Create a New Notebook Page
#'
#' This function will create a new notebook 'Page' under the '_pages' directory
#' by default
#'
#' @param title The title of the page
#' @param dir The directory all of your .Rmd pages are in
#' @param skeleton The filepath of a skeleton of a new page
#'
#' @details { \code{new_page} will create a .R file, and a .Rmd file
#'   withinin a directory, with names created by running \code{title} through
#'   \code{\link{filenamise}}. The .R file will contain a short note mentioning
#'   that it accompanies the .Rmd file, which will contain the same text as the
#'   file supplied by \code{skeleton} paramter. }
#'
#' @export
new_page <- function(title    = "New page",
                     dir      = "_pages",
                     skeleton = "_build/skeleton_page.Rmd") {

  # Sanitise the post title
  fname <- filenamise(title, sep_char = "-")

  rmd_name <- file.path(dir, paste0(fname, ".Rmd"))
  r_name   <- file.path(dir, paste0(fname, ".R"))

  # make sure the skeleton file exists
  if(!file.exists(skeleton)){
    stop(paste0("File ", skeleton, " does not exist."))
    #skeleton <- system.file("skeleton_entry", package = "")
  }

  # write skeleton post to empty file
  post <- readLines(skeleton)
  post[grepl("title: ", post)] <- paste0("title: ", title)
  post[grepl("permalink: ", post)] <- paste0("permalink: ", paste0("/",title,"/"))
  writeLines(post, rmd_name)

  # write out an empty R file as well, in case that's useful
  writeLines(
    c("# This R file accomanies the .Rmd file", paste("#", rmd_name), ""),
    r_name
  )

}

#' Build the Jekyll Notebook
#'
#' This function will call servr to build your .Rmd files in .md files and
#' further build those into html files within the '_site' directory
#'
#' @param input The input vector of directories to look for .Rmd files in
#' @param output The output vector to place .md files in that have been
#'    converted from .Rmd files
#' @param script The filepath to your build.R script
#' @param config The config file to use to for jekyll
#'
#' @details { \code{build_nb} takes input and output vectors that are in
#'    in one-to-one relationships with one another. So the first element of the
#'    input vector is output in the first element of the output vector. The
#'    function first removes the '_site' directory, then converts the .Rmd files
#'    to jekyll compatible .md files, and finally converts the .md files to
#'    html files under '_site.' }
#'
#' @export
build_nb <- function(input  = c("_pages", list.dirs("_rmd")),
                     output = c(".", rep("_posts", length(list.dirs("_rmd")))),
                     script = "_build/build.R",
                     config = "_config_dev.yml") {

  # delete _site directory for build
  system("rm -rf _site")

  # build site using servr and jekyll
  servr::jekyll(
    input   = input,
    output  = output,
    script  = script,
    serve   = FALSE,
    command = paste0("jekyll build --config ", config))

}

#' Serve the Jekyll Notebook
#'
#' This function will make a system call using jekyll serve on the specified
#' host address, port, and using the specified config file
#'
#' @param host The host address to serve the jekyll site on
#' @param port The port to make the jekyll site accessible on
#' @param config The config file to use to for jekyll
#'
#' @details { \code{serve_nb} will build the notebook only using jekyll. It will
#'    not rebuild any .Rmd files. \code{\link{build_nb}} needs to be run in
#'    order to rebuild any changed .Rmd files. }
#'
#' @export
serve_nb <- function(host = "0.0.0.0",
                     port = "4000",
                     config = "_config_dev.yml") {

  cmd <- paste("jekyll",  "serve",
             "--host",   host,
             "--port",   port,
             "--config", config,
             "--detach",
             "--watch")

  system(cmd)
}

#' Kill the Jekyll Serve
#'
#' This function will stop the jekyll server using a system call
#'
#' @details { \code{kill_nb} calls 'pkill -f jekyll' to kill the jekyll serve
#'     process. }
#'
#' @export
kill_nb <- function() {

  cmd <- "pkill -f jekyll"
  cat("Running", cmd, "...\n")
  system(cmd)
  cat("Jekyll server stopped.\n")

}

#push_nb <- function() {

  # This should be used to push
  # a live site to github-pages
  # probably build site locally
  # then push _site directory to
  # github-pages for hosting

#}

#' Build the References
#'
#' This function will call a script that contains a list of references using
#' the \code{knitcitations} package
#'
#' @details { \code{build_refs} calls \code{script} to build your references
#'     into 'references.bib' in the project root directory. }
#'
#' @param script Build script to build the bibliography and bibtex file
#' references.bib
#'
#' @export
build_refs <- function(script = "_build/references.R") {

  if(!file.exists(script)){
    stop(paste0(script, " doesn't exist. Abort."))
  }
  cat("Building references using", script)
  source(script)
  # check to make sure references.bib was created
  #if(!file.exists(""))

}
