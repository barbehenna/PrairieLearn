#' Source a file as a particular user
#'
#' The \code{source_std} helper sources a user-specified file from the
#' \code{/grade/student/} directory as a user-specified \code{"uid"}. The
#' \code{source_ref} helper does the same for the \code{/grade/tests/}
#' directory. Generally, \code{source_std} should be used to source a student's
#' solution, and \code{source_ref} should be used to source the reference
#' solution. Both functions default to the user \code{"ag"}, which is the
#' recommended user when sourcing student solutions. Situations could arise
#' where higher privileges are necessary when sourcing reference solutions.
#'
#' @param file Path to an R file to be sourced, typically containing student
#' code to be evaluated safely, that is, with lower privileges.
#' @param name Name of file (usually student or reference solution) to be
#' sourced.
#' @param uid Optional numeric or character user id identifying the user id with
#' (presumably lower) privileges as which the code is running. The corresponding
#' numeric \code{uid} is obtained via \code{\link[unix]{user_info}} when a
#' character \code{uid} is supplied. Note that using this argument requires
#' being the \sQuote{root} user. Default value of \code{"ag"} will source
#' supplied code with privileges that prevent access to sensitive files such as
#' reference solutions.
#' @param img_name Optional character used as the file name for the
#' \code{\link{png}} file that may be created while sourcing \code{file}. When
#' \code{img_name} is NULL a random png name is created in the current working
#' directory. No png file is created if no plotting happens in \code{file}.
#'
#' @return An environment containing objects created through sourcing the
#' \code{file} as \code{uid}. Any sourced functions are modified such that
#' future use of the function will be restricted by the lower privileges given
#' to the supplied \code{uid}.
#'
#' @examples
#' \dontrun{
#' std <- source_as_uid(file = "/grade/student/sub.R", uid = "ag")
#' ref <- source_as_uid(file = "/grade/student/ans.R", uid = "ag")
#' std <- source_std("sub.R")
#' ref <- source_ref("ans.R")
#' }
source_as_uid <- function(file, uid="ag", img_name = NULL) {

  # get and set integer uid
  if (!is.null(uid) && class(uid) == "character") uid <- user_info(uid)$uid

  # handle incorrectly specified files
  if (!file.exists(file)) return(invisible(NULL))

  # handle incorrectly specified files
  if (is.null(img_name)) img_name <- tempfile(pattern = "plot_", tmpdir = ".", fileext = ".png")

  # change file permissions, track original file permissions
  oldmode <- file.mode(file)
  Sys.chmod(file, mode="0664")

  # safely source the file as specified uid to an environment
  sourced_env <- eval_safe({
    temp_env <- new.env()
    png(img_name)
    source(file, temp_env, print.eval = TRUE)
    dev.off()
    temp_env
  }, uid = uid)

  # restore original file permissions
  Sys.chmod(file, mode=oldmode)

  for (f in lsf.str(envir = sourced_env)) {
    body(sourced_env[[f]]) <- parse(text = paste("eval_safe(", as.expression(body(sourced_env[[f]])), ", uid =", uid, ")"))
  }

  # return sourced objects as an environment
  return(sourced_env)

}

#' @rdname source_as_uid
source_std <- function(name, uid = "ag", img_name = "std_plot.png") {
  full_path <- file.path("/grade/student", name)
  source_as_uid(file = full_path, uid = uid, img_name = img_name)
}

#' @rdname source_as_uid
source_ref <- function(name, uid = "ag", img_name = "ref_plot.png") {
  full_path <- file.path("/grade/tests", name)
  source_as_uid(file = full_path, uid = uid, img_name = img_name)
}
