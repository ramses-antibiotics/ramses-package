

#' Deprecated functions in package \pkg{Ramses}.
#' 
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("<function>-deprecated")}.
#'  \itemize{
#'    \item{\code{\link[Ramses]{therapy_table}()} is now known as
#'     \code{\link[Ramses]{longitudinal_table}()}}
#'  }
#' @name Ramses-deprecated
#' @keywords internal
NULL


#' Get the therapy table (DEPRECATED)
#'
#' @param object an object of class \code{TherapyEpisode}
#' @param collect if \code{TRUE}, collect the remote \code{tbl_sql} and return a local 
#' \code{tbl_df}. The default is \code{FALSE}, and simply returns the remote \code{tbl_sql}
#' @return an object of class \code{tbl}
#' @rdname therapy_table-deprecated
#' @description This function is now deprecated. Please use
#'  \code{\link{longitudinal_table}()}.
#' @seealso \code{\link{Ramses-deprecated}}
#' @export
therapy_table <- function(object, collect = FALSE) {
  .Deprecated("longitudinal_table")
  longitudinal_table(
    object = object, 
    collect = collect
  )
}