#' Read an RLI target from a targets pipeline
#'
#' Reads a target from a [targets] pipeline, optionally appending a trait
#' name to the target name. This provides a convenient wrapper around
#' [targets::tar_read_raw()] for reading RLI-related targets.
#'
#' @param name A character string giving the base name of the target.
#' @param trait A character string giving the trait to append to the target
#'   name. Defaults to `NULL`, in which case the target name is unchanged.
#' @param store A character string giving the path to the targets store.
#'   Defaults to `"./source/targets"`.
#' @param ... Additional arguments passed to [targets::tar_read_raw()].
#'
#' @return The value stored in the requested target.
#'
tar_read_rli <- function(
  name,
  trait = NULL,
  store = "./source/targets",
  ...
) {
  # Specify name
  if (!is.null(trait)) {
    name <- paste0(name, "_", trait)
  }

  # Get target
  targets::tar_read_raw(
    name = name,
    ...,
    store = store
  )
}
