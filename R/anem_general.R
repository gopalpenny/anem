# anem_general

#' Is shiny running?
#'
#' Check to see if code is being run in Shiny environment
shiny_running <- function() {
  # Look for `runApp` call somewhere in the call stack.
  frames = sys.frames()
  calls = lapply(sys.calls(), `[[`, 1)
  call_name = function (call)
    if (is.function(call)) '<closure>' else deparse(call)
  call_names = vapply(calls, call_name, character(1))

  target_call = grep('^runApp$', call_names)

  if (length(target_call) == 0)
    return(FALSE)

  # Found a function called `runApp`, verify that it’s Shiny’s.
  target_frame = frames[[target_call]]
  namespace_frame = parent.env(target_frame)
  isNamespace(namespace_frame) && environmentName(namespace_frame) == 'shiny'
}
