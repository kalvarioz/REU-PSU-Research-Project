# =================================================================================================
# modules/input_validation.R
# 
# Input Validation and Safety Functions
#
# This module contains functions for:
# - Safe input access with fallbacks
# - Input existence validation
# - Input readiness checking
# - Input retrieval with fallbacks
#
# Brandon Calvario
# =================================================================================================

# ====================================================================
# INPUT VALIDATION & SAFETY
# ====================================================================

safe_input_access <- function(input_id, default_value = NULL) {
  tryCatch({
    if (!is.null(input[[input_id]])) {
      return(input[[input_id]])
    }
    return(default_value)
  }, error = function(e) {
    return(default_value)
  })
}
validate_input_exists <- function(input_id, input) {
  !is.null(input[[input_id]]) && input[[input_id]] != ""
}
check_input_readiness <- function(required_inputs, input) {
  all(sapply(required_inputs, function(x) validate_input_exists(x, input)))
}
get_input_with_fallback <- function(input_id, fallback, input) {
  if (validate_input_exists(input_id, input)) {
    return(input[[input_id]])
  }
  return(fallback)
}
