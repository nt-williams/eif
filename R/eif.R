new_eif <- function(x = double(), ic = double()) {
    vec_assert(x, double(), size = 1L)
    vec_assert(ic, double())
    new_vctr(x, ic = ic, class = "eif_eif")
}

eif <- function(x = double(), ic = double()) {
    new_eif(vec_cast(x, double()), ic)
}

is_eif <- function(x) {
    inherits(x, "eif_eif")
}

#' @export
format.eif_eif <- function(x, ...) {
    ic <- get_eif(x)
    ci <- vec_data(x) + c(-1, 1) * qnorm(0.975) * sqrt(var(ic) / length(ic))
    sprintf("%.0f [%.0f, %.0f]", vec_data(x), ci[1], ci[2])
}

#' @export
vec_ptype_abbr.eif_eif <- function(x, ...) {
    "eif"
}

get_eif <- function(x) {
    attr(x, "ic", exact = TRUE)
}
