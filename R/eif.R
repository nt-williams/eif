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
