vec_arith.eif_eif <- function(op, x, y, ...) {
    UseMethod("vec_arith.eif_eif", y)
}

vec_arith.eif_eif.default <- function(op, x, y, ...) {
    stop_incompatible_op(op, x, y)
}

vec_arith.eif_eif.eif_eif <- function(op, x, y, ...) {
    switch(
        op,
        "+" = eif_plus(x, y),
        "-" = eif_minus(x, y),
        "/" = eif_division(x, y),
        stop_incompatible_op()
    )
}

eif_plus <- function(x, y) {
    new_eif(
        vec_data(x) + vec_data(y),
        attr(x, "ic", exact = TRUE) + attr(y, "ic", exact = TRUE)
    )
}

eif_minus <- function(x, y) {
    new_eif(
        vec_data(x) - vec_data(y),
        attr(x, "ic", exact = TRUE) - attr(y, "ic", exact = TRUE)
    )
}

eif_division <- function(x, y) {
    psi_x <- vec_data(x)
    psi_y <- vec_data(y)
    ic_x <- attr(x, "ic", exact = TRUE)
    ic_y <- attr(y, "ic", exact = TRUE)
    new_eif(
        vec_data(x) / vec_data(y),
        (ic_x / psi_y) - (psi_x * ic_y / psi_y^2)
    )
}
