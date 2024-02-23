box::use(
    sh = shiny,
    pr = purrr,
    lub = lubridate,
)

#' @export
#' REMINDER sift_cols() does not work with by-item-selection of numeric vars
#' Numeric vars are always treated as ranges or cut-offs
#' For by-item-selection, treat respective vars as characters!
#' Regarding factor types, I think we should not have factors at this stage -> if needed, convert later
filter_col <- function(col, val) {
    if (is.null(val) || all("all" == val)) {
        # If no input exists or "all" should be shown, don't filter
        # FIX `is.null(val)` has some unintended behavior when the user selects nothing in multiselect inputs
        return(TRUE)
    } else if (is.logical(val)) { # logical before numeric to not treat logical as numeric
        return(as.logical(col) == val)
    } else if (is.numeric(col) && length(val) == 1) {
        return(!is.na(col) & col >= as.numeric(val))
    } else if (is.numeric(col) && length(val) == 2) { # numeric range
        return(!is.na(col) & col >= min(val) & col <= max(val))
    } else if (is.character(col)) {
        return(!is.na(col) & col %in% val)
    } else {
        # No control, so don't filter
        TRUE
    }
}

#' @export
#' Loops over all column names in data which have matched inputs
#' Creates bool vectors for each as per filter_col
#' And combines them into a single one with iterative `&` concatenation
filter_inputs <- function(data, input) {
    stopifnot(sh$is.reactive(data))

    vars <- sh$reactive(colnames(data()))

    sh$reactive({
        each_var <- pr$map(vars(), \(v) filter_col(data()[[v]], input[[v]]))

        # Apply `&` iteratively to pairs of bool vectors in each_var until there is only one bool vector left
        # This vector will extract those rows where all filter conditions where TRUE
        pr$reduce(each_var, `&`)
    })
}
