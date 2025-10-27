box::use(
    lub = lubridate,
    str = stringr,
)

regex_mail <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
regex_doi <- "\\b(10\\.\\d{4,5}/[-._;()/:A-Za-z0-9]+)\\b"

#' @export
invalid_format <- list(
    age = \(x) !str$str_detect(x, "\\d+\\.\\d+") && x != "",
    name = \(x) !str$str_detect(x, "\\w+\\, \\w+") && x != "",
    email = \(x) !str$str_detect(x, regex_mail) && x != "",
    doi = \(x) !str$str_detect(x, regex_doi) && x != "",
    color = \(x) !(str$str_length(x) == 7) && !(str$str_length(x) == 9)
)

#' @export
is_valid <- function(input, name) {
    # If no check exists, input is always valid
    if (is.null(invalid_format[[name]])) {
        return(TRUE)
    } else {
        !invalid_format[[name]](input[[name]])
    }
}
