box::use(
    lub = lubridate,
    str = stringr,
)

box::use(
    app / logic / backend / misc[between],
)

regex_mail <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
regex_doi <- "\\b(10\\.\\d{4,5}/[-._;()/:A-Za-z0-9]+)\\b"

#' @export
invalid_format <- list(
    year = \(x) !between(1900, x, lub$year(lub$today())),
    age = \(x) !between(18, x, 100),
    name = \(x) !str$str_detect(x, "\\w+\\, \\w+") && x != "",
    email = \(x) !str$str_detect(x, regex_mail) && x != "",
    doi = \(x) !str$str_detect(x, regex_doi) && x != "",
    pubyear = \(x) !between(1900, x, lub$year(lub$today()))
)