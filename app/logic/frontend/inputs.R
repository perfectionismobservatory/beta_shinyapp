box::use(
    sh = shiny,
    shw = shinyWidgets,
    lub = lubridate,
    bsi = bsicons,
    bsl = bslib,
    rl = rlang[`%||%`],
    shj = shinyjs,
    pr = purrr,
)

#' @export
btn_return <- function(id, label = "Back", icon = sh$icon("angles-left")) {
    sh$actionButton(
        id,
        class = "btn btn-secondary hover shadow border-info border-4",
        label = label,
        icon = icon
    )
}

#' @export
radio <- function(id, label, choices, selected = NULL) {
    shw$prettyRadioButtons(
        id,
        label = label,
        choices = choices,
        selected = selected,
        status = "primary",
        shape = "round",
        animation = "smooth",
        outline = TRUE
    )
}

#' @export
checkboxgroup <- function(id, label, choices, selected = NULL) {
    shw$prettyCheckboxGroup(
        id,
        label = label,
        choices = choices,
        selected = selected %||% choices,
        status = "primary",
        shape = "curve",
        icon = sh$icon("check"),
        animation = "smooth",
        outline = TRUE
    )
}

# CSS classes for inputs below
class_summary <- "d-flex flex-row align-items-center justify-content-between"
class_center <- "d-flex flex-row justify-content-center align-items-center"
class_header <- "d-flex gap-2 align-items-center"

#' @export
class_button <- "btn btn-secondary hover shadow border-info border-4 px-4"

#' @export
validation_icons <- list(
    year = bsi$bs_icon("calendar-date"),
    scale = bsi$bs_icon("rulers"),
    sample = bsi$bs_icon("person-bounding-box"),
    age = bsi$bs_icon("calculator"),
    status = bsi$bs_icon("send-check"),
    details = bsi$bs_icon("zoom-in")
)

icon_filled <- list(
    bsi$bs_icon("x-lg", class = "text-danger"),
    bsi$bs_icon("check-lg", class = "text-success")
)

#' @export
#' List of anonymous functions to programmatically create the summary
validation_summary <- list(
    year = \(x, fill) {
        sh$div(
            class = class_summary,
            sh$div(class = class_header, validation_icons$year, x), icon_filled[[fill + 1]]
        )
    },
    scale = \(x, fill) {
        sh$div(
            class = class_summary,
            sh$div(class = class_header, validation_icons$scale, x), icon_filled[[fill + 1]]
        )
    },
    sample = \(x, fill) {
        x <- ifelse(x == "University students", "Uni students", x)
        sh$div(
            class = class_summary,
            sh$div(class = class_header, validation_icons$sample, x), icon_filled[[fill + 1]]
        )
    },
    age = \(x, fill) {
        sh$div(
            class = class_summary,
            sh$div(class = class_header, validation_icons$age, x), icon_filled[[fill + 1]]
        )
    },
    status = \(x, fill) {
        sh$div(
            class = class_summary,
            sh$div(class = class_header, validation_icons$status, x), icon_filled[[fill + 1]]
        )
    },
    details = \(x, fill) {
        sh$div(
            class = class_summary,
            sh$div(class = class_header, validation_icons$details, x), icon_filled[[fill + 1]]
        )
    }
)


#' @export
validation_inputs <- list(
    list( # no restriction
        header = sh$div(class = class_header, validation_icons$year, "Data collection"),
        body = \(ns) sh$numericInput(
            ns("year"),
            label = "Enter year",
            min = 1988,
            max = lub$year(lub$today()),
            value = NA,
            width = "150px",
        )
    ),
    list( # must not be other
        class = class_center,
        header = sh$div(class = class_header, validation_icons$scale, "Scale"),
        body = \(ns) radio(
            ns("scale"),
            label = NULL,
            choices = c("Unspecified", "F-MPS", "HF-MPS", "Other")
        )
    ),
    list(
        class = class_center,
        header = sh$div(class = class_header, validation_icons$status, "Publication status"),
        body = \(ns) radio(
            ns("status"),
            label = NULL,
            choices = c("Unspecified", "Published", "In review", "Unpublished")
        )
    ),
    list( # must be below 25
        header = sh$div(class = class_header, validation_icons$age, "Age"),
        body = \(ns) sh$textInput(ns("age"), "Enter sample mean", placeholder = "e.g. 20.3")
    ),
    list( # must be uni
        class = class_center,
        header = sh$div(class = class_header, validation_icons$sample, "Sample type"),
        body = \(ns) radio(
            ns("sample"),
            label = NULL,
            choices = c("Unspecified", "University students", "General public", "Other")
        )
    )
)

#' @export
conditional_validation_inputs <- list(
    name = \(ns) sh$textInput(ns("name"), "Enter main author", placeholder = "Surname, Name"),
    # either make custom input or only corresponding author mail?
    email = \(ns) sh$textInput(ns("email"), "Enter institution email"),
    type = \(ns) radio(ns("type"), "Type of document", c("Unspecified", "Journal article", "Thesis", "Poster")),
    pubyear = \(ns) sh$numericInput(
        ns("pubyear"),
        "Year of publication",
        min = 1988,
        max = lub$year(lub$today()),
        value = NA,
        width = "150px",
    ),
    doi = \(ns) sh$textInput(ns("doi"), "Enter doi", placeholder = "10. ..."),
    prereg = \(ns) sh$textInput(ns("prereg"), "Enter preregistration link", placeholder = "https:// ...")
)

#' @export
conditional_validation_card <- function(...) {
    bsl$card(
        bsl$card_header(sh$div(class = class_header, validation_icons$details, "Details")),
        bsl$card_body(...)
    )
}

#' @export
btn_modal <- function(id, label, modal_title, footer_confirm = NULL, footer_dismiss = NULL, ..., modal_bl = NULL) {
    dots <- rl$list2(...)

    if (!is.null(footer_dismiss)) {
        footer_dismiss <- sh$tags$button(
            id = paste0(id, "_dismiss"),
            type = "button",
            class = class_button,
            `data-bs-dismiss` = "modal",
            footer_dismiss
        )
    }

    if (!is.null(footer_confirm)) {
        footer_confirm <- sh$tags$button(
            id = paste0(id, "_confirm"),
            type = "button",
            class = "btn btn-success action-button hover-success",
            `data-bs-dismiss` = "modal",
            footer_confirm
        )
    }

    if (!is.null(dots$class_toggle)) {
        toggle <- sh$tags$button(
            id = paste0(id, "_toggle"),
            class = dots$class_toggle,
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = paste("#inputModal", id, sep = "-"),
            label
        )
    } else {
        toggle <- sh$tags$button(
            id = paste0(id, "_toggle"),
            class = "btn btn-secondary hover",
            type = "button",
            `data-bs-toggle` = "modal",
            `data-bs-target` = paste("#inputModal", id, sep = "-"),
            label
        )
    }

    sh$div(
        toggle,
        sh$div(
            class = "modal fade",
            id = paste("inputModal", id, sep = "-"),
            tabindex = "-1",
            `aria-labelledby` = paste("inputModalLabel", id, sep = "-"),
            `aria-hidden` = "true",
            sh$div(
                class = "modal-dialog",
                sh$div(
                    class = "modal-content",
                    sh$div(
                        class = "bg-secondary modal-header",
                        sh$tags$h1(
                            class = "modal-title fs-5",
                            id = paste("inputModalLabel", id, sep = "-"),
                            modal_title
                        ),
                        sh$tags$button(
                            id = paste0(id, "_x"),
                            type = "button",
                            class = "btn-close",
                            `data-bs-dismiss` = "modal",
                            `aria-label` = "Close"
                        ),
                    ),
                    sh$div(
                        class = "modal-body",
                        !!!dots
                    ),
                    sh$div(
                        class = "modal-footer justify-content-between",
                        sh$div(modal_bl),
                        sh$div(
                            footer_dismiss,
                            footer_confirm
                        )
                    )
                )
            )
        )
    )
}

#' @export
#' Custom toggle switch
disabled_upload_inputs <- list(
    age = \(id, ns, value) {
        shj$disabled(
            sh$textInput(
                ns(id),
                "Mean age",
                value = value,
                width = "120px"
            )
        )
    },
    year = \(id, ns, value) {
        shj$disabled(
            sh$numericInput(
                ns(id),
                "Data collection",
                value = value,
                width = "120px"
            )
        )
    },
    scale = \(id, ns, value) {
        shj$disabled(
            sh$textInput(
                ns(id),
                "Scale",
                value = value,
                width = "120px"
            )
        )
    },
    doi = \(id, ns, value) {
        shj$disabled(
            sh$textInput(
                ns(id),
                "DOI",
                value = value,
                width = "265px"
            )
        )
    },
    prereg = \(id, ns, value) {
        shj$disabled(
            sh$textInput(
                ns(id),
                "Preregistration link",
                value = value,
                width = "265px"
            )
        )
    },
    status = \(id, ns, value) {
        shj$disabled(
            sh$textInput(
                ns(id),
                "Status",
                value = value,
                width = "120px"
            )
        )
    },
    sample = \(id, ns, value) {
        shj$disabled(
            sh$textInput(
                ns(id),
                "Sample",
                value = value,
                width = "120px"
            )
        )
    },
    type = \(id, ns, value) {
        shj$disabled(
            sh$textInput(
                ns(id),
                "Document type",
                value = value,
                width = "120px"
            )
        )
    },
    name = \(id, ns, value) {
        shj$disabled(
            sh$textInput(
                ns(id),
                "Author name",
                value = value,
                width = "120px"
            )
        )
    }
)

#' @export
toggleswitch <- function(id, label, value = FALSE, class = NULL) {
    if (!is.null(class)) {
        class <- paste("form-check form-switch", class, " ")
    } else {
        class <- "form-check form-switch"
    }

    input_tag <- sh$tags$input(
        class = "form-check-input",
        type = "checkbox",
        role = "switch",
        id = id
    )

    if (!is.null(value) && value) {
        input_tag$attribs$checked <- "checked"
    }

    sh$div(
        class = class,
        input_tag,
        sh$tags$label(
            class = "form-check-label",
            `for` = id,
            label
        )
    )
}

#' @export
scale_lookup <- list(
    "HF-MPS" = c(
        sop = "SOP (Self-oriented perfectionism)",
        oop = "OOP (Other-oriented perfectionism)",
        spp = "SPP (Self-prescribed perfectionism)"
    ),
    "F-MPS" = c(
        ps = "PS (Personal standards)",
        pe = "PE (Parental expectation)",
        pc = "PC (Parental criticism)",
        com = "COM (Concerns over mistakes)",
        daa = "DAA (Doubts about actions)",
        o = "O (Organisation)"
    )
)

#' @export
conditional_scale_inputs <- function(scale, ns) {
    sh$div(
        class = "d-flex flex-row flex-wrap justify-content-center gap-4",
        !!!pr$map(names(scale_lookup[[scale]]), \(v) {
            sh$div(
                class = "d-flex flex-column p-3 bg-secondary border border-info rounded",
                style = "text-align: center;",
                sh$p(scale_lookup[[scale]][v]),
                sh$div(
                    class = "d-flex flex-row gap-4",
                    sh$numericInput(
                        ns(paste0(v, "_nitems")),
                        "Item number",
                        value = NA,
                        width = "120px"
                    ),
                    sh$numericInput(
                        ns(paste0(v, "_mean")),
                        "Mean",
                        value = NA,
                        width = "120px"
                    ),
                    sh$numericInput(
                        ns(paste0(v, "_sd")),
                        "SD",
                        value = NA,
                        width = "120px"
                    )
                ),
            )
        })
    )
}
