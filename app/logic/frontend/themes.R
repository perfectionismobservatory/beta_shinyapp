box::use(
    bsl = bslib,
)

#' @export
light <- bsl$bs_theme(
  version = 5,
  success = "#6fad8c",
  primary = "black",
  base_font = bsl$font_collection(
    bsl$font_google("Roboto Mono", local = FALSE), "Roboto", "serif"
  ),
  heading_font = bsl$font_google("Roboto Mono"),
  code_font = bsl$font_google("Roboto Mono"),
  "btn-border-radius" = "0px",
  "btn-hover-bg" = "black"
)

dark <- bsl$bs_theme()
