box::use(
    bsl = bslib,
)

#' @export
light <- bsl$bs_theme(
  version = 5,
  success = "#6fad8c",
  primary = "black",
  base_font = bsl$font_collection(
    bsl$font_google("Commissioner", local = FALSE), "Roboto", "serif"
  ),
  heading_font = bsl$font_google("Fraunces"),
  code_font = bsl$font_google("Roboto Mono")
  #spacer = "1.5rem"
  #"btn-border-radius" = "0px",
  #"btn-hover-bg" = "black"
)

dark <- bsl$bs_theme()
