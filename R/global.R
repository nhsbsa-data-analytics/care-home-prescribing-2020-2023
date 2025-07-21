# A global.R is the cleanest way of loading in some data before the UI is run
latest_figures <- yaml::read_yaml("data/latest_figures.yaml")

# This interpolates the values into the markdown
# 
# NOTE: the placeholders as written in the markdown are
# £> and <£ - when glue gets these it will be in HTML form, thus why different
# in this function
# 
# This would go in utils function file or similar
include_dynamic_md <- function(md_path) {
  HTML(
    glue::glue(
      shiny::includeMarkdown(md_path),
      .open = "£&gt;",
      .close = "&lt;£"
    )
  )
}