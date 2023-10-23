library(rmarkdown)
library(officedown)
library(purrr)
library(stringr)

# Setup -------------------------------------------------------------------

md_dir     <- "inst/markdown"     # Markdown dir
rv_dir     <- "review"            # Review directory
rmd_file   <- "all_md.rmd"        # Output rmarkdown
docx_file  <- "review.docx"       # Output Word doc
styles_dir <- "review/styles"     # Styles dir
styles_rmd <- "draft-styles.rmd"  # Creates Word template
styles_doc <- "draft-styles.docx" # Word template

render(
  file.path(styles_dir, styles_rmd),
  output_dir = styles_dir,
  output_file = styles_doc,
  quiet = TRUE
)


# Create rmarkdown --------------------------------------------------------

# Get relative path of md files
md_files <- Sys.glob(file.path(md_dir, "*.md"))

# Combine them into one rmarkdown file
all_md <- map(md_files, readLines) # List of md content

# all_md <- map(
#   all_md, 
#   \(x) map(
#     x,
#     \(xx) {
#       browser()
#       open_tags  <- str_locate_all(xx, "<([a-z]+)(?![^>]*\\/>)[^>]*>")
#       close_tags <- str_locate_all(xx, "<\\/[a-z]+>")
#       if (!is.null(nrow(open_tags[[1]]))) {
#         open_tags_starts  <- open_tags[[1]][,1]
#         open_tags_stops   <- open_tags[[1]][,2]
#         close_tags_starts <- close_tags[[1]][,1]
#         close_tags_stops  <- close_tags[[1]][,2]
#         num_matches       <- length(open_tags_starts)
#         
#         new_text <- xx
#         offset <- 0
#         for (i in seq_len(num_matches)) {
#           new_text <- paste0(
#             substr(new_text, 0, open_tags_starts[i] - 1 + offset),
#             "````",
#             substr(new_text, open_tags_starts[i] + offset, open_tags_stops[i] + offset),
#             "````",
#             substr(new_text, open_tags_stops[i] + 1 + offset, nchar(new_text))
#           )
#           offset <- offset + 8
#           
#           new_text <- paste0(
#             substr(new_text, 0, close_tags_starts[i] - 1 + offset),
#             "````",
#             substr(new_text, close_tags_starts[i] + offset, close_tags_stops[i] + offset),
#             "````",
#             substr(new_text, close_tags_stops[i] + 1 + offset, nchar(new_text))
#           )
#           offset <- offset + 8
#         }
#         
#         new_text
#       }
#     }
#   )
# )

all_md <- map2(all_md, md_files, \(x, y) c(y, "", x, "")) # Add marker to md file
# all_md <- reduce(reduce(all_md, c), c)                    # All content in one vector
all_md <- reduce(all_md, c)                               # All content in one vector

writeLines(all_md, file.path(rv_dir, rmd_file))


# Create Word document ----------------------------------------------------

render(
  file.path(rv_dir, rmd_file),
  rdocx_document(
    reference_docx = file.path(styles_dir, styles_doc),
    toc = FALSE,
    number_sections = FALSE
  ),
  output_dir = rv_dir,
  output_file = docx_file,
  quiet = TRUE
)


# Tidy up -----------------------------------------------------------------

unlink(file.path(styles_dir, styles_doc)) # Delete style doc
unlink(file.path(rv_dir, rmd_file))       # Delete combined rmd
