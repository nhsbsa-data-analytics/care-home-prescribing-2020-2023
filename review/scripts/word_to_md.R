library(officer)
library(purrr)
library(dplyr)
library(tidyr)
library(xml2)

# Setup -------------------------------------------------------------------

md_dir     <- "inst/markdown" # Markdown dir
rv_dir     <- "review"        # Review directory
docx_file  <- "review.docx"   # Input Word doc
md_out_dir <- "review/temp"   # Output markdown dir


# Get Word doc contents ---------------------------------------------------

doc <- read_docx(file.path(rv_dir, docx_file))
doc_df <- docx_summary(doc)


# Get style data ----------------------------------------------------------

# We need to create maps for each type of styling or hyperlink.
# These will be applied to the markdown generated from the Word doc
style_map <- function(doc, t1, t2 = NULL, t3 = NULL) {
  smap         <- list()
  num_blank    <- 0
  num_elements <- length(doc)
  
  for (i in 1:num_elements) {
    doc$officer_cursor$which <- i
    num_chars <- 0
    parent <- docx_current_block_xml(doc)
    first_children <- xml_children(parent)
    if(xml_text(parent) == "") num_blank <- num_blank + 1
    for (first_child in first_children) {
      num_chars <- num_chars + nchar(xml_text(first_child))
      if (!xml_name(first_child) == t1) next
      second_children <- xml_children(first_child)
      for (second_child in second_children) {
        if (!is.null(t2)) {
          if (!xml_name(second_child) == t2) next
        }
        third_children <- xml_children(second_child)
        if (!is.null(t3)) {
          if (!(t3 %in% xml_name(third_children))) next
        }
        style_data <- list(
          num_chars - nchar(xml_text(first_child)) + 1,
          num_chars
        )
        xml_attr_names <- xml_attrs(first_child) %>% names()
        if (length(xml_attr_names) & "id" %in% xml_attr_names) {
          style_data <- c(style_data, list(xml_attrs(first_child)[["id"]]))
        }
        if (as.character(i - num_blank) %in% names(smap)) {
          new_index <- as.character(length(smap[[as.character(i - num_blank)]]) + 1)
          smap[[as.character(i - num_blank)]][[new_index]] <- style_data
        } else {
          smap[[as.character(i - num_blank)]] <- list(
            `1` = style_data
          )
        }
      }
    }
  }
  
  smap %>% map(unique)
}

bold_map <- style_map(doc, "r", "rPr", "b")
ital_map <- style_map(doc, "r", "rPr", "i")
hypl_map <- style_map(doc, "hyperlink")


# Compute file breakpoints ------------------------------------------------

# This will find the rows to use for each md file
breaks <- doc_df %>%
  filter(startsWith(text, md_dir)) %>%
  mutate(
    md_file = text,
    begin   = doc_index + 1,
    end     = lead(doc_index) - 1,
    .keep   = "none"
  ) %>%
  replace_na(list(end = nrow(doc_df)))


# Create markdown files ---------------------------------------------------

# Iterate over the markdown filenames. The content for each file is transformed
# to apply styling and hyperlinks and then written to md_out_dir
pwalk(
  breaks,
  \(md_file, begin, end) {
    # Each file has content from row number start to end
    doc_df <- doc_df %>%
      filter(between(doc_index, begin, end))

    # Apply any bold styling
    for (row in names(bold_map)) {
      offset <- 0
      for (style_data in bold_map[[row]]) {
        row    <- as.integer(row)
        start  <- style_data[[1]] + offset
        stop   <- style_data[[2]] + offset

        bold_applied <- doc_df %>% 
          filter(doc_index == row) %>% 
          mutate(
            text = paste0(
              substr(text, 0, start - 1),
              "__",
              substr(text, start, stop),
              "__",
              substr(text, stop + 1, nchar(text))
            )
          ) %>% 
          pull(text)
        
        doc_df <- doc_df %>% 
          mutate(
            text = replace(
              text, 
              doc_index == row, 
              bold_applied
            )
          )
        
        offset <- offset + 4
      }
    }
    
    # Apply any italics styling
    for (row in names(ital_map)) {
      offset <- 0
      for (style_data in ital_map[[row]]) {
        row    <- as.integer(row)
        start  <- style_data[[1]] + offset
        stop   <- style_data[[2]] + offset
        
        ital_applied <- doc_df %>% 
          filter(doc_index == row) %>% 
          mutate(
            text = paste0(
              substr(text, 0, start - 1),
              "_",
              substr(text, start, stop),
              "_",
              substr(text, stop + 1, nchar(text))
            )
          ) %>% 
          pull(text)
        
        doc_df <- doc_df %>% 
          mutate(
            text = replace(
              text, 
              doc_index == row, 
              ital_applied
            )
          )
        
        offset <- offset + 2
      }
    }
    
    # Add any hyperlinks
    for (row in names(hypl_map)) {
      offset <- 0
      for (style_data in hypl_map[[row]]) {
        row    <- as.integer(row)
        start  <- style_data[[1]] + offset
        stop   <- style_data[[2]] + offset
        invisible(capture.output(
          url <- doc$
            doc_obj$
            relationship()$
            show() %>%
            as_tibble() %>%
            filter(id == style_data[[3]]) %>%
            pull(target)
        ))

        hypl_applied <- doc_df %>%
          filter(doc_index == row) %>%
          mutate(
            text = paste0(
              substr(text, 0, start - 1),
              "[",
              substr(text, start, stop),
              "](",
              url,
              ")",
              substr(text, stop + 1, nchar(text))
            )
          ) %>%
          pull(text)

        doc_df <- doc_df %>%
          mutate(
            text = replace(
              text,
              doc_index == row,
              hypl_applied
            )
          )
        
        offset <- offset + 4 + nchar(url)
      }
    }
    
    # Add heading and bullet markup and find where to add blank lines
    doc_df <- doc_df %>%
      mutate(
        text = case_match(
          style_name,
          "Heading 2" ~ paste0("## ", text),
          "Heading 3" ~ paste0("### ", text),
          "Heading 4" ~ paste0("#### ", text),
          "Compact"   ~ paste0("- ", text),
          .default    = text
        ),
        next_style = lead(style_name),
        blank_after = (
          (style_name != "Compact") |
            (style_name == "Compact" & lead(style_name) != "Compact")
        ) &
          (!is.na(lead(style_name)))
      )

    # Get the element indices which need a blank line afterward
    needs_blank_after <- which(doc_df$blank_after) +
      seq_len(length(which(doc_df$blank_after))) - 1

    # Iterate over the indices and add a new blank row after each
    walk(needs_blank_after, \(x) doc_df <<- add_row(doc_df, text = "", .after = x))

    # Write the markdown file
    writeLines(doc_df$text, file.path(md_out_dir, basename(md_file)))
  }
)
