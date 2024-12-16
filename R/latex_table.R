df_to_latex <- function(df, caption = NULL, label = NULL, align = "l", digits = NULL, table_env = TRUE) {
  # Format numeric columns to the specified number of decimal places
  if (!is.null(digits)) {
    for (i in seq_along(digits)) {
      if (i <= ncol(df) && is.numeric(df[[i]])) {
        df[[i]] <- format(round(df[[i]], digits[i]), nsmall = digits[i])
      }
    }
  }

  # Begin building the LaTeX table
  latex_table <- ""

  # Add table environment if requested
  if (table_env) {
    latex_table <- paste0(latex_table, "\\begin{table}[ht]
")
  }

  # Add caption if provided
  if (!is.null(caption)) {
    latex_table <- paste0(latex_table, "\\caption{", caption, "}\n")
  }

  # Add label if provided
  if (!is.null(label)) {
    latex_table <- paste0(latex_table, "\\label{", label, "}\n")
  }

  # Begin tabular environment
  col_align <- paste(rep(align, ncol(df)), collapse = " ")
  latex_table <- paste0(latex_table, "\\begin{tabular}{", col_align, "}\n")
  latex_table <- paste0(latex_table, "\\hline\n")

  # Add column names
  header <- paste(names(df), collapse = " & ")
  latex_table <- paste0(latex_table, header, " \\\\ \n\\hline\n")

  # Add rows
  for (i in 1:nrow(df)) {
    row <- paste(df[i, ], collapse = " & ")
    latex_table <- paste0(latex_table, row, " \\\\ \n")
  }

  # End tabular environment
  latex_table <- paste0(latex_table, "\\hline\n\\end{tabular}\n")

  # End table environment if requested
  if (table_env) {
    latex_table <- paste0(latex_table, "\\end{table}\n")
  }

  return(latex_table)
}
