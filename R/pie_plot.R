#' Simple Pie Plot
#'
#' @param df Data frame containing factor variable to plot.
#' @param var_name Factor variable name.
#' @param main_title Optional.
#' @param legend_title Optional.
#' @param label_size Optional, numeric. Size of the text label.
#' @param palette Optional. See R Color Brewer’s palettes.
#'
#' @returns A simple pie chart where the labels correspond to the percentage of each category.
#' @export
#'
#' @examples my_data=data.frame(category = c("A", "B", "A", "C", "B", "B", "C", "A"))
#' pie_plot(my_data,"category")

pie_plot = function(df, var_name, main_title = NULL, legend_title = NULL,
                     label_size = NULL, palette = NULL) {
  # Check if the variable exists in the data.frame
  if (!var_name %in% names(df)) {
    stop("The variable '", var_name, "' does not exist in the data.frame.")
  }

  # Convert the variable name into a symbol
  variable_name_sym <- rlang::sym(var_name)

  # Create the table for positioning
  table = df %>%
    group_by(!!variable_name_sym) %>%
    summarise(count = n()) %>%
    mutate(percent = count / sum(count) * 100) %>%
    mutate(csum = rev(cumsum(rev(percent))),
           pos = percent / 2 + lead(csum, 1),
           pos = if_else(is.na(pos), percent / 2, pos))

  # Pie graph
  p = ggplot(table, aes(x = "", y = percent, fill = fct_inorder(!!variable_name_sym))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    ggrepel::geom_label_repel(data = table,
                              aes(y = pos, label = paste0(round(percent), "%")),
                              size = label_size %||% 4, nudge_x = 1, show.legend = FALSE) +
    theme_void()

  # Add fill color palette if provided
  if (!is.null(palette)) {
    p = p + scale_fill_brewer(palette = palette)
  } else {
    p = p + scale_fill_brewer(palette = "Pastel1")  # Default palette
  }

  # Add main title if provided
  if (!is.null(main_title)) {
    p = p + labs(title = main_title)
  }

  # Add legend title if provided
  if (!is.null(legend_title)) {
    p = p + guides(fill = guide_legend(title = legend_title))
  } else {
    p = p + guides(fill = guide_legend(title = ""))  # Empty legend title if not provided
  }

  # Return the plot
  return(p)
}
