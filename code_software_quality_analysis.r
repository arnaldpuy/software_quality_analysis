## ----setup, include=FALSE----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "pdf", cache = TRUE)


## ----warning=FALSE, message=FALSE--------------------------------------------------------------------------

#   PRELIMINARY FUNCTIONS ######################################################

sensobol::load_packages(c("data.table", "tidyverse", "openxlsx", "scales", 
                          "cowplot", "readxl", "tidytext"))

# Create custom theme -----------------------------------------------------------

theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent",
                                           color = NA),
          legend.key = element_rect(fill = "transparent",
                                    color = NA), 
          strip.background = element_rect(fill = "white"), 
          legend.text = element_text(size = 7.3), 
          axis.title = element_text(size = 10),
          legend.key.width = unit(0.4, "cm"), 
          legend.key.height = unit(0.4, "cm"), 
          legend.key.spacing.y = unit(0, "lines"),
          legend.box.spacing = unit(0, "pt"),
          legend.title = element_text(size = 7.3), 
          axis.text.x = element_text(size = 7), 
          axis.text.y = element_text(size = 7), 
          axis.title.x = element_text(size = 7.3), 
          axis.title.y = element_text(size = 7.3),
          plot.title = element_text(size = 8),
          strip.text.x = element_text(size = 7.4), 
          strip.text.y = element_text(size = 7.4)) 
}

# Select color palette ----------------------------------------------------------

color_languages <- c("fortran" = "steelblue", "python" = "lightgreen")


## ----dataset-----------------------------------------------------------------------------------------------

# READ IN DATASET ##############################################################

# Get name of sheets -----------------------------------------------------------

sheets <- excel_sheets("./datasets/results_sqa.xlsx")

# Read all sheets --------------------------------------------------------------

dt <- lapply(sheets, function(x) data.table(read_excel("./datasets/results_sqa.xlsx", 
                                                       sheet = x)))
names(dt) <- sheets

# DESCRIPTIVE STATS SHEET ######################################################

## ----plot_lines_code, dependson="dataset", fig.height=1.5, fig.width=1.8-----------------------------------

# PLOT LINES OF CODE ###########################################################

plot_lines_code <- dt$descriptive_stats[, .(total_lines_code = sum(lines_code)), model] %>%
  ggplot(., aes(total_lines_code)) +
  geom_histogram() +
  labs(x = "Lines of code", y = "Nº models") +
  theme_AP()

plot_lines_code


## ----plot_comment_density, dependson="dataset", fig.height=1.5, fig.width=3--------------------------------

# PLOT COMMENT DENSITY #########################################################

plot_comment_density <- dt$descriptive_stats[, .(total_lines_code = sum(lines_code), 
       total_lines_comments = sum(lines_comments)), .(model, language)] %>%
  .[, comment_density:= total_lines_comments / total_lines_code] %>%
  ggplot(., aes(comment_density, fill = language)) + 
  geom_histogram() +
  facet_wrap(~language) +
  scale_y_continuous(breaks = breaks_pretty(n = 3)) +
  scale_fill_manual(values = color_languages) +
  labs(x = "Comment density", y = "Nº models") +
  theme_AP() + 
  theme(legend.position = "none")

plot_comment_density


## ----merge_plots_lines_comment_density, dependson=c("plot_lines_code", "plot_comment_density"),fig.height=1.5, fig.width=4.5----

# MERGE PLOTS ##################################################################

plot_grid(plot_lines_code, plot_comment_density + labs(x = "Comment density", y  = ""), 
          labels = "auto", rel_widths = c(0.4, 0.6))

model_ordered <- dt$descriptive_stats[, sum(lines), model] %>%
  .[order(V1)] %>%
  .[, model]

col_names <- colnames(dt$descriptive_stats)

facet_order <- c("lines", "lines_code", "lines_comments", "functions", 
                 "lines_function", "files", "modules")

melt(dt$descriptive_stats, measure.vars = col_names[-c(1, length(col_names))]) %>%
  .[, variable:= factor(variable, levels = facet_order)] %>%
  .[, model:= factor(model, levels = model_ordered)] %>%
  .[!variable == "lines"] %>%
  ggplot(., aes(model, value, fill = language)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(breaks = breaks_pretty(n = 2)) +
  scale_fill_manual(values = color_languages) +
  facet_wrap(~ variable, ncol = 7, scales = "free_x") +
  labs(x = "", y = "N") +
  theme_AP() +
  theme(legend.position = "none")




library(ggrepel)
dt$maintainability_index %>% 
  melt(., measure.vars = c("M_loc", "M_average")) %>%
  ggplot(., aes(model, value, color = language, shape = type)) +
  geom_point() +
  facet_wrap(~variable,
             labeller = as_labeller(c("M_loc" = expression(M[LOC]),
                                      "M_average" = expression(M[average])))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 65,
           fill = "red", alpha = 0.18) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 65, ymax = 85,
           fill = "orange", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 85, ymax = Inf,
           fill = "green", alpha = 0.1) +
  labs(x = "", y = "Value") +
  scale_color_manual(values = color_languages, guide = "none") +
  theme_AP() +
  coord_flip()
  
  
  
  
  
  
  ggplot(aes(M_loc, M_average, color = language, label = model)) +
  geom_point(size = 1.75) +
  geom_text_repel(aes(label = model), size = 2) +
  scale_color_manual(values = color_languages) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  scale_y_continuous(breaks = breaks_pretty(n = 3)) +
  facet_wrap(~type) +
  theme_AP() + 
  theme(legend.position = "none")

dt$score %>%
  ggplot(., aes(reorder(model, score), score, fill = language)) +
  geom_bar(stat = "identity") +
  facet_wrap(~language, scales = "free_y") +
  scale_fill_manual(values = color_languages) +
  labs(x = "", y = "Score") +
  coord_flip() +
  theme_AP() + 
  theme(legend.position = "none")



dt$score %>%
  ggplot(aes(x = reorder_within(model, score, language), y = score, fill = language)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ language, scales = "free_y") +
  labs(x = "", y = "Score") +
  scale_fill_manual(values = color_languages) +
  scale_x_reordered() +   # important: cleans up the labels
  coord_flip() +
  scale_y_continuous(limits = c(0, 10)) +
  theme_AP() + 
  theme(legend.position = "none")

