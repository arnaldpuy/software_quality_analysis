## ----setup, include=FALSE-----------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "pdf", cache = TRUE)


## ----warning=FALSE, message=FALSE---------------------------

#   PRELIMINARY FUNCTIONS ####################################################

sensobol::load_packages(c("data.table", "tidyverse", "openxlsx", "scales", 
                          "cowplot", "readxl", "ggrepel", "tidytext", "here", 
                          "tidygraph", "igraph", "foreach", "parallel", "ggraph", 
                          "tools", "purrr"))

# Create custom theme -----------------------------------------------------------

theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA), 
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

# Source all .R files in the "functions" folder --------------------------------

r_functions <- list.files(path = here("functions"), 
                          pattern = "\\.R$", full.names = TRUE)

lapply(r_functions, source)


## ----dataset------------------------------------------------

# READ IN DATASET ############################################################

# Get name of sheets -----------------------------------------------------------

sheets <- excel_sheets("./datasets/results_sqa.xlsx")

# Read all sheets --------------------------------------------------------------

dt <- lapply(sheets, function(x) data.table(read_excel("./datasets/results_sqa.xlsx", 
                                                       sheet = x)))

# Name the slots ---------------------------------------------------------------

names(dt) <- sheets


## ----plot_lines_code, dependson="dataset", fig.height=1.5, fig.width=1.8----

# PLOT LINES OF CODE ###########################################################

plot_lines_code <- dt$descriptive_stats[, .(total_lines_code = sum(lines_code)), model] %>%
  ggplot(., aes(total_lines_code)) +
  geom_histogram() +
  labs(x = "Lines of code", y = "Nº models") +
  theme_AP()

plot_lines_code


## ----plot_comment_density, dependson="dataset", fig.height=1.5, fig.width=3----

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


## ----plot_other_stats, dependson="dataset", fig.height=2.8, fig.width=6----

# PLOT PER MODEL ###############################################################

# Sort by model ----------------------------------------------------------------

model_ordered <- dt$descriptive_stats[, sum(lines), model] %>%
  .[order(V1)] 

# Print ------------------------------------------------------------------------

model_ordered

# Extract column names ---------------------------------------------------------

col_names <- colnames(dt$descriptive_stats)

# Order facets -----------------------------------------------------------------

facet_order <- c("lines", "lines_code", "lines_comments", "functions", 
                 "lines_function", "files", "modules")


# Plot -------------------------------------------------------------------------

plot_per_model <- melt(dt$descriptive_stats, measure.vars = col_names[-c(1, length(col_names))]) %>%
  .[, variable:= factor(variable, levels = facet_order)] %>%
  .[, model:= factor(model, levels = model_ordered[, model])] %>%
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

plot_per_model


## ----merge_plots_lines_comment_density, dependson=c("plot_lines_code", "plot_comment_density", "plot_other_stats"),fig.height=3.5, fig.width=5.8----

# MERGE PLOTS ##################################################################

top <- plot_grid(plot_lines_code, plot_comment_density + labs(x = "Comment density", y  = ""), 
          labels = "auto", rel_widths = c(0.4, 0.6))

p1 <- plot_grid(top, plot_per_model, ncol = 1, labels = c("", "c"), rel_heights = c(0.4, 0.6))

p1


## ----maintainability_index_interpretation, dependson="dataset"----

# CALCULATE INTERPRETATIBILITY OF MAINTAINABILITY INDEX 3#######################

# Define vector of interpretation ----------------------------------------------

vec_interpretation <- c("low", "moderate", "high")

# Calculate --------------------------------------------------------------------

dt$maintainability_index %>%
  melt(., measure.vars = c("M_loc", "M_average")) %>%
  .[, interpretativity:= ifelse(value > 85, vec_interpretation[3], 
                                ifelse(value <=85 & value >= 65, vec_interpretation[2], 
                                       vec_interpretation[1]))] %>%
  .[, .N, .(language, interpretativity, variable)] %>%
  dcast(., variable + language ~ interpretativity, value.var = "N") %>%
  .[, total:= rowSums(.SD, na.rm = TRUE), .SDcols = vec_interpretation] %>%
  .[, paste(vec_interpretation, "prop", sep = "_"):= lapply(.SD, function(x) 
    x / total), .SDcols = vec_interpretation] %>%
  print()


## ----maintainability_index, dependson="dataset", fig.height=2.5, fig.width=4----

# PLOT MAINTAINABILITY INDEX ###################################################

plot_maintainability_index <- dt$maintainability_index %>% 
  melt(., measure.vars = c("M_loc", "M_average")) %>%
  .[, variable:= factor(variable, levels = c("M_average", "M_loc"))] %>%
  ggplot(., aes(model, value, color = language, shape = type)) +
  geom_point() +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 65,
           fill = "red", alpha = 0.18) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 65, ymax = 85,
           fill = "orange", alpha = 0.1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 85, ymax = Inf,
           fill = "green", alpha = 0.1) +
  facet_wrap(~variable, labeller = as_labeller(c(M_loc = "M[LOC]", 
                                               M_average = "M[average]"),
                                             default = label_parsed)) +
  labs(x = "", y = "Value") +
  scale_color_manual(values = color_languages, guide = "none") +
  theme_AP() +
  theme(legend.position = "none") +
  coord_flip()

plot_maintainability_index


## ----score, dependson="dataset", fig.height=2, fig.width=3.5----

# PLOT SCORE ###################################################################

plot_score <- dt$score %>%
  ggplot(aes(x = reorder_within(model, score, language), y = score, fill = language)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ language, scales = "free_y") +
  labs(x = "", y = "Score") +
  scale_fill_manual(values = color_languages) +
  geom_hline(yintercept = 5, lty = 2) +
  scale_x_reordered() +   
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10)) +
  coord_flip() +
  theme_AP() + 
  theme(legend.position = "none")

plot_score


## ----merge_maintainability_score, dependson=c("score", "maintainability_index"), fig.width=6, fig.height=5----

# MERGE PLOTS ##################################################################

bottom <- plot_grid(plot_maintainability_index, plot_score, ncol = 2, labels = c("d", "e"))

plot_grid(p1, bottom, ncol = 1, rel_heights = c(0.62, 0.38))


## ----read_metrics_function_data-----------------------------

# METRICS AT THE FILE AND FUNCTION LEVEL #######################################

folder <- "./datasets/results_function"

# Get names of files -----------------------------------------------------------

csv_files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)

# Split into file_metrics and func_metrics -------------------------------------

file_metric_files <- grep("file_metrics", csv_files, value = TRUE)
func_metric_files <- grep("func_metrics", csv_files, value = TRUE)

# Build one named list ---------------------------------------------------------

list_metrics <- list(file_metrics = setNames(lapply(file_metric_files, fread), 
                                             basename(file_metric_files)),
                     func_metrics = setNames(lapply(func_metric_files, fread), 
                                             basename(func_metric_files)))

# Create function to combine files ---------------------------------------------

make_combined <- function(subset_list, pattern) {
  rbindlist(subset_list[grep(pattern, names(subset_list))], idcol = "source_file")
}

# Combine files ----------------------------------------------------------------

metrics_combined <- list(file_fortran = make_combined(list_metrics$file_metrics,  "fortran"),
                         file_python = make_combined(list_metrics$file_metrics,  "python"),
                         func_fortran = make_combined(list_metrics$func_metrics, "fortran"),
                         func_python = make_combined(list_metrics$func_metrics, "python"))

# Functions to extract name of model and language from file --------------------

extract_model <- function(x) 
  sub("^(file|func)_metrics_\\d+_([A-Za-z0-9-]+)_(fortran|python).*", "\\2", x)

extract_lang  <- function(x) 
  sub("^(file|func)_metrics_\\d+_([A-Za-z0-9-]+)_(fortran|python).*", "\\3", x)

# Extract name of model and language -------------------------------------------

metrics_combined <- lapply(metrics_combined, function(dt) {
  dt[, source_file:= sub("\\.csv$", "", basename(source_file))]
  dt[, model:= extract_model(source_file)]
  dt[, language:= extract_lang(source_file)]
  dt
})

# Add column of complexity category --------------------------------------------

metrics_combined <- lapply(names(metrics_combined), function(nm) {
  dt <- as.data.table(metrics_combined[[nm]])
  if (grepl("^func_", nm) && "cyclomatic_complexity" %in% names(dt)) {
    dt[, complexity_category := cut(
      cyclomatic_complexity,
      breaks = c(-Inf, 10, 20, 50, Inf),
      labels = c("b1","b2","b3","b4")
    )]
  }
  dt
}) |> setNames(names(metrics_combined))

# Define labels ----------------------------------------------------------------

lab_expr <- c(
  b1 = expression(C %in% "(" * 0 * ", 10" * "]"),
  b2 = expression(C %in% "(" * 10 * ", 20" * "]"),
  b3 = expression(C %in% "(" * 20 * ", 50" * "]"),
  b4 = expression(C %in% "(" * 50 * ", " * infinity * ")")
)

# EXPORT DATA TO .CSV ##########################################################

# set output folder inside "datasets" ------------------------------------------

outdir <- file.path("datasets", "merged_results")

# write each slot to its own CSV -----------------------------------------------

lapply(names(metrics_combined), function(nm) {
  out_file <- file.path(outdir, paste0(nm, ".csv"))
  fwrite(metrics_combined[[nm]], out_file)
})


## ----plot_cyclomatic_model, dependson="read_metrics_function_data", fig.height=5.5, fig.width=6----

# PLOT #########################################################################

# Cyclomatic complexity at the model level -------------------------------------

metrics_combined[grep("^func_", names(metrics_combined))] %>%
  lapply(., function(x) x[, .(cyclomatic_complexity, model, language)]) %>%
  rbindlist() %>%
  ggplot(., aes(cyclomatic_complexity)) +
  geom_histogram() +
  annotate("rect",
           xmin = 11, xmax = 20,
           ymin = -Inf, ymax = Inf,
           fill = "orange", alpha = 0.2) +
  annotate("rect",
           xmin = 21, xmax = 50,
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  annotate("rect",
           xmin = 51, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = "purple", alpha = 0.2) +
  facet_wrap(model ~ language, scales = "free") +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  scale_y_continuous(breaks = breaks_pretty(n = 2)) +
  labs(x = "Cyclomatic complexity", y = "Nº") +
  theme_AP()


## ----plot_scatterplot, dependson="read_metrics_function_data", fig.height=1.7, fig.width=2.5----

plot_scatterplot <- metrics_combined[grep("^func_", names(metrics_combined))] %>%
  lapply(., function(x) 
    x[, .(model, language, `function`, cyclomatic_complexity, loc, bugs, type)]) %>%
  rbindlist() %>%
  .[type == "FUNCTION"] %>%
  ggplot(., aes(cyclomatic_complexity, loc, color = language)) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = color_languages) +
  coord_flip() +
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  facet_wrap(~language) +
  labs(x = "C", y = "Lines of code") +
  theme_AP() +
  theme(legend.position = "none")

plot_scatterplot


## ----plot_c_model, dependson="read_metrics_function_data", fig.height=2.2, fig.width=3.1----

plot_c_model <- metrics_combined[grep("^func_", names(metrics_combined))] %>%
  lapply(., function(x) 
    x[, .(model, language, `function`, cyclomatic_complexity, loc, bugs, type)]) %>%
  rbindlist() %>%
  ggplot(., aes(model, cyclomatic_complexity, fill = language, color = language)) +
  geom_boxplot(outlier.size = 1) +
  coord_flip() +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 2)) +
  facet_wrap(~language) +
  labs(x = "", y = "C") +
  theme_AP() +
  scale_color_manual(values = color_languages) +
  theme(legend.position = "none")

plot_c_model


## ----plot_scatter_and_bar, dependson="read_metrics_function_data", fig.height=2.5, fig.width=3----

# Scatterplot cyclomatic vs lines of code --------------------------------------

plot_c_vs_loc <- metrics_combined[grep("^func_", names(metrics_combined))] %>%
  lapply(., function(x) x[, .(loc, cyclomatic_complexity, language)]) %>%
  rbindlist() %>%
  ggplot(., aes(loc, cyclomatic_complexity, color = language)) +
  geom_point(alpha = 0.5, size = 0.7) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  labs(x = "Lines of code", y = "C") +
  scale_color_manual(values = color_languages) +
  theme_AP() + 
  theme(legend.position = "none")

plot_c_vs_loc

# Count & proportion -----------------------------------------------------------

plot_bar_cyclomatic <- metrics_combined[grep("^func_", names(metrics_combined))] %>%
  lapply(., function(x) x[, .(complexity_category, language, type)]) %>%
  rbindlist() %>%
  .[type == "FUNCTION"] %>%
  .[, .N, .(complexity_category, language)] %>%
  .[, proportion := N / sum(N), language] %>%
  ggplot(., aes(complexity_category, proportion, fill = language)) +
  geom_bar(stat = "identity", position = position_dodge(0.6)) +
  scale_fill_manual(values = color_languages) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 4)) +
  scale_x_discrete(labels = lab_expr) +
  labs(x = "", y = "Proportion") +
  coord_flip() +
  theme_AP() +
  theme(legend.position = "none")

plot_bar_cyclomatic


## ----merge_scatter, dependson="plot_scatter_and_bar", fig.height=1.8, fig.width=4----

# MERGE ########################################################################


 plot_cyclomatic <- plot_grid(plot_c_vs_loc, plot_bar_cyclomatic, ncol = 2, labels = "auto", 
          rel_widths = c(0.45, 0.55))
 
plot_cyclomatic



## ----plot_c_fraction, fig.height=2, fig.width=2.3-----------

plot_bar_category <- metrics_combined[grep("^func_", names(metrics_combined))] %>%
  lapply(., function(x) 
    x[, .(model, language, complexity_category, type)]) %>%
  rbindlist() %>%
  .[type %in% c("SUBROUTINE", "FUNCTION")] %>%
  .[, .N, .(model, language, complexity_category)] %>%
  .[, proportion := N / sum(N), .(language, model)] %>%
  ggplot(., aes(model, proportion, fill = complexity_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("yellowgreen", "orange", "red", "purple"), 
                    labels = lab_expr, 
                    name = "") +
  facet_wrap(~language) + 
  labs(x = "", y = "Proportion") +
  coord_flip() +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_AP() + 
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank(), 
        legend.text = element_text(size = 7))

plot_bar_category


## ----merge_5, dependson=c("plot_c_model", "plot_c_fraction", "merge_plots_lines_comment_density", "merge_scatter"), fig.height=7.2, fig.width=6----

di <- plot_grid(plot_scatterplot, plot_bar_cyclomatic, ncol = 1, labels = c("d", "e"))
legend <- get_legend_fun(plot_bar_category + theme(legend.position = "top"))
dada <- plot_grid(plot_c_model, plot_bar_category, ncol = 2, rel_widths = c(0.61, 0.39))
dada2 <- plot_grid(legend, dada, ncol = 1, rel_heights = c(0.1, 0.9), labels = "f")
dada3 <- plot_grid(di, dada2, ncol = 2, rel_widths = c(0.4, 0.6))
dada4 <- plot_grid(plot_maintainability_index, plot_score, ncol = 2, labels = c("g", "h"))
dada5 <- plot_grid(p1, dada3, ncol = 1, rel_heights = c(0.6, 0.4))
plot_grid(dada5, dada4, rel_heights = c(0.73, 0.27), ncol = 1)


###############################
###############################
###############################

# NETWORK CITATION ANALYSIS ----------------------------------------------------

# Path to folder ---------------------------------------------------------------

path <- "./datasets/call_metrics"

# List CSV files ---------------------------------------------------------------

files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Split by language ------------------------------------------------------------

python_files  <- grep("python",  files, value = TRUE, ignore.case = TRUE)
fortran_files <- grep("fortran", files, value = TRUE, ignore.case = TRUE)

base_fortran <- file_path_sans_ext(basename(fortran_files))
base_python <- file_path_sans_ext(basename(python_files))

model_names_fortran <- models <- sub(".*_", "", base_fortran)
model_names_python <- models <- sub(".*_", "", base_python)

# Load and name files ----------------------------------------------------------

python_list  <- lapply(python_files, fread)
fortran_list <- lapply(fortran_files, fread)

names(python_list) <- model_names_python
names(fortran_list) <- model_names_fortran

# RBIND ------------------------------------------------------------------------

make_callgraph <- function(lst, lang) {
  rbindlist(lst, idcol = "model") %>%
    .[, language := lang] %>%
    .[, .(model, language, `function`, call)] %>%
    setnames(., c("function", "call"), c("from", "to"))
}

python_callgraphs  <- make_callgraph(python_list,  "python") 
fortran_callgraphs <- make_callgraph(fortran_list, "fortran")

all_callgraphs <- rbind(python_callgraphs, fortran_callgraphs)

# CREATE THE NETWORK ###########################################################

# Define the weights to characterize risky nodes -------------------------------

alpha <- 0.6  # Weight to cyclomatic complexity
beta  <- 0.3  # Weight to in-degree (impact of bug upstream)
gamma <- 0.1  # Weight to betweenness (critical bridge)

# Ensure unique names complexity dataset ---------------------------------------

cc_unique <- metrics_combined[grep("^func_", names(metrics_combined))] %>%
  lapply(., function(x) 
    x[, .(model, language, `function`, cyclomatic_complexity, complexity_category, type)]) %>%
  rbindlist() %>%
  setnames(., "function", "name") %>%
  arrange(desc(cyclomatic_complexity)) %>%
  distinct(name, .keep_all = TRUE)

# CREATE NETWORKS FROM CALL GRAPHS #############################################

all_graphs <- all_callgraphs[, .(graph = list(as_tbl_graph(.SD, directed = TRUE))), .(model, language)] %>%
  .[, graph:= Map(function(g, m, lang) {
    
  comp_sub <- cc_unique[model == m & language == lang]
  
  g %>%
    activate(nodes) %>%
    
    # Left join with dataset with cyclomatic complexity values -----------------
  
  left_join(comp_sub, by = "name") %>%
    
    # Remove Python MODULE_AGG / CLASS_AGG nodes from this graph 
    # because they are not callable --------------------------------------------
  
  filter(!(language == "python" & type %in% c("MODULE_AGG", "CLASS_AGG"))) %>%
    
    # Calculation of key network metrics ---------------------------------------
  
    mutate(indeg = centrality_degree(mode = "in"),
           outdeg = centrality_degree(mode = "out"),
           btw = centrality_betweenness(directed = TRUE, weights = NULL),
           cyclo_scaled = rescale(cyclomatic_complexity),
           indeg_scaled = rescale(indeg),
           btw_scaled = rescale(btw),
           risk_node = alpha * cyclo_scaled + beta * indeg_scaled + gamma * btw_scaled
    )},
  
    graph, model, language
  )
]

# COMPUTE ALL PATHS AND THEIR RISK SCORES ######################################

all_graphs[, paths_tbl:= lapply(graph, all_paths_fun)]

# PLOT ALL CALLGRAPHS ##########################################################

all_graphs[, plot_obj := mapply(plot_top_risky_paths_fun, call_g = graph, 
                                paths_tbl = paths_tbl, model.name = model,
                                language   = language, SIMPLIFY = FALSE)]









# Step 1: Extract nodes from each tbl_graph
all_graphs[, nodes := lapply(graph, function(g) {
  g %>% activate(nodes) %>% as_tibble()
})]

# Step 2: Unnest nodes into long format
nodes_long <- all_graphs %>%
  .[, ID:= .I] %>%
  .[, .(ID, nodes)] %>%
  tidyr::unnest(nodes) %>%
  data.table()

nodes_long[, .(max = max(indeg, na.rm = TRUE), 
                min = min(indeg, na.rm = TRUE)), .(model, language)]










library(tidygraph)
library(ggraph)
library(dplyr)
library(purrr)
library(tibble)

plot_top_risky_paths <- function(call_g, paths_tbl, model.name = "") {
  # paths_tbl must already contain: path_nodes, p_path_fail, risk_sum, indeg in call_g
  
  # If there are no paths, nothing to plot
  if (nrow(paths_tbl) == 0) {
    message("No paths in paths_tbl; skipping plot for: ", model.name)
    return(invisible(NULL))
  }
  
  # ---- Top 10 most risky paths (by p_path_fail) -----------------------------
  top_paths <- paths_tbl %>% 
    arrange(desc(p_path_fail)) %>% 
    slice_head(n = 10)
  
  print(top_paths)
  
  k <- min(10, nrow(top_paths))
  top_k_paths <- top_paths %>% 
    slice_head(n = k)
  
  # ---- Build edge list for those paths --------------------------------------
  path_edges_all <- purrr::imap_dfr(top_k_paths$path_nodes, function(nodes_vec, pid) {
    tibble(
      from    = head(nodes_vec, -1),
      to      = tail(nodes_vec, -1),
      path_id = pid,
      risk_sum = top_k_paths$risk_sum[pid]
    )
  })
  
  ig2 <- as.igraph(call_g)
  edge_df_names <- igraph::as_data_frame(ig2, what = "edges") %>%
    dplyr::mutate(.edge_idx = dplyr::row_number())
  
  # ---- Collapse duplicate edges across paths --------------------------------
  path_edges_collapsed <- path_edges_all %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarise(
      path_freq      = dplyr::n(),
      risk_mean_path = mean(risk_sum, na.rm = TRUE),
      .groups        = "drop"
    )
  
  # ---- Join with all edges ---------------------------------------------------
  edge_marks <- edge_df_names %>%
    dplyr::left_join(path_edges_collapsed, by = c("from", "to")) %>%
    dplyr::mutate(
      on_top_path    = !is.na(path_freq),
      path_freq      = ifelse(is.na(path_freq), 0L, path_freq),
      risk_mean_path = ifelse(is.na(risk_mean_path), 0, risk_mean_path)
    )
  
  # ---- Prepare graph for plotting -------------------------------------------
  call_g_sugi <- call_g %>%
    tidygraph::activate(edges) %>%
    dplyr::mutate(
      on_top_path    = edge_marks$on_top_path,
      path_freq      = edge_marks$path_freq,
      risk_mean_path = edge_marks$risk_mean_path
    ) %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(
      cyclo_class = dplyr::case_when(
        cyclomatic_complexity <= 10 ~ "green",
        cyclomatic_complexity <= 20 ~ "orange",
        cyclomatic_complexity <= 50 ~ "red",
        cyclomatic_complexity >  50 ~ "purple",
        TRUE                        ~ "grey"
      )
    )
  
  # Mark nodes that appear in at least one of the top-k risky paths
  risky_nodes <- unique(unlist(top_k_paths$path_nodes))
  call_g_sugi <- call_g_sugi %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(on_top_node = name %in% risky_nodes)
  
  call_g_sugi %>%
    tidygraph::activate(nodes) %>%
    data.frame() %>%
    data.table::data.table() %>%
    .[order(-indeg)]
  
  # ---- Plot ------------------------------------------------------------------
  p_sugi <- ggraph(call_g_sugi, layout = "sugiyama") +
    
    # 1) Background: non-top edges
    ggraph::geom_edge_link0(
      aes(filter = !on_top_path),
      colour = "grey80", alpha = 0.05, width = 0.3
    ) +
    
    # 2) Foreground: risky edges
    ggraph::geom_edge_link0(
      aes(
        filter = on_top_path,
        colour = risk_mean_path,
        width  = pmin(pmax(path_freq, 0.5), 3)
      ),
      alpha = 0.9,
      arrow = grid::arrow(length = grid::unit(1, "mm"))
    ) +
    ggraph::scale_edge_colour_gradient(low = "orange", high = "red", guide = "none") +
    ggraph::scale_edge_width(range = c(0.3, 2.2), guide = "none") +
    
    # Nodes
    ggraph::geom_node_point(size = 1.2, colour = "#BDBDBD", alpha = 0.35,
                            show.legend = FALSE) +
    ggraph::geom_node_point(
      aes(filter = on_top_node, size = indeg, fill = cyclo_class),
      shape = 21, alpha = 0.95, show.legend = FALSE
    ) +
    scale_fill_manual(
      values = c(
        green  = "yellowgreen",
        orange = "orange",
        red    = "red",
        purple = "purple",
        grey   = "#A0A0A0"
      ),
      guide = "none"
    ) +
    theme_AP() +
    labs(x = "", y = "") +
    theme(
      axis.text.y   = element_blank(),
      axis.ticks.y  = element_blank(),
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      legend.position = "top"
    ) +
    ggtitle(model.name)
  
  print(p_sugi)
  invisible(p_sugi)
}















# COMPUTE MORE PATH-LEVEL METRICS ##############################################

paths_tbl %>%
  mutate(max_node_risk = purrr::map_dbl(path_nodes, ~ max(get_path_risks_fun(.x), na.rm = TRUE)),
         
         # Nonlinear failure aggregation: 1 - Π (1 - p_i). Probability that at
         # least one node on the path fails (formula is "union of independent events")
         # --------------------------------------------------------------------------
         
         p_path_fail = purrr::map_dbl(path_nodes, ~ {
           r <- get_path_risks_fun(.x) 
           r <- r[is.finite(r) & !is.na(r)]
           if (length(r) == 0) 0 else 1 - prod(pmax(0, pmin(1, 1 - r)))
         }),
         
         # Inequality of risk along the path (concentration): Gini index: high Gini
         # means that few risky nodes dominate the path. Low Gini + high mean means
         # that many risk nodes (diffuse fragility) ---------------------------------
         
         gini_node_risk = purrr::map_dbl(path_nodes, ~ {
           r <- get_path_risks_fun(.x); r <- r[is.finite(r) & !is.na(r)]
           if (length(r) <= 1) 0 else ineq::Gini(r)
         }),
         
         # Trend of risk along the path (slope of linear fit over node order): positive slope
         # means risk increases downstream (dangerous propagation chain); negative slope
         # means high-risk nodes early but safer callees (lower systemic threat) ----
         # --------------------------------------------------------------------------
         
         risk_slope = purrr::map_dbl(path_nodes, ~ {
           r <- get_path_risks_fun(.x); r <- r[is.finite(r) & !is.na(r)]
           if (length(r) <= 1) 0 else as.numeric(coef(lm(r ~ seq_along(r)))[2])
         })
  )


all_graphs <- all_graphs[, all_paths:= lapply(graph, all_paths_fun)]

# COMPUTE RISK OF EACH PATH ####################################################

all_graphs <- all_graphs[, paths_tbl:= lapply(all_paths, function(ap) {
  
  # ap is a list of lists of paths; flatten to a single list of paths ----------
  
  ap_flat <- flatten(ap)   
  
  if (length(ap_flat) == 0) {
    
    # return empty tibble if no paths -----------
    
    return(tibble())
  }
  
  map_dfr(ap_flat, risk_of_path_fun)
})]


all_graphs$all_paths[[1]]










all_graphs$paths_tbl


all_graphs[, pairs_st := lapply(graph, pairs_st_fun)]


all_graphs$pairs_st






as_tbl_graph(all_callgraphs, directed = TRUE) %>%
  left_join(cc_unique %>%
              select(name, type, language, cyclomatic_complexity, complexity_category), 
            by = "name")







model.name <- "VIC"

callgraph_edges <- fread(paste("./datasets/callgraphs/callgraph_edges_", model.name, ".csv", sep = ""))
setnames(callgraph_edges, c("caller", "callee", "lang"), c("from", "to", "language"))
setcolorder(callgraph_edges, c("from", "to", "model", "language", "caller_line", 
                               "callee_file_hint", "callee_kind"))
callgraph_edges[, model:= sub("^\\d+_", "", model)]

# Keep only the function in the column "from" if the language is python --------

callgraph_edges[language == "python" & grepl(":", from, fixed = TRUE),
   from := sub(".*:+", "", from)]

# Define the weights to characterize risky nodes -------------------------------

alpha <- 0.6  # Weight to cyclomatic complexity
beta  <- 0.3  # Weight to in-degree (impact of bug upstream)
gamma <- 0.1  # Weight to betweenness (critical bridge)

# Ensure unique names complexity dataset ---------------------------------------

cc_unique <- metrics_combined[grep("^func_", names(metrics_combined))] %>%
  lapply(., function(x) 
    x[, .(model, language, `function`, cyclomatic_complexity, complexity_category, type)]) %>%
  rbindlist() %>%
  .[model == model.name] %>%
  setnames(., "function", "name") %>%
  arrange(desc(cyclomatic_complexity)) %>%
  distinct(name, .keep_all = TRUE)

# Transform to graph -----------------------------------------------------------

call_g <- as_tbl_graph(callgraph_edges, directed = TRUE) %>%
  left_join(cc_unique %>%
              select(name, type, language, cyclomatic_complexity, complexity_category), 
            by = "name") %>%
  mutate(
    indeg = centrality_degree(mode = "in"),
    outdeg = centrality_degree(mode = "out"),
    btw = centrality_betweenness(directed = TRUE, weights = NULL),
    cyclo_scaled = rescale(cyclomatic_complexity),
    indeg_scaled = rescale(indeg),
    btw_scaled = rescale(btw),
    risk_node = alpha * cyclo_scaled + beta * indeg_scaled + gamma * btw_scaled
  )

  
# Find paths from entries (in-degree = 0) to sinks (out-degree = 0) ----------

ig <- as.igraph(call_g)
V(ig)$name <- call_g %>% activate(nodes) %>% as_tibble() %>% pull(name)
node_df <- call_g %>% activate(nodes) %>% as_tibble()

entry_ids <- which(node_df$indeg == 0) # entries
sink_ids  <- which(node_df$outdeg == 0) # sinks

# To reduce complexity of paths: we only account for connected nodes (if there
# is no path, it returns Inf) --------------------------------------------------

dist_mat <- igraph::distances(ig, v = entry_ids, to = sink_ids, mode = "out")

# Create pairs only where there is a path --------------------------------------

pairs_st <- expand.grid(s = seq_along(entry_ids), t = seq_along(sink_ids))
pairs_st <- pairs_st[pairs_st$s != pairs_st$t, , drop = FALSE]

# Filter only paths where distance is finite (there is a path) -----------------

pairs_st <- subset(pairs_st, dist_mat[cbind(s, t)] < Inf)

# Prepare cluster for parallelization ------------------------------------------

ncores <- max(1, parallel::detectCores() * 0.75)
cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)


all_paths <- foreach(i = 1:nrow(pairs_st), .combine = "c", .packages = "igraph") %dopar% {
  s <- pairs_st$s[i]
  t <- pairs_st$t[i]
  igraph::all_simple_paths(ig, from = s, to = t, mode = "out")
}

stopCluster(cl)
  
# Calculate risk of a given path -----------------------------------------------

paths_tbl <- purrr::map_dfr(all_paths, risk_of_path_fun)

paths_tbl <- paths_tbl %>%
  mutate(max_node_risk = purrr::map_dbl(path_nodes, ~ max(get_path_risks_fun(.x), na.rm = TRUE)),
    
    # Nonlinear failure aggregation: 1 - Π (1 - p_i). Probability that at
    # least one node on the path fails (formula is "union of independent events")
    # --------------------------------------------------------------------------
    
    p_path_fail = purrr::map_dbl(path_nodes, ~ {
      r <- get_path_risks_fun(.x) 
      r <- r[is.finite(r) & !is.na(r)]
      if (length(r) == 0) 0 else 1 - prod(pmax(0, pmin(1, 1 - r)))
    }),
    
    # Inequality of risk along the path (concentration): Gini index: high Gini
    # means that few risky nodes dominate the path. Low Gini + high mean means
    # that many risk nodes (diffuse fragility) ---------------------------------
    
    gini_node_risk = purrr::map_dbl(path_nodes, ~ {
      r <- get_path_risks_fun(.x); r <- r[is.finite(r) & !is.na(r)]
      if (length(r) <= 1) 0 else ineq::Gini(r)
    }),
    
    # Trend of risk along the path (slope of linear fit over node order): positive slope
    # means risk increases downstream (dangerous propagation chain); negative slope
    # means high-risk nodes early but safer callees (lower systemic threat) ----
    # --------------------------------------------------------------------------
    
    risk_slope = purrr::map_dbl(path_nodes, ~ {
      r <- get_path_risks_fun(.x); r <- r[is.finite(r) & !is.na(r)]
      if (length(r) <= 1) 0 else as.numeric(coef(lm(r ~ seq_along(r)))[2])
    })
  )


# PLOT #########################################################################

vline_df <- data.frame(variable = "risk_slope", xintercept = 0)

paths_tbl %>%
  data.table() %>%
  melt(., measure.vars = c("p_path_fail", "gini_node_risk", "risk_slope")) %>%
  ggplot(., aes(value)) +
  geom_histogram() +
  geom_vline(data = vline_df, aes(xintercept = xintercept), color = "red",
             linetype = 2) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Value", y = "Counts") +
  theme_AP()

# Top 10 most risky paths (by probability of failure) --------------------------

top_paths <- paths_tbl %>% 
  arrange(desc(p_path_fail)) %>% 
  slice_head(n = 10)

print(top_paths)

# Highlight the edges pertaining to the top ten risky paths --------------------

k <- min(10, nrow(top_paths))
top_k_paths <- top_paths %>% 
  slice_head(n = k)

path_edges_all <- purrr::imap_dfr(top_k_paths$path_nodes, function(nodes_vec, pid){
  tibble(
    from = head(nodes_vec, -1),
    to   = tail(nodes_vec, -1),
    path_id  = pid,
    risk_sum = top_k_paths$risk_sum[pid]
  )
})

ig2 <- as.igraph(call_g)
edge_df_names <- igraph::as_data_frame(ig2, what = "edges") %>%
  mutate(.edge_idx = dplyr::row_number())

# Collapse possible duplicate (same edge in multiple risky paths) --------------

path_edges_collapsed <- path_edges_all %>%
  dplyr::group_by(from, to) %>%
  dplyr::summarise(
    path_freq = dplyr::n(),
    risk_mean_path = mean(risk_sum, na.rm = TRUE),
    .groups = "drop"
  )

# Join by edge -----------------------------------------------------------------

edge_marks <- edge_df_names %>%
  dplyr::left_join(path_edges_collapsed, by = c("from","to")) %>%
  dplyr::mutate(
    on_top_path    = !is.na(path_freq),
    path_freq      = ifelse(is.na(path_freq), 0L, path_freq),
    risk_mean_path = ifelse(is.na(risk_mean_path), 0, risk_mean_path)
  )

# Prepare object for plotting --------------------------------------------------

call_g_sugi <- call_g %>%
  activate(edges) %>%
  mutate(
    on_top_path    = edge_marks$on_top_path,
    path_freq      = edge_marks$path_freq,
    risk_mean_path = edge_marks$risk_mean_path
  ) %>%
  activate(nodes) %>%
  mutate(
    cyclo_class = case_when(
      cyclomatic_complexity <= 10 ~ 'green',
      cyclomatic_complexity <= 20 ~ 'orange',
      cyclomatic_complexity <= 50 ~ 'red',
      cyclomatic_complexity >  50 ~ 'purple',
      TRUE ~ 'grey'
    )
  )

# Mark nodes that show at least in one of the top ten risky paths --------------

risky_nodes <- unique(unlist(top_k_paths$path_nodes))
call_g_sugi <- call_g_sugi %>%
  activate(nodes) %>%
  mutate(on_top_node = name %in% risky_nodes)

call_g_sugi %>%
  activate(nodes) %>%
  data.frame() %>%
  data.table() %>%
  .[order(-indeg)]

# PLOT #########################################################################

p_sugi <- ggraph(call_g_sugi, layout = "sugiyama") +
  
  # 1) Background: NON-top edges in faint grey ---------------------------------

  geom_edge_link0(aes(filter = !on_top_path), colour = "grey80", alpha = 0.05, width = 0.3) +
  
  # 2) Foreground: TOP edges (drawn last so they sit on top) -------------------

  geom_edge_link0(aes(filter = on_top_path,
                      colour = risk_mean_path,
                      width  = pmin(pmax(path_freq, 0.5), 3)),
                  alpha = 0.9,
                  arrow = grid::arrow(length = unit(1, "mm"))) +
  scale_edge_colour_gradient(low = "orange", high = "red", guide = "none") +
  scale_edge_width(range = c(0.3, 2.2), guide = "none") +
  
  # Nodes: grey base + colored only for nodes on risky paths -------------------

  geom_node_point(size = 1.2, colour = "#BDBDBD", alpha = 0.35, show.legend = FALSE) +
  geom_node_point(aes(filter = on_top_node, size = indeg, fill = cyclo_class),
                  shape = 21, alpha = 0.95, show.legend = FALSE) +
  scale_fill_manual(values = c(green = "yellowgreen", orange = "orange", red = "red", 
                               purple = "purple", grey = "#A0A0A0"), guide = "none") +
  theme_AP() +
  labs(x = "", y = "") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position = "top") + 
  ggtitle(model.name)

p_sugi


all_graphs

















  mutate(node_id = row_number(), 
         indegree = centrality_degree(mode = "in"), 
         outdegree = centrality_degree(mode = "out"), 
         betweenness = centrality_betweenness(directed = TRUE))















# Join complexity info onto nodes-----------------------------------------------

alpha <- 0.6 # peso a complejidad (probabilidad de bug)
beta <- 0.3 # peso a in-degree (impacto aguas arriba)
gamma <- 0.1 # peso a betweenness (puente crítico)

graph <- g %>%
  activate(nodes) %>%
  left_join(cc_unique %>%
              select(name, type, language, cyclomatic_complexity, complexity_category), by = "name") %>%
  mutate(cyclomatic_scaled = rescale(cyclomatic_complexity), 
         indegree_scaled = rescale(indegree), 
         outdegree_scaled = rescale(outdegree), 
         betweenness_scaled = rescale(betweenness),
         risk_node = alpha * cyclomatic_scaled + beta * indegree_scaled + gamma * betweenness_scaled)

# 4) Encontrar caminos desde entradas (in-degree == 0) hacia sinks (out-degree == 0)
ig <- as.igraph(graph)
V(ig)$name <- graph %>% activate(nodes) %>% as_tibble() %>% pull(name)
node_df <- graph %>% activate(nodes) %>% as_tibble()


entry_ids <- which(node_df$indegree == 0) # entradas
sink_ids <- which(node_df$outdegree == 0) # sinks

# Generamos todos los caminos simples desde entradas a sinks (cuidado con explosión exponencial; aquí el grafo es pequeño)
all_paths <- list()
for (s in entry_ids) {
  for (t in sink_ids) {
    if (s != t) {
      ps <- igraph::all_simple_paths(ig, from = s, to = t, mode = "out")
      all_paths <- c(all_paths, ps)
    }
  }
}

# 5) Función para calcular el riesgo de un camino como suma y como media de riesgos de nodo
risk_of_path <- function(path_vertices) {
  idx <- as.integer(path_vertices)
  rn <- node_df$risk_node[idx]
  tibble(
    path_nodes = list(node_df$name[idx]),
    path_str = paste(node_df$name[idx], collapse = " → "),
    hops = length(idx) - 1,
    risk_sum = sum(rn),
    risk_mean = mean(rn)
  )
}


paths_tbl <- purrr::map_dfr(all_paths, risk_of_path)
# 6) Top-10 caminos más riesgosos (por riesgo acumulado)
top_paths <- paths_tbl %>% arrange(desc(risk_sum)) %>% slice_head(n = 10)
print(top_paths)


graph %>%
  activate(nodes) %>%
  data.frame() %>%
  data.table() %>%
  ggplot(., aes(indegree, cyclomatic_complexity)) +
  geom_point()

call_g <- graph

ggraph(graph, layout = "sugiyama") +
  geom_edge_link0(alpha = 0.05, arrow = grid::arrow(length = unit(1,"mm"))) +
  geom_node_point(aes(size = indegree), show.legend = FALSE) +
  theme_graph()













# 10) Visualizar en layout jerárquico (sugiyama) y colorear las 10 rutas más riesgosas con degradado de color y las demás en gris difuminado
if (nrow(top_paths) > 0) {
  # Construir tabla de aristas de los Top-10 caminos
  k <- min(10, nrow(top_paths))
  top_k_paths <- top_paths %>% 
    slice_head(n = k)
  
  path_edges_all <- purrr::imap_dfr(top_k_paths$path_nodes, function(nodes_vec, pid) {
    tibble(from = head(nodes_vec, -1),
           to = tail(nodes_vec, -1),
           path_id = pid,
           risk_sum = top_k_paths$risk_sum[pid])
  })
  
  ig2 <- as.igraph(call_g)
  edge_df_names <- igraph::as_data_frame(ig2, what = "edges") %>%
    mutate(.edge_idx = row_number())
  
  path_edges_collapsed <- path_edges_all %>%
    group_by(from, to) %>%
    summarise(path_freq = n(),
              risk_mean_path = mean(risk_sum, na.rm = TRUE), .groups = "drop")
  
  edge_marks <- edge_df_names %>%
    left_join(path_edges_collapsed, by = c("from","to")) %>%
    mutate(on_top_path = !is.na(path_freq),
           risk_mean_path = ifelse(is.na(risk_mean_path), 0, risk_mean_path))
  
  call_g_sugi <- call_g %>%
    activate(edges) %>%
    mutate(on_top_path = edge_marks$on_top_path,
           path_freq = edge_marks$path_freq,
           risk_mean_path = edge_marks$risk_mean_path)
  
  # Escalado de color: más riesgosa (mayor risk_mean_path) = rojo, menor = naranja
  # Aristas fuera del Top-10: gris difuminado (baja alpha)
  p_sugi <- ggraph(call_g_sugi, layout = "sugiyama") +
    geom_edge_link0(aes(alpha = on_top_path,
                        colour = ifelse(on_top_path, risk_mean_path, NA_real_),
                        width = ifelse(on_top_path, pmin(pmax(path_freq, 0.5), 3), 0.3)),
                    arrow = arrow(length = unit(1, "mm"))) +
    scale_edge_alpha_manual(values = c(`FALSE` = 0.05, `TRUE` = 0.9), guide = "none") +
    scale_edge_colour_gradient(low = "red", high = "purple", na.value = "grey80", name = "Risk") +
    scale_edge_width(range = c(0.3, 2.2), guide = "none") +
    geom_node_point(aes(size = indegree), show.legend = FALSE) +
    theme_AP() +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          legend.position = "top") 
  
  print(p_sugi)
}


graph %>%
  activate(nodes) %>%
  left_join(., prove, by = "name")








citation_graph <- lapply(split.networks, function(dt) 
  graph_from_data_frame(d = dt, directed = TRUE))

# Calculate network metrics ----------------------------------------------------

lapply(citation_graph, function(x) edge_density(x))

# Modularity: 
# - c.1: Strong community structure, where nodes within groups are highly connected.
# - c. -1: Opposite of community structure, where nodes between groups are more connected.
# - c. 0: Indicates absence of community structure or anti-community structure in the network.
wtc <- lapply(citation_graph, function(x) cluster_walktrap(x))
lapply(wtc, function(x) modularity(x))

network_metrics <- lapply(citation_graph, function(x) 
  data.table(node = V(x)$name,
             
             # Degree of a node: The number of connections or 
             # edges linked to that node. 
             # It represents how well-connected or central a 
             # node is within the graph.
             degree = degree(x, mode = "in"),
             
             degree.out = degree(x, mode = "out"),
             
             # Betweenness centrality of a node: Measures the 
             # extent to which a node lies on the shortest 
             # paths between all pairs of other nodes in the graph. 
             # Nodes with high betweenness centrality act as 
             # bridges or intermediaries, facilitating 
             # communication and information flow between other nodes.
             betweenness = betweenness(x),
             
             # Closeness centrality of a node: Measures how 
             # close a node is to all other nodes in the graph, 
             # taking into account the length of the shortest paths. 
             # Nodes with high closeness centrality are able to 
             # efficiently communicate or interact with other 
             # nodes in the graph.
             closeness = closeness(x),
             pagerank = page_rank(x)$vector)
)




tidygraph::as_tbl_graph(callgraph_edges, directed = TRUE)

