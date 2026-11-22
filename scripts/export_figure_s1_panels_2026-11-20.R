# Rebuild the six Figure S1 panels for manual insertion into Word.
# Run from the project root. No manuscript, model or input data is modified.
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(grid)
  library(gtable)
})

output_dir <- file.path("figs", "Figure_S1_for_Word_2026-11-20")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# Keep font-metric calculations off the default Rplots.pdf device.
pdf(file = NULL)

dat <- readRDS("data/moult_breeding_KK_data.rds") %>%
  mutate(rings_comb = as.character(rings_comb_simp))

status_levels <- c("Migrant", "Resident", "Unknown")
strip_fill <- c(Migrant = "#B22222", Resident = "#2E7D32", Unknown = "#D9D9D9")
strip_text <- c(Migrant = "white", Resident = "white", Unknown = "black")
page_width <- 6.5
page_height <- 8
strip_width <- unit(2.4, "cm")
legend_width <- unit(1.18, "inches")
strip_font_size <- 8

# Build both legends independently of the plotted subset. This retains all
# three status categories even when a category is absent from a given panel.
legend_data <- data.frame(
  x = 1:3,
  y = 1:3,
  migration_group = factor(status_levels, levels = status_levels),
  score = c(1, 4, 7)
)
legend_plot <- ggplot(legend_data, aes(x, y)) +
  geom_point(aes(colour = migration_group), shape = 15, size = 3) +
  geom_point(aes(fill = score), shape = 21, size = 2) +
  scale_colour_manual(
    name = "migratory status", values = strip_fill,
    limits = status_levels, drop = FALSE,
    guide = guide_legend(order = 1, override.aes = list(shape = 15, size = 4))
  ) +
  scale_fill_gradient(
    name = "moult score", low = "white", high = "#cc4c02",
    limits = c(1, 7), breaks = 1:7,
    guide = guide_colourbar(order = 2, barheight = unit(1.1, "inches"),
                            barwidth = unit(0.15, "inches"))
  ) +
  theme_void(base_family = "sans", base_size = 9) +
  theme(
    legend.position = "right", legend.box = "vertical",
    legend.box.just = "left", legend.justification = "left",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.18, "inches"),
    legend.key.width = unit(0.18, "inches"),
    legend.spacing.y = unit(0.16, "inches"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0)
  )
legend_gt <- ggplotGrob(legend_plot)
legend_index <- which(legend_gt$layout$name == "guide-box-right")
stopifnot(length(legend_index) == 1L)
shared_legend <- legend_gt$grobs[[legend_index]]
stopifnot(inherits(shared_legend, "gtable"))

make_panel <- function(season_year, bird_sex) {
  first_date <- as.Date(sprintf("%s-07-01", season_year))
  last_date <- as.Date(sprintf("%s-05-01", season_year + 1L))
  panel_data <- dat %>%
    filter(as.character(season) == as.character(season_year), sex == bird_sex) %>%
    mutate(
      migration_group = case_when(
        migratory_status == "M" ~ "Migrant",
        migratory_status == "R" ~ "Resident",
        TRUE ~ "Unknown"
      )
    )
  stopifnot(nrow(panel_data) > 0L,
            all(panel_data$date >= first_date & panel_data$date <= last_date))
  bird_order <- panel_data %>%
    count(rings_comb, migration_group, name = "n_plot_records") %>%
    mutate(migration_rank = match(migration_group, status_levels)) %>%
    arrange(migration_rank, desc(n_plot_records), rings_comb)
  stopifnot(nrow(bird_order) == n_distinct(panel_data$rings_comb))
  n_birds <- nrow(bird_order)
  panel_data <- panel_data %>%
    mutate(bird_row = n_birds + 1L - match(rings_comb, bird_order$rings_comb))
  sex_label <- if (bird_sex == "F") "Females" else "Males"

  # Each row still represents a single bird, but a single plotting panel allows
  # row labels to share a genuinely fixed physical strip width.
  p <- ggplot(panel_data, aes(date, bird_row)) +
    geom_hline(yintercept = seq(0.5, n_birds + 0.5, by = 1),
               colour = "grey90", linewidth = 0.2) +
    geom_point(aes(fill = as.numeric(score)),
               shape = 21, colour = "black", size = 2.25, stroke = 0.25) +
    scale_fill_gradient(low = "white", high = "#cc4c02", limits = c(1, 7)) +
    scale_x_date(limits = c(first_date, last_date), date_labels = "%W",
                 date_breaks = "3 weeks", expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.5, n_birds + 0.5),
                       breaks = NULL, expand = c(0, 0)) +
    labs(
      x = "week", y = NULL,
      title = sprintf("%s from the %s-%s breeding season in Kaikōura",
                      sex_label, season_year, season_year + 1L)
    ) +
    theme_bw(base_family = "sans", base_size = 9) +
    theme(
      legend.position = "none", axis.ticks = element_blank(),
      axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10),
      axis.text.y = element_blank(), axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "grey55", linewidth = 0.2),
      panel.border = element_rect(colour = "grey75", linewidth = 0.3),
      plot.title = element_text(size = 10, margin = margin(b = 7)),
      plot.margin = margin(5, 5, 5, 5)
    )
  gt <- ggplotGrob(p)
  panel <- gt$layout[gt$layout$name == "panel", ]
  stopifnot(nrow(panel) == 1L)

  label_y <- (n_birds - seq_len(n_birds) + 0.5) / n_birds
  strips <- grobTree(
    rectGrob(
      x = 0.5, y = label_y, width = 1, height = 1 / n_birds,
      gp = gpar(fill = unname(strip_fill[bird_order$migration_group]),
                col = "white", lwd = 0.35)
    ),
    textGrob(
      bird_order$rings_comb, x = 0.5, y = label_y,
      gp = gpar(fontsize = strip_font_size, fontfamily = "sans",
                col = unname(strip_text[bird_order$migration_group]))
    )
  )
  gt <- gtable_add_cols(gt, strip_width, pos = panel$r)
  gt <- gtable_add_grob(gt, strips, t = panel$t, b = panel$b,
                        l = panel$r + 1L, clip = "on", name = "identifier-strips")
  gt <- gtable_add_cols(gt, unit(0.09, "inches"))
  gt <- gtable_add_cols(gt, legend_width)
  legend <- shared_legend
  legend$vp <- viewport(x = 0, y = 1, just = c("left", "top"),
                         width = grobWidth(legend), height = grobHeight(legend))
  gt <- gtable_add_grob(gt, legend, t = panel$t, b = panel$b,
                        l = length(gt$widths), clip = "off", name = "fixed-legends")
  list(grob = gt, order = bird_order, data = panel_data,
       year = season_year, sex = bird_sex)
}

panels <- lapply(2021:2023, function(year) {
  lapply(c("F", "M"), function(sex) make_panel(year, sex))
}) %>% unlist(recursive = FALSE)

audit <- vector("list", length(panels))
for (i in seq_along(panels)) {
  x <- panels[[i]]
  stem <- sprintf("Figure_S1_%s-%s_%s", x$year, x$year + 1L,
                  if (x$sex == "F") "females" else "males")
  ragg::agg_png(file.path(output_dir, paste0(stem, ".png")),
                 width = page_width, height = page_height, units = "in", res = 600,
                 background = "white")
  grid.newpage()
  grid.draw(x$grob)
  # Measure the actual strip font rather than the device's default font.
  pushViewport(viewport(gp = gpar(fontsize = strip_font_size, fontfamily = "sans")))
  label_width <- max(convertWidth(stringWidth(x$order$rings_comb), "cm",
                                  valueOnly = TRUE))
  popViewport()
  legend_width_cm <- convertWidth(grobWidth(shared_legend), "cm", valueOnly = TRUE)
  strip_width_cm <- convertWidth(strip_width, "cm", valueOnly = TRUE)
  panel_cell <- x$grob$layout[x$grob$layout$name == "panel", ]
  grid.force()
  stopifnot(label_width + 0.12 < strip_width_cm,
            legend_width_cm <= convertWidth(legend_width, "cm", valueOnly = TRUE))
  dev.off()
  audit[[i]] <- data.frame(
    file = paste0(stem, ".png"), season = paste(x$year, x$year + 1L, sep = "-"),
    sex = x$sex, rows = nrow(x$order), plotted_records = nrow(x$data),
    migrants = sum(x$order$migration_group == "Migrant"),
    residents = sum(x$order$migration_group == "Resident"),
    unknown = sum(x$order$migration_group == "Unknown"),
    image_width_inches = page_width, image_height_inches = page_height,
    resolution_dpi = 600, identifier_strip_width_cm = strip_width_cm,
    longest_label_width_cm = label_width, legend_width_cm = legend_width_cm
  )
  write.csv(x$order, file.path(output_dir, paste0(stem, "_row_order.csv")),
             row.names = FALSE)
  cat("Exported", stem, "\n")
}

# A six-page vector copy is convenient for checking all panels together.
pdf_path <- file.path(output_dir, "Figure_S1_all_six_panels.pdf")
cairo_pdf(pdf_path, width = page_width, height = page_height, family = "sans",
           onefile = TRUE, bg = "white")
for (x in panels) {
  grid.newpage()
  grid.draw(x$grob)
}
dev.off()

write.csv(bind_rows(audit), file.path(output_dir, "export_audit.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(output_dir, "R_session_info.txt"))
dev.off()
cat("Saved six PNGs and the six-page vector PDF to", output_dir, "\n")
