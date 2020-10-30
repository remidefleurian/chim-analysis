# Top-level function -----------------------------------------------------------
analyse_data <- function(df, with) {
  with %>% 
    distinct(analysis_id) %>% 
    pull() %>% 
    map_dfr(~run_analyses(df, with, .x)) %>% 
    pca_loadings_plots()
}

# Helper functions -------------------------------------------------------------
## Save analysis results for one set of matches in a single row ----------------
run_analyses <- function(.df, .with, .analysis_id) {
  .with_iter <- .with %>% 
    filter(analysis_id == .analysis_id) %>% 
    remove_outliers(.n_std = 3, .analysis_id)
  
  .df <- .df %>% 
    aggregate_df(.with_iter)
  
  stat_row <- tibble(
    iter = .analysis_id,
    n_outl = nrow(filter(.with, analysis_id == .analysis_id)) - nrow(.with_iter),
    n_pairs = nrow(.df) / 2,
    avg_eucl = .with %>% 
      filter(analysis_id == .analysis_id) %>% 
      summarise(m = mean(eucl_dist)) %>% 
      pull()
  ) 
  
  stat_row <- stat_row %>% 
    bind_cols(t_tests(.df, .analysis_id)) %>% 
    bind_cols(glm_valence(.df)) %>% 
    bind_cols(med_valence(.df, .analysis_id, .sims = 5000)) %>%
    bind_cols(med_valence(.df, .analysis_id, .sims = 5000, .transform = TRUE)) %>%
    bind_cols(pca_features(.df, .analysis_id)) %>% 
    bind_cols(glm_components(.df)) %>% 
    bind_cols(lm_valence_diff(.df, .analysis_id))
  
  .df %>% 
    collinearity_plot(.analysis_id)
  
  if (.analysis_id == "a1") {
    .df %>% 
      matching_plot()
  }

  stat_row
}

## Remove Euclidean distance outliers ------------------------------------------
remove_outliers <- function(.df, .n_std = 3, .analysis_id) {
  p <- .df %>%
    mutate(outlier = ifelse(
      is.na(eucl_dist) | eucl_dist < mean(eucl_dist, na.rm = TRUE) + 
        3 * sd(eucl_dist, na.rm = TRUE), 
      "No", 
      "Yes")) %>%
    ggplot(aes(eucl_dist, fill= outlier)) +
    geom_histogram() +
    theme_minimal() +
    scale_fill_manual(values = c("#35B779", "#31688E")) +
    labs(x = "Euclidian distance", y = "Count", fill = "Outlier")
    
    ggsave(paste0(
      "output/plots/diagnostics/eucl-outliers/", 
      .analysis_id, 
      "-outliers.png"
    ), p)
  
  .df %>% 
    filter(is.na(eucl_dist) | 
             eucl_dist < mean(eucl_dist, na.rm = TRUE) + 
             .n_std * sd(eucl_dist, na.rm = TRUE))
}

## Aggregate tracks and remove chills tracks with no matches -------------------
aggregate_df <- function(.df, .with) {
  .df %>% 
    bind_rows(select(.with, -eucl_dist)) %>% 
    add_count(match_id) %>% 
    filter(n == 2) %>% 
    select(-n) 
}

## Nonparametric t-tests for differences in popularity and duration ------------
t_tests <- function(.df, .analysis_id) {
  t_dur <- .df %>% 
    select(type, match_id, duration_ms) %>% 
    pivot_wider(names_from = type, values_from = duration_ms) %>% 
    wilcox.test(.$chills, .$match, data = ., paired = TRUE)
  
  t_pop <- .df %>% 
    select(type, match_id, popularity) %>% 
    pivot_wider(names_from = type, values_from = popularity) %>% 
    wilcox.test(.$chills, .$match, data = ., paired = TRUE)
  
  tibble(
    t_dur_V   = t_dur$statistic,
    t_dur_p   = t_dur$p.value,
    t_dur_sig = t_dur_p < 0.05,
    t_pop_V   = t_pop$statistic,
    t_pop_p   = t_pop$p.value,
    t_pop_sig = t_pop_p < 0.05,
  )
}

## Logistic regression for effect of valence -----------------------------------
glm_valence <- function(.df) {
  mod <- .df %>% 
    mutate(type = ifelse(type == "chills", 1, 0)) %>% 
    glm(type ~ valence, data = ., family = "binomial")
  
  # Influential data points for coefficient of valence
  infl <- dfbeta(mod) %>% 
    as_tibble %>% 
    filter(abs(valence) >= abs(mod$coefficients[2]) / 2) %>% 
    nrow()
  
  mod_stats <- lrm(type ~ valence, data = .df)
  
  val_diff <- .df %>% 
    group_by(type) %>% 
    summarise(m_val = mean(valence)) %>% 
    pivot_wider(names_from = type, values_from = m_val) %>% 
    mutate(val_diff = chills - match) %>% 
    pull()
    
  tibble(
    val_mod_df   = mod_stats[["stats"]][["d.f."]],
    val_mod_chi2 = mod_stats[["stats"]][["Model L.R."]],
    val_mod_p    = mod_stats[["stats"]][["P"]],
    val_mod_r2   = mod_stats[["stats"]][["R2"]],
    val_mod_infl = infl,
    val_mod_sig  = val_mod_p < 0.05,
    val_coef     = mod_stats[["coefficients"]][["valence"]],
    val_coef_Z   = (mod_stats$coef / sqrt(diag(mod_stats$var)))[["valence"]],
    val_coef_p   = pnorm(abs(val_coef_Z), lower.tail = FALSE) * 2,
    val_coef_sig = val_coef_p < 0.05,
    val_diff     = val_diff
  )
}

## Mediation of sqrt popularity and log duration on log valence ----------------
med_valence <- function(.df, .analysis_id, .sims, .transform = FALSE) {
  if (.transform) {
    .df <- .df %>%
      filter(popularity != 0 & valence != 0) %>%
      mutate(valence = log(valence),
             duration_ms = log(duration_ms),
             popularity= sqrt(popularity)) %>% 
      mutate(type = ifelse(type == "chills", 1, 0))
    
    output_path <- "output/plots/diagnostics/mediation/transformed/"
  } else {
    .df <- .df %>% 
      mutate(type = ifelse(type == "chills", 1, 0))
    
    output_path <- "output/plots/diagnostics/mediation/original/"
  }
  
  mod_M_pop <- .df %>% lm(popularity ~ valence, data = .)
  mod_Y_pop <- .df %>% glm(
    type ~ valence + popularity, data = ., 
    family = "binomial"
  )
  res_pop   <- mediate(
    mod_M_pop, 
    mod_Y_pop, 
    treat = "valence", 
    mediator = "popularity", 
    boot = TRUE, 
    sims = .sims
  )
  
  png(filename = paste0(output_path, .analysis_id, "-pop-resid.png"))
  plot(fitted(mod_M_pop), resid(mod_M_pop))
  abline(h = 0)
  dev.off()
  
  png(filename = paste0(output_path, .analysis_id, "-pop-resid-qq.png"))
  qqnorm(resid(mod_M_pop))
  dev.off()
  
  mod_M_dur <- .df %>% lm(duration_ms ~ valence, data = .)
  mod_Y_dur <- .df %>% glm(type ~ valence + duration_ms, data = ., family = "binomial")
  res_dur   <- mediate(mod_M_dur, mod_Y_dur, treat = "valence", mediator = "duration_ms", boot = TRUE, sims = .sims)
  
  png(filename = paste0(output_path, .analysis_id, "-dur-resid.png"))
  plot(fitted(mod_M_dur), resid(mod_M_dur))
  abline(h = 0)
  dev.off()
  
  png(filename = paste0(output_path, .analysis_id, "-dur-resid-qq.png"))
  qqnorm(resid(mod_M_dur))
  dev.off()
  
  results <- tibble(
    med_dur_ACME   = res_dur$d.avg,
    med_dur_ACME_p = res_dur$d.avg.p,
    med_dur_ADE    = res_dur$z.avg,
    med_dur_ADE_p  = res_dur$z.avg.p,
    med_dur_prop   = res_dur$n.avg,
    med_dur_med    = case_when(
      med_dur_ACME_p < 0.05 & med_dur_ADE_p >= 0.05 ~ "full",
      med_dur_ACME_p < 0.05 & med_dur_ADE_p < 0.05 ~ "partial",
      TRUE ~ "none"
    ),
    med_pop_ACME   = res_pop$d.avg,
    med_pop_ACME_p = res_pop$d.avg.p,
    med_pop_ADE    = res_pop$z.avg,
    med_pop_ADE_p  = res_pop$z.avg.p,
    med_pop_prop   = res_pop$n.avg,
    med_pop_med    = case_when(
      med_pop_ACME_p < 0.05 & med_pop_ADE_p >= 0.05 ~ "full",
      med_pop_ACME_p < 0.05 & med_pop_ADE_p < 0.05 ~ "partial",
      TRUE ~ "none"
    ))
  
  if (.transform) {
    results <- results %>% 
      rename_with(function(x) str_replace(x, "med", "med_t"))
  }
  
  results
}

## Reduce collinearity with PCA and save scree plot and biplot -----------------
pca_features <- function(.df, .analysis_id) {
  pca <- .df %>% 
    select(
      tempo, 
      loudness, 
      valence, 
      danceability, 
      energy, 
      acousticness, 
      instrumentalness, 
      speechiness, 
      liveness
    ) %>% 
    prcomp(center = TRUE, scale. = TRUE)
  
  eigen <- pca$sdev^2
  
  .df <- .df %>% 
    bind_cols(as_tibble(pca$x))
  
  # Scree plot
  p <- summary(pca)$importance[2:3, 1:9] %>% 
    as_tibble() %>% 
    mutate(type = ifelse(row_number() == 1, "Individual", "Cumulative")) %>% 
    pivot_longer(-type) %>% 
    pivot_wider(names_from = type, values_from = value) %>% 
    mutate(PC = as.character(row_number())) %>% 
    ggplot() +
    geom_col(aes(PC, Individual, fill = "Individual")) +
    geom_point(aes(PC, Cumulative, group = 1, colour = "Cumulative")) +
    geom_line(aes(PC, Cumulative, group = 1, colour = "Cumulative")) +
    labs(x = "Dimension", y = "Proportion of variance") +
    scale_color_manual(name = "", values = c("Cumulative" = "#31688E")) +
    scale_fill_manual(name = "", values = c("Individual" = "#35B779")) +
    theme_minimal()
  
  ggsave(paste0(
    "output/plots/diagnostics/scree/", 
    .analysis_id, 
    "-scree.png"
  ), p)
  
  # Biplot
  if(.analysis_id == "a1") {
    p <- .df %>% 
      custom_biplot(.pca_obj = pca)
    
    ggsave("output/plots/figure-2.jpg", p, dpi = 600, width = 9.7, height = 6.5) 
  }
  
  row <- tibble(
    pca_n_pc = sum(eigen > 1),
    pca_cum  = as_tibble(summary(pca)[["importance"]])[[pca_n_pc]][3]
  )
  
  loadings <- pca$rotation %>% 
    as_tibble() %>% 
    select(pca_pc1 = PC1, 
           pca_pc2 = PC2) %>% 
    mutate(var = c("tmpo", "ldns", "vlnc", "dnce", "nrgy", 
                   "acst", "inst", "spch", "lvns")) %>% 
    pivot_wider(names_from = var, values_from = c(pca_pc1, pca_pc2))
  
  row %>% 
    bind_cols(loadings)
}

### Custom biplot function 
### https://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
custom_biplot <- function(.df, .pca_obj) {
  
  rot <- as_tibble(.pca_obj$rotation) %>% 
    mutate(name = str_to_title(rownames(.pca_obj$rotation)))
  
  mult <- min((max(.df$PC2) - min(.df$PC2) / (max(rot$PC2) - min(rot$PC2))),
              (max(.df$PC1) - min(.df$PC1) / (max(rot$PC1) - min(rot$PC1))))
  
  rot <- rot %>% 
    mutate(x = 0.4 * mult * PC1, # arrow coordinates
           y = 0.4 * mult * PC2, # arrow coordinates
           d = atan2(y, x) * 180 / pi, # degrees
           a = d * pi / 180, # angle
           l = sqrt(x^2 + y^2), # length
           h = ifelse(abs(d) > 90, "right", "left"), # text aligntment
           xtxt = ifelse(abs(d) > 90, (l + 0.15) * cos(a), (l + 0.15) * cos(a)), 
           ytxt = ifelse(abs(d) > 90, (l + 0.15) * sin(a), (l + 0.15) * sin(a)),
           d = ifelse(abs(d) > 90, d + 180, d)) %>% # no upside-down text
    # prevent overlap between lavels for speechiness and liveness
    mutate(xtxt = case_when(name == "Speechiness" ~ xtxt + 0.16,
                            name == "Liveness" ~ xtxt - 0.16,
                            TRUE ~ xtxt),
           ytxt = case_when(name == "Speechiness" ~ ytxt - 0.08,
                            name == "Liveness" ~ ytxt + 0.08,
                            TRUE ~ ytxt))
  
  x_range <- range(.df$PC1)
  y_range <- range(.df$PC2)
  
  .df <- .df %>% 
    mutate(type = str_to_title(type))
  
  p1 <- .df %>% 
    ggplot(aes(PC1, PC2, colour = type)) +
    geom_point(alpha = 0.4, size = 1.2, stroke = 0) +
    geom_text(
      data=rot, 
      aes(xtxt, ytxt, label = name, angle = d, hjust = h),
      vjust = "center", 
      size = 3, 
      color = "black"
    ) +
    geom_segment(
      data = rot, 
      aes(x = 0, y = 0, xend = x, yend = y), 
      arrow = arrow(length = unit(0.2, "cm")),
      color = "black"
    ) +
    coord_equal() +
    scale_x_continuous(limits = x_range) +
    scale_y_continuous(limits = y_range) +
    theme_minimal() +
    labs(x = "Component 1", y = "Component 2") +
    scale_color_manual(values = c("#d55e00", "#9ad0f3")) +
    scale_fill_manual(values = c("#d55e00", "#9ad0f3")) +
    theme(plot.margin = margin(0,0,0,0, unit = "pt"),
          legend.position = "none")

  p2 <- .df %>% 
    select(type, PC1) %>% 
    group_by(type) %>% 
    mutate(median = median(PC1, na.rm = TRUE),
           median_d = approx(
             density(PC1)$x, 
             density(PC1)$y, 
             xout = median)$y
    ) %>%
    ggplot(aes(PC1, colour = type, fill = type)) +
    geom_segment(
      aes(x = median, xend = median, y = 0, yend = median_d, colour = type)
    ) +
    geom_density(alpha = 0.4) + 
    scale_x_continuous(limits = x_range) +
    theme_minimal() +
    labs(colour = "Track", fill = "Track") +
    scale_color_manual(values = c("#d55e00", "#9ad0f3")) +
    scale_fill_manual(values = c("#d55e00", "#9ad0f3")) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = margin(0,0,0,0, unit = "pt"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

  p3 <- .df %>% 
    select(type, PC2) %>% 
    group_by(type) %>% 
    mutate(median = median(PC2, na.rm = TRUE),
           median_d = approx(
             density(PC2)$x, 
             density(PC2)$y, 
             xout = median)$y
    ) %>%
    ggplot(aes(PC2, colour = type, fill = type)) +
    geom_segment(
      aes(x = median, xend = median, y = 0, yend = median_d, colour = type)
    ) +
    geom_density(alpha = 0.4) + 
    scale_x_continuous(limits = y_range) +
    coord_flip() +
    theme_minimal() +
    labs(colour = "Track", fill = "Track") +
    scale_color_manual(values = c("#d55e00", "#9ad0f3")) +
    scale_fill_manual(values = c("#d55e00", "#9ad0f3")) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = margin(0,0,0,0, unit = "pt"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())

  p2 + guide_area() + p1 + p3 + plot_layout(guides = "collect")
}

## Logistic regression for effect of PC1 and PC2 -------------------------------
glm_components <- function(.df) {
  pca <- .df %>% 
    select(
      tempo, 
      loudness, 
      valence, 
      danceability, 
      energy, 
      acousticness, 
      instrumentalness, 
      speechiness, 
      liveness
    ) %>% 
    prcomp(center = TRUE, scale. = TRUE)
  
  .df <- .df %>% 
    bind_cols(as_tibble(pca$x))
  
  mod <- .df %>% 
    mutate(type = ifelse(type == "chills", 1, 0)) %>% 
    glm(type ~ PC1 + PC2, data = ., family = "binomial")
  
  # Influential data points for coefficient of valence
  infl <- dfbeta(mod) %>% 
    as_tibble %>% 
    filter(abs(PC1) >= abs(mod$coefficients[2]) / 2 |
             abs(PC2) >= abs(mod$coefficients[3]) / 2) %>% 
    nrow()
  
  mod_stats <- lrm(type ~ PC1 + PC2, data = .df)
  
  row <- tibble(
    pca_mod_df       = mod_stats[["stats"]][["d.f."]],
    pca_mod_chi2     = mod_stats[["stats"]][["Model L.R."]],
    pca_mod_p        = mod_stats[["stats"]][["P"]],
    pca_mod_r2       = mod_stats[["stats"]][["R2"]],
    pca_mod_infl     = infl,
    pca_mod_sig      = pca_mod_p < 0.05,
    pca_coef_pc1     = mod_stats[["coefficients"]][["PC1"]],
    pca_coef_pc1_Z   = (mod_stats$coef / sqrt(diag(mod_stats$var)))[["PC1"]],
    pca_coef_pc1_p   = pnorm(abs(pca_coef_pc1_Z), lower.tail = FALSE) * 2,
    pca_coef_pc1_sig = pca_coef_pc1_p < 0.05,
    pca_coef_pc2     = mod_stats[["coefficients"]][["PC2"]],
    pca_coef_pc2_Z   = (mod_stats$coef / sqrt(diag(mod_stats$var)))[["PC2"]],
    pca_coef_pc2_p   = pnorm(abs(pca_coef_pc2_Z), lower.tail = FALSE) * 2,
    pca_coef_pc2_sig = pca_coef_pc2_p < 0.05
  )
  
  # Re-analysis for influential data points
  if(infl > 0) {
    outl <- dfbeta(mod) %>% 
      as_tibble %>% 
      mutate(row_number = row_number()) %>% 
      filter(abs(PC1) >= abs(mod$coefficients[2]) / 2 |
               abs(PC2) >= abs(mod$coefficients[3]) / 2) %>% 
      select(row_number)
    
    .df <- .df %>% 
      filter(!row_number() %in% pull(outl))
    
    mod <- .df %>% 
      mutate(type = ifelse(type == "chills", 1, 0)) %>% 
      glm(type ~ PC1 + PC2, data = ., family = "binomial")
    
    infl <- dfbeta(mod) %>% 
      as_tibble %>% 
      filter(abs(PC1) >= abs(mod$coefficients[2]) / 2 |
               abs(PC2) >= abs(mod$coefficients[3]) / 2) %>% 
      nrow()
    
    mod_stats <- lrm(type ~ PC1 + PC2, data = .df)
    
    row <- row %>% 
      bind_cols(tibble(
      pca_mod_df_no_outl       = mod_stats[["stats"]][["d.f."]],
      pca_mod_chi2_no_outl     = mod_stats[["stats"]][["Model L.R."]],
      pca_mod_p_no_outl        = mod_stats[["stats"]][["P"]],
      pca_mod_r2_no_outl       = mod_stats[["stats"]][["R2"]],
      pca_mod_infl_no_outl     = infl,
      pca_mod_sig_no_outl      = pca_mod_p_no_outl < 0.05,
      pca_coef_pc1_no_outl     = mod_stats[["coefficients"]][["PC1"]],
      pca_coef_pc1_Z_no_outl   = (mod_stats$coef / sqrt(diag(mod_stats$var)))[["PC1"]],
      pca_coef_pc1_p_no_outl   = pnorm(abs(pca_coef_pc1_Z_no_outl), lower.tail = FALSE) * 2,
      pca_coef_pc1_sig_no_outl = pca_coef_pc1_p_no_outl < 0.05,
      pca_coef_pc2_no_outl     = mod_stats[["coefficients"]][["PC2"]],
      pca_coef_pc2_Z_no_outl   = (mod_stats$coef / sqrt(diag(mod_stats$var)))[["PC2"]],
      pca_coef_pc2_p_no_outl   = pnorm(abs(pca_coef_pc2_Z_no_outl), lower.tail = FALSE) * 2,
      pca_coef_pc2_sig_no_outl = pca_coef_pc2_p_no_outl < 0.05
    ))
  } else {
    row <- row %>% 
      bind_cols(tibble(
        pca_mod_df_no_outl       = NA,
        pca_mod_chi2_no_outl     = NA,
        pca_mod_p_no_outl        = NA,
        pca_mod_r2_no_outl       = NA,
        pca_mod_infl_no_outl     = NA,
        pca_mod_sig_no_outl      = NA,
        pca_coef_pc1_no_outl     = NA,
        pca_coef_pc1_Z_no_outl   = NA,
        pca_coef_pc1_p_no_outl   = NA,
        pca_coef_pc1_sig_no_outl = NA,
        pca_coef_pc2_no_outl     = NA,
        pca_coef_pc2_Z_no_outl   = NA,
        pca_coef_pc2_p_no_outl   = NA,
        pca_coef_pc2_sig_no_outl = NA
      ))
  }
  
  row
}

## Linear regression for effect of PC1 and PC2 on valence difference -----------
lm_valence_diff <- function(.df, .analysis_id) {
  .df <- .df %>% 
    select(match_id, type, valence) %>% 
    pivot_wider(names_from = type, values_from = valence) %>% 
    mutate(valence_diff = chills - match) %>% 
    select(-chills, -match) %>% 
    left_join(filter(.df, type == "chills"))
  
  pca <- .df %>% 
    select(
      tempo, 
      loudness, 
      danceability, 
      energy, 
      acousticness, 
      instrumentalness, 
      speechiness, 
      liveness,
      valence
    ) %>% 
    prcomp(center = TRUE, scale. = TRUE)
  
  eigen <- pca$sdev^2
  
  .df <- .df %>% 
    bind_cols(as_tibble(pca$x))
  
  mod <- .df %>% 
    lm(valence_diff ~ PC1 + PC2, data = .)
  
  # Influential data points for coefficient of valence
  infl <- dfbeta(mod) %>% 
    as_tibble %>% 
    filter(abs(PC1) >= abs(mod$coefficients[2]) / 2 |
             abs(PC2) >= abs(mod$coefficients[3]) / 2) %>% 
    nrow()
  
  mod_stats <- mod %>% 
    summary()
  
  row <- tibble(
    pcavd_n_pc = sum(eigen > 1),
    pcavd_cum  = as_tibble(summary(pca)[["importance"]])[[pcavd_n_pc]][3]
  )
  
  loadings <- pca$rotation %>% 
    as_tibble() %>% 
    select(pcavd_pc1 = PC1, 
           pcavd_pc2 = PC2) %>% 
    mutate(var = c("tmpo", "ldns", "dnce", "nrgy", "acst",
                   "inst", "spch", "lvns", "vlnc")) %>% 
    pivot_wider(names_from = var, values_from = c(pcavd_pc1, pcavd_pc2))
  
  mod_values <- tibble(
    pcavd_mod_colin    = cor.test(.df$PC1, .df$PC2)$p.value < .05,
    pcavd_mod_df1      = mod_stats[["fstatistic"]][["numdf"]],
    pcavd_mod_df2      = mod_stats[["fstatistic"]][["dendf"]],
    pcavd_mod_F        = mod_stats[["fstatistic"]][["value"]],
    pcavd_mod_p        = unname(pf(pcavd_mod_F, pcavd_mod_df1, pcavd_mod_df2, lower.tail = FALSE)),
    pcavd_mod_r2       = mod_stats$r.squared,
    pcavd_mod_adjr2    = mod_stats$adj.r.squared,
    pcavd_mod_infl     = infl,
    pcavd_mod_sig      = pcavd_mod_p < 0.05,
    pcavd_coef_pc1     = mod_stats$coefficients[2, 1],
    pcavd_coef_pc1_t   = mod_stats$coefficients[2, 3],
    pcavd_coef_pc1_p   = mod_stats$coefficients[2, 4],
    pcavd_coef_pc1_sig = pcavd_coef_pc1_p < 0.05,
    pcavd_coef_pc2     = mod_stats$coefficients[3, 1],
    pcavd_coef_pc2_t   = mod_stats$coefficients[3, 3],
    pcavd_coef_pc2_p   = mod_stats$coefficients[3, 4],
    pcavd_coef_pc2_sig = pcavd_coef_pc2_p < 0.05
  )
  
  row <- row %>% 
    bind_cols(loadings) %>% 
    bind_cols(mod_values)
  
  png(filename = paste0(
    "output/plots/diagnostics/linear-reg/", 
    .analysis_id, 
    "-resid.png"
  ))
  plot(fitted(mod), resid(mod))
  abline(h = 0)
  dev.off()
  
  png(filename = paste0(
    "output/plots/diagnostics/linear-reg/", 
    .analysis_id, 
    "-resid-qq.png"
  ))
  qqnorm(resid(mod))
  dev.off()
  
  row
}

## Collinearity plot for continuous variables -----------------------------------
collinearity_plot <- function(.df, .analysis_id) {
  df_cor <- .df %>%
    select(
      Tempo= tempo, 
      Loudness = loudness, 
      Valence = valence, 
      Danceability = danceability, 
      Energy = energy, 
      Acousticness = acousticness, 
      Instrumentalness = instrumentalness, 
      Speechiness = speechiness, 
      Liveness = liveness
    )
  
  df_r_val <- df_cor %>% cor()
  df_p_val <- df_cor %>% cor_pmat()
  
  p <- ggcorrplot(
    df_r_val, 
    p.mat = df_p_val, 
    hc.order = TRUE, 
    type = "lower",
    insig = "pch", 
    pch.col = "black", 
    pch.cex = 5,
    colors = c("#35B779", "white", "#31688E"),
    outline.color = "white", 
    legend.title = "R coef.", 
    tl.cex = 10
  )
  
  ggsave(paste0(
    "output/plots/diagnostics/collinearity/", 
    .analysis_id, 
    "-collinearity.png"
  ), p)
  
  .df
}

## Plot for matching procedure and resulting feature densities -----------------
matching_plot <- function(.df) {
  
  # Standardise variables
  df_std <- df_chil %>% 
    select(artist_id, track_id, duration_ms, popularity) %>%
    mutate(type = "Chills")
  
  with_std <- matches %>% 
    select(artist_id, track_id, duration_ms, popularity) %>% 
    mutate(type = "Match (available)")
  
  all_std <- df_std %>% 
    bind_rows(with_std) %>% 
    mutate(
      duration_ms_std = scale(duration_ms),
      popularity_std = scale(popularity)
    )
  
  # Plot for Pink Floyd tracks
  # Hard-coded - Will probably be broken for a newly generated set of matches
  p1 <- all_std %>%
    filter(artist_id == "0k17h0D3J5VfsdmQ1iZtE9") %>%
    mutate(type = ifelse(track_id %in% c(
      "0lT4imdjSzpCtooUccYAKj",
      "7CLHw352I29cIRf944RQnO",
      "0gEaeqVRHPzRc7HMXtOKc7",
      "7F02x6EKYIQV3VcTaTm7oN",
      "2mZreZ4DxJE1et4I7Zy9l7",
      "5HNCy40Ni5BZJFw1TKzRsC"
    ), "Match (selected)", type)) %>%
    mutate(duration_min = duration_ms / 60000) %>%
    ggplot(aes(popularity, duration_min, colour = type)) +
    geom_point(
      data = . %>% filter(type == "Match (available)"),
      alpha = 0.4, 
      size = 1.2,
      stroke = 0
    ) +
    geom_point(
      data = . %>% filter(type != "Match (available)"), 
      size = 2.4, 
      stroke = 0
    ) +
    theme_minimal() +
    scale_color_manual(values = c("#d55e00", "grey60", "#9ad0f3")) +
    labs(title = "A", x = "Popularity", y = "Duration (min.)", colour = "Track") +
    theme(legend.position = "none")
  
  # Densities
  p2 <- .df %>% 
    select(
      type, 
      tempo, 
      loudness, 
      valence, 
      danceability, 
      energy, 
      acousticness, 
      instrumentalness, 
      speechiness, 
      liveness,
      duration_ms,
      popularity
    ) %>% 
    mutate(duration = duration_ms / 60000) %>%
    select(-duration_ms) %>% 
    pivot_longer(-type) %>% 
    mutate(name = str_to_title(name),
           type = str_to_title(type)) %>% 
    group_by(name, type) %>% 
    mutate(median = median(value, na.rm = TRUE),
           median_d = approx(
             density(value)$x, 
             density(value)$y, 
             xout = median)$y
    ) %>%
    ggplot(aes(value, colour = type, fill = type)) +
    geom_segment(
      aes(x = median, xend = median, y = 0, yend = median_d, colour = type)
    ) +
    geom_density(alpha = 0.4) +
    facet_wrap(~name, ncol = 3, scales = "free") + 
    theme_minimal() +
    scale_color_manual(values = c("#d55e00", "#9ad0f3")) +
    scale_fill_manual(values = c("#d55e00", "#9ad0f3")) +
    theme(axis.text.x  = element_text(angle = 45, hjust = 1), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(title = "B", x = "", y = "Density", fill = "Track", colour = "Track")
  
  p <- p1 + p2
  
  ggsave("output/plots/figure-1.jpg", p, dpi = 600, width = 8, height = 5) 
}
## Plots for PCA loadings over analysis iterations -----------------------------
pca_loadings_plots <- function(.df) {
  p <- .df %>%
    # Check if sign diffs in PCA loadings are reflected in direction of effect
    mutate(pc2_effect_dir = ifelse(pca_coef_pc2 > 0, "positive", "negative")) %>%
    select(iter, pc2_effect_dir, pca_pc1_tmpo:pca_pc2_lvns) %>%
    pivot_longer(
      -c("iter", "pc2_effect_dir"),
      names_to = "variable",
      values_to = "loadings"
    ) %>%
    separate(variable, into = c("pca", "component", "variable")) %>%
    select(-pca) %>%
    ggplot(aes(variable, loadings)) +
    geom_boxplot() +
    geom_jitter(aes(colour = pc2_effect_dir)) +
    coord_flip() +
    theme_minimal() +
    facet_wrap( ~ component)
  
  ggsave("output/plots/diagnostics/pca-loadings/loadings.png", p)
  
  p <- .df %>%
    mutate(pc2_effect_dir = ifelse(pca_coef_pc2 > 0, "positive", "negative")) %>%
    select(iter, pc2_effect_dir, pca_pc1_tmpo:pca_pc2_lvns) %>%
    pivot_longer(
      -c("iter", "pc2_effect_dir"),
      names_to = "variable",
      values_to = "loadings"
    ) %>%
    separate(variable, into = c("pca", "component", "variable")) %>%
    select(-pca) %>%
    ggplot(aes(variable, abs(loadings))) +
    geom_boxplot() +
    geom_jitter(aes(colour = pc2_effect_dir)) +
    coord_flip() +
    theme_minimal() +
    facet_wrap( ~ component)
  
  ggsave("output/plots/diagnostics/pca-loadings/abs-loadings.png", p)
  
  .df
}
