library(readr)
library(dplyr)
library(ape)
library(plotly)

plot_pcoa <- function(distance_file, metadata_file = NULL, color_var = NULL, filter_var = NULL) {
  # Leer matriz de distancias
  dist_df <- read_csv2(distance_file, show_col_types = FALSE)
  ids <- dist_df[[1]]
  dist_mat <- dist_df %>%
    select(-1) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  rownames(dist_mat) <- ids
  colnames(dist_mat) <- ids
  diag(dist_mat) <- 0
  
  # PCoA (primeros dos ejes)
  pco <- ape::pcoa(dist_mat)
  scores <- as.data.frame(pco$vectors[, 1:2])
  colnames(scores) <- c("Axis.1", "Axis.2")
  scores$ID <- rownames(pco$vectors)
  
  # Si hay metadatos
  if (!is.null(metadata_file) && !is.null(color_var)) {
    meta <- read_csv2(metadata_file, show_col_types = FALSE)
    data_plot <- left_join(scores, meta, by = dplyr::join_by(ID == id))
    
    # Asegurar que color_var y filter_var son factores (para trazas separadas)
    if (!is.factor(data_plot[[color_var]])) data_plot[[color_var]] <- as.factor(data_plot[[color_var]])
    if (!is.null(filter_var) && !is.factor(data_plot[[filter_var]])) {
      data_plot[[filter_var]] <- as.factor(data_plot[[filter_var]])
    }
    
    # Tooltip con toda la info de la fila
    tooltip_text <- apply(data_plot, 1, function(row) {
      paste(names(row), row, sep = ": ", collapse = "<br>")
    })
    
    # Si se especifica filter_var, creamos una traza por nivel (split)
    if (!is.null(filter_var)) {
      fig <- plot_ly(
        data_plot,
        x = ~Axis.1, y = ~Axis.2,
        color = as.formula(paste0("~", color_var)),
        split = as.formula(paste0("~", filter_var)),  # <-- esto permite filtrar desde la leyenda
        text = tooltip_text, hoverinfo = "text",
        type = "scatter", mode = "markers"
      ) %>%
        layout(title = "PCoA (filtrado interactivo desde la leyenda)")
    } else {
      # Sin filtro explícito (igual puedes ocultar/mostrar desde la leyenda si color_var es discreta)
      fig <- plot_ly(
        data_plot,
        x = ~Axis.1, y = ~Axis.2,
        color = as.formula(paste0("~", color_var)),
        text = tooltip_text, hoverinfo = "text",
        type = "scatter", mode = "markers"
      ) %>%
        layout(title = "PCoA")
    }
    
  } else {
    # Sin metadatos
    fig <- plot_ly(
      scores,
      x = ~Axis.1, y = ~Axis.2,
      text = ~ID, hoverinfo = "text",
      type = "scatter", mode = "markers"
    ) %>%
      layout(title = "PCoA")
  }
  
  return(fig)
}