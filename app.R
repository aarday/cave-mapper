library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)

deg2rad <- function(deg) deg * pi / 180

# Returns the rotated azimuth (positive angle: left, negative: right)
rotate_azimuth <- function(azimuth, angle) {
  (azimuth + angle) %% 360
}

# Returns mirrored azimuth (horizontal flip)
mirror_azimuth <- function(azimuth) {
  (360 - azimuth) %% 360
}

extract_all_records <- function(sql_line) {
  record_matches <- str_match_all(sql_line, "\\((.*?)\\)")[[1]][,2]
  lapply(record_matches, function(record) {
    fields <- str_split(record, ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")[[1]]
    fields <- str_trim(gsub('^"|"$', '', fields))
    return(fields)
  })
}

ui <- fluidPage(
  titlePanel("Cave Survey Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("sqlFile", "Upload .sql file", accept = ".sql"),
      br(),
      actionButton("rotate_left", "Rotate Left 90°"),
      actionButton("rotate_right", "Rotate Right 90°"),
      actionButton("mirror", "Mirror Horizontally"),
      selectInput("filetype", "Choose file type:",
                  choices = c("PNG" = "png", "PDF" = "pdf")),
      downloadButton("downloadPlot", "Download Map")
    ),
    mainPanel(
      plotOutput("cavePlot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to track transformation state
  transform_state <- reactiveValues(angle = 0, mirror = FALSE)
  
  # Update transform_state when buttons are clicked
  observeEvent(input$rotate_left, {
    transform_state$angle <- (transform_state$angle + 90) %% 360
  })
  
  observeEvent(input$rotate_right, {
    transform_state$angle <- (transform_state$angle - 90) %% 360
  })
  
  observeEvent(input$mirror, {
    transform_state$mirror <- !transform_state$mirror
  })
  
  output$cavePlot <- renderPlot({
    req(input$sqlFile)
    
    sql_lines <- readLines(input$sqlFile$datapath, encoding = "UTF-8")
    shots_lines <- grep("^INSERT into shots values", sql_lines, value = TRUE)
    shots_data_list <- unlist(lapply(shots_lines, extract_all_records), recursive = FALSE)
    
    target_col_count <- 21
    shots_data_list_fixed <- lapply(shots_data_list, function(row) {
      len <- length(row)
      if (len < target_col_count) c(row, rep(NA, target_col_count - len)) else row[1:target_col_count]
    })
    
    shots_df <- as.data.frame(do.call(rbind, shots_data_list_fixed), stringsAsFactors = FALSE)
    colnames(shots_df) <- c(
      "survey_id", "shot_id", "from", "to", "length", "azimuth", "inclination", 
      "compass", "x", "y", "z", "flags", "deleted", "clino", "tape", "comment",
      "direction", "timestamp", "quality", "uncertainty"
    )
    
    numeric_cols <- c("survey_id", "shot_id", "length", "azimuth", "inclination", 
                      "compass", "x", "y", "z", "flags", "deleted", "clino", "tape", 
                      "direction", "timestamp", "quality", "uncertainty")
    shots_df[numeric_cols] <- lapply(shots_df[numeric_cols], as.numeric)
    
    data_shots <- shots_df %>%
      select(shot_id, from, to, length, azimuth, inclination, compass) %>%
      mutate(
        from = as.numeric(trimws(gsub('"', '', from))),
        to   = as.numeric(trimws(gsub('"', '', to)))
      ) %>%
      filter(!(is.na(from) & is.na(to)))
    
    # Modify azimuth according to transform_state
    azimuth_transformed <- data_shots$azimuth
    if (transform_state$mirror) {
      azimuth_transformed <- mirror_azimuth(azimuth_transformed)
    }
    azimuth_transformed <- rotate_azimuth(azimuth_transformed, transform_state$angle)
    
    data_shots_xy <- data_shots %>%
      mutate(
        azimuth_mod = azimuth_transformed,
        azimuth_rad = deg2rad(azimuth_mod),
        dx = -length * sin(azimuth_rad),
        dy = length * cos(azimuth_rad)
      )
    
    istasyonlar <- data.frame(id = 0, x = 0, y = 0, stringsAsFactors = FALSE)
    
    for (i in 1:nrow(data_shots_xy)) {
      shot <- data_shots_xy[i, ]
      from_id <- shot$from
      to_id <- shot$to
      if (!(from_id %in% istasyonlar$id)) next
      if (is.na(to_id)) next
      from_row <- istasyonlar %>% filter(id == from_id)
      if (!(to_id %in% istasyonlar$id)) {
        new_x <- from_row$x + shot$dx
        new_y <- from_row$y + shot$dy
        istasyonlar <- bind_rows(istasyonlar, data.frame(id = to_id, x = new_x, y = new_y))
      }
    }
    
    segments <- data_shots_xy %>%
      mutate(from = as.numeric(from), to = as.numeric(to)) %>%
      left_join(istasyonlar, by = c("from" = "id")) %>%
      rename(x = x, y = y) %>%
      left_join(istasyonlar, by = c("to" = "id"), suffix = c("_from", "_to")) %>%
      rename(x = x_from, y = y_from, xend = x_to, yend = y_to)
    
    wallshots <- data_shots_xy %>%
      filter(is.na(to) & !is.na(from)) %>%
      left_join(istasyonlar, by = c("from" = "id")) %>%
      mutate(
        xend = x + dx,
        yend = y + dy
      ) %>%
      filter(length <= 40)
    
    hulls <- wallshots %>%
      group_by(from) %>%
      do({
        df <- .
        if (nrow(df) >= 3) {
          idx <- chull(df$xend, df$yend)
          hull_df <- df[idx, c("xend", "yend")]
          hull_df$from <- df$from[1]
          colnames(hull_df) <- c("x", "y", "from")
          hull_df
        } else {
          tibble(x = numeric(0), y = numeric(0), from = df$from[1])
        }
      }) %>%
      ungroup()
    
    hulls_closed <- hulls %>%
      group_by(from) %>%
      summarize(geom = list(
        st_polygon(list(rbind(cbind(x, y), c(x[1], y[1]))))
      )) %>%
      st_as_sf()
    
    merged_hull <- st_union(hulls_closed$geom)
    
    ggplot() +
      geom_sf(data = merged_hull, fill = NA, color = "red", size = 1) +
      geom_segment(data = segments,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "gray30", size = 0.6) +
      geom_segment(data = wallshots,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   color = "orange", linetype = "dashed", size = 0.5) +
      geom_point(data = istasyonlar, aes(x = x, y = y),
                 color = "black", size = 1.5) +
      geom_text(data = istasyonlar, aes(x = x, y = y, label = id),
                color = "blue", size = 2.8, vjust = -1, hjust = 0.5) +
      coord_sf() +
      theme_void() +
      labs(title = paste("Plan - ", input$sqlFile$name))
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("cave_map_", Sys.Date(), ".", input$filetype)
    },
    content = function(file) {
      plot_obj <- last_plot()
      if (input$filetype == "png") {
        ggsave(file, plot = plot_obj, width = 10, height = 8, dpi = 300, bg = "white")
      } else if (input$filetype == "pdf") {
        ggsave(file, plot = plot_obj, width = 10, height = 8, device = cairo_pdf)
      }
    }
  )
  
}

shinyApp(ui = ui, server = server)
