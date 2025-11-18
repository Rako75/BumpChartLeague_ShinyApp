library(shiny)
library(worldfootballR)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggimage)
library(shinythemes)
library(shinyWidgets)

# ---- Fonction principale ----
bumpchart_championnat <- function(country, season, equipes_focus = NULL, 
                                  couleurs_focus = NULL, theme_sombre = TRUE,
                                  dossier_logos = "Logos clubs de football",
                                  extension_logos = c("png", "jpg", "jpeg", "svg")) {
  
  safe_tm <- function(j) {
    tryCatch(
      tm_matchday_table(country_name = country, start_year = season, matchday = j),
      error = function(e) NULL
    )
  }
  
  journee <- 1
  liste_classements <- list()
  
  repeat {
    df <- safe_tm(journee)
    if (is.null(df)) break
    df <- df %>% mutate(matchday = journee)
    liste_classements[[journee]] <- df
    Sys.sleep(0.5)
    journee <- journee + 1
  }
  
  if(length(liste_classements) == 0){
    stop("Aucune donnée disponible pour la saison ", season, " du championnat ", country)
  }
  
  # Modification ici : garder les points (pts)
  data_bump <- bind_rows(liste_classements) %>%
    select(matchday, squad, rk, pts) %>%
    filter(!is.na(rk)) %>%
    mutate(
      matchday = as.integer(matchday),
      pts = as.integer(pts),
      highlight = ifelse(!is.null(equipes_focus) & squad %in% equipes_focus, squad, "Autres")
    ) %>%
    arrange(squad, matchday)
  
  if(theme_sombre) {
    couleur_bg <- "#0d1117"
    couleur_texte <- "#e6edf3"
    couleur_grid <- "#21262d"
    couleur_autres <- "#484f58"
  } else {
    couleur_bg <- "#ffffff"
    couleur_texte <- "#24292f"
    couleur_grid <- "#d0d7de"
    couleur_autres <- "#6e7781"
  }
  
  couleurs <- c("Autres" = couleur_autres)
  
  if(!is.null(equipes_focus)){
    if(is.null(couleurs_focus)) {
      couleurs_defaut <- c("#ff4757", "#5f27cd", "#00d2d3", "#ff6348", 
                           "#1e90ff", "#ff9ff3", "#feca57", "#48dbfb",
                           "#ee5a6f", "#00d8d6")
      couleurs_focus <- couleurs_defaut[1:length(equipes_focus)]
    }
    for(i in seq_along(equipes_focus)){
      couleurs[equipes_focus[i]] <- couleurs_focus[i]
    }
  }
  
  # Modification : ajouter les points dans data_labels
  data_labels <- data_bump %>% 
    group_by(squad) %>% 
    filter(matchday == max(matchday)) %>%
    ungroup()
  
  trouver_logo <- function(nom_equipe, dossier, extensions) {
    nom_nettoye <- tolower(gsub("[^a-zA-Z0-9]", "", nom_equipe))
    
    if(!dir.exists(dossier)) {
      return(NA)
    }
    
    fichiers <- list.files(dossier, full.names = TRUE)
    
    for(fichier in fichiers) {
      nom_fichier <- tolower(gsub("[^a-zA-Z0-9]", "", basename(tools::file_path_sans_ext(fichier))))
      ext_fichier <- tolower(tools::file_ext(fichier))
      
      if(grepl(nom_nettoye, nom_fichier, fixed = TRUE) && ext_fichier %in% extensions) {
        return(fichier)
      }
    }
    
    return(NA)
  }
  
  data_labels <- data_labels %>%
    mutate(
      logo_path = sapply(squad, function(x) trouver_logo(x, dossier_logos, extension_logos)),
      # Créer le label avec les points
      label_avec_pts = paste0(squad, " : ", pts, " pts")
    )
  
  data_avec_logo <- data_labels %>% filter(!is.na(logo_path))
  data_sans_logo <- data_labels %>% filter(is.na(logo_path))
  
  titre_principal <- paste0("**", country, " ", season, "/", season + 1, 
                            " — Évolution du classement**")
  
  if(!is.null(equipes_focus) && length(equipes_focus) > 0) {
    equipes_formattees <- sapply(seq_along(equipes_focus), function(i) {
      paste0("<span style='color:", couleurs_focus[i], "'>**", 
             equipes_focus[i], "**</span>")
    })
    sous_titre <- paste(equipes_formattees, collapse = " • ")
  } else {
    sous_titre <- paste("Journées 1 à", max(data_bump$matchday))
  }
  
  p <- ggplot(data_bump, aes(x = matchday, y = rk, group = squad, 
                             color = highlight, alpha = highlight)) +
    geom_line(linewidth = 1.2, lineend = "round") +
    geom_point(data = data_bump, aes(x = matchday, y = rk), 
               size = 3, shape = 21, fill = couleur_bg, stroke = 1.2)
  
  if(nrow(data_avec_logo) > 0) {
    p <- p + 
      geom_image(
        data = data_avec_logo,
        aes(x = matchday + 1.2, y = rk, image = logo_path),
        size = 0.02,
        inherit.aes = FALSE,
        asp = 1.5,
        by = "width",
        image_fun = function(img) {
          if(inherits(img, "magick-image")) {
            img <- magick::image_scale(img, geometry = "200x200")
          }
          img
        }
      ) +
      # Modification : utiliser label_avec_pts au lieu de squad
      geom_text(
        data = data_avec_logo,
        aes(x = matchday + 2.5, label = label_avec_pts, color = highlight),
        hjust = 0,
        size = 3,
        fontface = "bold",
        show.legend = FALSE
      )
  }
  
  if(nrow(data_sans_logo) > 0) {
    p <- p +
      # Modification : utiliser label_avec_pts au lieu de squad
      geom_text(
        data = data_sans_logo,
        aes(x = matchday + 0.6, label = label_avec_pts),
        hjust = 0,
        size = 4,
        fontface = "bold",
        show.legend = FALSE
      )
  }
  
  p <- p +
    scale_y_reverse(
      breaks = seq(1, max(data_bump$rk), by = 1),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_x_continuous(
      breaks = seq(1, max(data_bump$matchday), by = 
                     ifelse(max(data_bump$matchday) > 30, 2, 1)),
      expand = expansion(mult = c(0.03, 0.15))
    ) +
    scale_color_manual(values = couleurs) +
    scale_alpha_manual(
      values = setNames(
        c(0.25, rep(1, length(equipes_focus))),
        c("Autres", equipes_focus)
      )
    ) +
    labs(
      title = titre_principal,
      subtitle = sous_titre,
      x = "**Journée**",
      y = "**Position**",
      caption = paste0("Source : Transfermarkt via worldfootballR | ",
                       "Saison ", season, "-", season + 1, " | @AlexRakotomalala")
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.background = element_rect(fill = couleur_bg, color = NA),
      panel.background = element_rect(fill = couleur_bg, color = NA),
      panel.grid.major.y = element_line(color = couleur_grid, linewidth = 0.4),
      panel.grid.major.x = element_line(color = couleur_grid, linewidth = 0.4, 
                                        linetype = "dotted"),
      panel.grid.minor = element_blank(),
      plot.title = element_markdown(
        size = 24, 
        face = "bold", 
        color = couleur_texte,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_markdown(
        size = 17, 
        color = couleur_texte,
        margin = margin(b = 20)
      ),
      plot.caption = element_text(
        size = 10, 
        color = couleur_texte, 
        hjust = 0.5,
        margin = margin(t = 20)
      ),
      axis.title.x = element_markdown(size = 15, color = couleur_texte, 
                                      margin = margin(t = 12)),
      axis.title.y = element_markdown(size = 15, color = couleur_texte, 
                                      margin = margin(r = 12)),
      axis.text = element_text(size = 13, color = couleur_texte, face = "bold"),
      axis.text.x = element_text(angle = 0),
      legend.position = "none",
      plot.margin = margin(25, 25, 25, 25)
    )
  
  return(p)
}

# ---- Interface utilisateur ----
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      
      body {
        background: linear-gradient(135deg, #0d1117 0%, #161b22 100%);
        color: #e6edf3;
        font-family: 'Inter', sans-serif;
      }
      
      .main-header {
        background: linear-gradient(135deg, #1f6feb 0%, #0969da 100%);
        padding: 30px;
        border-radius: 12px;
        margin-bottom: 30px;
        box-shadow: 0 8px 32px rgba(31, 111, 235, 0.2);
      }
      
      .main-header h1 {
        color: #ffffff;
        font-weight: 700;
        margin: 0;
        font-size: 2.2em;
        letter-spacing: -0.5px;
      }
      
      .main-header h4 {
        color: #c9d1d9;
        margin: 8px 0 0 0;
        font-weight: 400;
        font-size: 1.1em;
      }
      
      .well {
        background-color: #161b22;
        border: 1px solid #30363d;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
      }
      
      .control-label {
        font-weight: 600;
        color: #e6edf3;
        font-size: 0.95em;
        margin-bottom: 8px;
      }
      
      .form-control, .selectize-input {
        background-color: #0d1117;
        border: 1px solid #30363d;
        color: #e6edf3;
        border-radius: 6px;
        padding: 10px;
        transition: all 0.2s ease;
      }
      
      .form-control:focus, .selectize-input.focus {
        background-color: #161b22;
        border-color: #1f6feb;
        box-shadow: 0 0 0 3px rgba(31, 111, 235, 0.1);
        color: #e6edf3;
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #1f6feb 0%, #0969da 100%);
        border: none;
        font-weight: 600;
        padding: 14px 28px;
        border-radius: 8px;
        font-size: 1.05em;
        transition: all 0.3s ease;
        box-shadow: 0 4px 12px rgba(31, 111, 235, 0.3);
      }
      
      .btn-primary:hover {
        background: linear-gradient(135deg, #2970e5 0%, #0860ca 100%);
        transform: translateY(-2px);
        box-shadow: 0 6px 16px rgba(31, 111, 235, 0.4);
      }
      
      .btn-success {
        background: linear-gradient(135deg, #238636 0%, #1a7f37 100%);
        border: none;
        font-weight: 600;
        padding: 12px 24px;
        border-radius: 8px;
        transition: all 0.3s ease;
        box-shadow: 0 4px 12px rgba(35, 134, 54, 0.3);
      }
      
      .btn-success:hover {
        background: linear-gradient(135deg, #2ea043 0%, #238636 100%);
        transform: translateY(-2px);
        box-shadow: 0 6px 16px rgba(35, 134, 54, 0.4);
      }
      
      .section-title {
        color: #1f6feb;
        font-weight: 700;
        font-size: 1.2em;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #30363d;
      }
      
      .chart-container {
        background-color: #161b22;
        padding: 25px;
        border-radius: 12px;
        border: 1px solid #30363d;
        box-shadow: 0 8px 24px rgba(0, 0, 0, 0.4);
      }
      
      .placeholder-container {
        background: linear-gradient(135deg, #161b22 0%, #0d1117 100%);
        padding: 80px;
        border-radius: 12px;
        border: 2px dashed #30363d;
        text-align: center;
      }
      
      .placeholder-container h3 {
        color: #8b949e;
        font-weight: 600;
        margin-bottom: 20px;
      }
      
      .loading-container {
        background: linear-gradient(135deg, #161b22 0%, #0d1117 100%);
        padding: 60px;
        border-radius: 12px;
        text-align: center;
        border: 1px solid #30363d;
      }
      
      .loading-container h3 {
        color: #1f6feb;
        font-weight: 600;
        margin-bottom: 20px;
      }
      
      .progress {
        background-color: #0d1117;
        height: 8px;
        border-radius: 4px;
        overflow: hidden;
      }
      
      .progress-bar {
        background: linear-gradient(90deg, #1f6feb 0%, #0969da 100%);
        animation: progress-animation 1.5s ease-in-out infinite;
      }
      
      @keyframes progress-animation {
        0% { width: 30%; }
        50% { width: 70%; }
        100% { width: 30%; }
      }
      
      hr {
        border-color: #30363d;
        margin: 25px 0;
      }
      
      .selectize-dropdown {
        background-color: #161b22;
        border: 1px solid #30363d;
        border-radius: 6px;
        box-shadow: 0 8px 24px rgba(0, 0, 0, 0.4);
      }
      
      .selectize-dropdown-content {
        max-height: 250px;
      }
      
      .selectize-dropdown .option {
        color: #e6edf3;
        padding: 10px 12px;
      }
      
      .selectize-dropdown .option:hover,
      .selectize-dropdown .active {
        background-color: #1f6feb;
        color: #ffffff;
      }
      
      .irs--shiny .irs-bar {
        background: linear-gradient(90deg, #1f6feb 0%, #0969da 100%);
      }
      
      .irs--shiny .irs-handle {
        background-color: #1f6feb;
        border: 2px solid #0d1117;
      }
      
      .checkbox label {
        color: #e6edf3;
        font-weight: 500;
      }
    "))
  ),
  
  div(
    class = "main-header",
    style = "text-align: center;",
    h1("Classement Bump Chart — Championnats de Football"),
    h4("Analyse visuelle de l'évolution du classement")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      div(class = "section-title", "Paramètres"),
      
      selectInput(
        "ligue",
        "Championnat",
        choices = c(
          "Angleterre (Premier League)" = "England",
          "Espagne (La Liga)" = "Spain",
          "Italie (Serie A)" = "Italy",
          "Allemagne (Bundesliga)" = "Germany",
          "France (Ligue 1)" = "France"
        ),
        selected = "England"
      ),
      
      sliderInput(
        "saison",
        "Saison",
        min = 1992,
        max = 2024,
        value = 2023,
        step = 1,
        sep = ""
      ),
      
      numericInput(
        "nb_clubs",
        "Nombre de clubs à mettre en avant",
        value = 6,
        min = 1,
        max = 10,
        step = 1
      ),
      
      uiOutput("selection_clubs"),
      
      checkboxInput(
        "theme_sombre",
        "Thème sombre",
        value = TRUE
      ),
      
      hr(),
      
      actionButton(
        "generer",
        "Générer le graphique",
        class = "btn-primary btn-lg btn-block"
      ),
      
      br(),
      
      downloadButton(
        "telecharger",
        "Télécharger le graphique",
        class = "btn-success btn-block"
      )
    ),
    
    mainPanel(
      width = 9,
      
      conditionalPanel(
        condition = "input.generer == 0",
        div(
          class = "placeholder-container",
          h3("Configurez vos paramètres et générez votre visualisation"),
          p(style = "color: #8b949e; font-size: 1.1em;", 
            "Sélectionnez un championnat, une saison et les clubs à analyser")
        )
      ),
      
      conditionalPanel(
        condition = "input.generer > 0",
        div(
          class = "chart-container",
          
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            div(
              class = "loading-container",
              h3("Génération du graphique en cours..."),
              div(class = "progress",
                  div(class = "progress-bar", style = "width: 100%"))
            )
          ),
          
          plotOutput("bumpchart", height = "750px")
        )
      )
    )
  )
)

# ---- Serveur ----
server <- function(input, output, session) {
  
  clubs_disponibles <- reactiveVal(NULL)
  
  observeEvent(c(input$ligue, input$saison), {
    req(input$ligue, input$saison)
    
    tryCatch({
      df <- tm_matchday_table(
        country_name = input$ligue,
        start_year = input$saison,
        matchday = 1
      )
      
      if(!is.null(df) && nrow(df) > 0) {
        clubs_disponibles(sort(unique(df$squad)))
      }
    }, error = function(e) {
      clubs_disponibles(NULL)
    })
  })
  
  output$selection_clubs <- renderUI({
    req(clubs_disponibles())
    
    nb_clubs <- min(input$nb_clubs, length(clubs_disponibles()))
    
    lapply(1:nb_clubs, function(i) {
      selectInput(
        paste0("club_", i),
        paste("Club", i),
        choices = c("", clubs_disponibles()),
        selected = ""
      )
    })
  })
  
  graphique <- eventReactive(input$generer, {
    req(input$ligue, input$saison)
    
    clubs_selectionnes <- sapply(1:input$nb_clubs, function(i) {
      input[[paste0("club_", i)]]
    })
    clubs_selectionnes <- clubs_selectionnes[clubs_selectionnes != ""]
    
    if(length(clubs_selectionnes) == 0) {
      clubs_selectionnes <- NULL
    }
    
    couleurs_defaut <- c("#ff4757", "#5f27cd", "#00d2d3", "#ff6348", 
                         "#1e90ff", "#ff9ff3", "#feca57", "#48dbfb",
                         "#ee5a6f", "#00d8d6")
    
    withProgress(message = 'Génération en cours...', value = 0, {
      incProgress(0.3, detail = "Récupération des données")
      
      p <- bumpchart_championnat(
        country = input$ligue,
        season = input$saison,
        equipes_focus = clubs_selectionnes,
        couleurs_focus = couleurs_defaut[1:length(clubs_selectionnes)],
        theme_sombre = input$theme_sombre,
        dossier_logos = "Logos clubs de football"
      )
      
      incProgress(0.7, detail = "Finalisation")
      
      return(p)
    })
  })
  
  output$bumpchart <- renderPlot({
    graphique()
  }, res = 120)
  
  output$telecharger <- downloadHandler(
    filename = function() {
      paste0("bumpchart_", input$ligue, "_", input$saison, "_", 
             format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggsave(
        file, 
        plot = graphique(), 
        width = 20,
        height = 12,
        dpi = 400,
        bg = if(input$theme_sombre) "#0d1117" else "#ffffff",
        device = "png",
        type = "cairo"
      )
    }
  )
}

shinyApp(ui = ui, server = server)
