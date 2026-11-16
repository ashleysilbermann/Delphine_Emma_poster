# ============================================================
# APPLICATION R SHINY
# ANALYSE ET COMPREHENSION DES MODELES MIXTES
# Version pédagogique avec export Excel professionnel
# ============================================================


# ============================================================
# 1. INSTALLATION ET CHARGEMENT DES PACKAGES
# ============================================================

packages <- c(
  "shiny",
  "lme4",
  "lmerTest",
  "ggplot2",
  "DT",
  "readxl",
  "openxlsx"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(shiny)
library(lme4)
library(lmerTest)
library(ggplot2)
library(DT)
library(readxl)
library(openxlsx)


# ============================================================
# 2. CREATION DES DONNEES PEDAGOGIQUES
# ============================================================

creer_donnees_exemple <- function() {
  
  set.seed(2026)
  
  echantillons <- sprintf("E%02d", 1:30)
  
  laboratoires <- c(
    "Laboratoire A",
    "Laboratoire B",
    "Laboratoire C"
  )
  
  temperatures <- c(20, 25, 30)
  
  effet_lab <- c(
    "Laboratoire A" = -1.2,
    "Laboratoire B" = 0.5,
    "Laboratoire C" = 1.4
  )
  
  effet_echantillon <- rnorm(
    length(echantillons),
    mean = 0,
    sd = 1.5
  )
  
  names(effet_echantillon) <- echantillons
  
  d <- expand.grid(
    Echantillon = echantillons,
    Laboratoire = laboratoires,
    Temperature = temperatures,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  d$Concentration <- 10 +
    0.45 * d$Temperature +
    effet_lab[d$Laboratoire] +
    effet_echantillon[d$Echantillon] +
    rnorm(nrow(d), 0, 1)
  
  d$Concentration <- round(
    d$Concentration,
    3
  )
  
  d$Echantillon <- factor(d$Echantillon)
  
  d$Laboratoire <- factor(d$Laboratoire)
  
  d
}


# ============================================================
# 3. FONCTION DE CONSTRUCTION DE LA FORMULE
# ============================================================

nom_entre_backticks <- function(x) {
  
  paste0(
    "`",
    gsub("`", "", x, fixed = TRUE),
    "`"
  )
  
}


construire_formule <- function(
    reponse,
    fixes,
    aleatoires
) {
  
  lhs <- nom_entre_backticks(reponse)
  
  if (length(fixes) == 0) {
    
    partie_fixe <- "1"
    
  } else {
    
    partie_fixe <- paste(
      vapply(
        fixes,
        nom_entre_backticks,
        character(1)
      ),
      collapse = " + "
    )
    
  }
  
  partie_aleatoire <- paste(
    vapply(
      aleatoires,
      function(x) {
        
        paste0(
          "(1 | ",
          nom_entre_backticks(x),
          ")"
        )
        
      },
      character(1)
    ),
    collapse = " + "
  )
  
  as.formula(
    paste(
      lhs,
      "~",
      partie_fixe,
      "+",
      partie_aleatoire
    )
  )
  
}


# ============================================================
# 4. INTERFACE UTILISATEUR
# ============================================================

ui <- navbarPage(
  
  title = "Comprendre les modèles mixtes",
  
  id = "navigation",
  
  collapsible = TRUE,
  
  
  # ==========================================================
  # ONGLET 1 : ACCUEIL
  # ==========================================================
  
  tabPanel(
    
    "Accueil",
    
    fluidPage(
      
      br(),
      
      h1("Analyse statistique des modèles mixtes"),
      
      h4(
        "Un outil pédagogique pour comprendre, construire et interpréter un modèle mixte."
      ),
      
      br(),
      
      fluidRow(
        
        column(
          
          4,
          
          wellPanel(
            
            h4("1. Préparer les données"),
            
            p(
              "Importez vos données depuis Excel ou CSV."
            ),
            
            p(
              "Explorez les variables et vérifiez leur structure."
            )
            
          )
          
        ),
        
        column(
          
          4,
          
          wellPanel(
            
            h4("2. Construire le modèle"),
            
            p(
              "Choisissez la variable réponse, les effets fixes et les effets aléatoires."
            ),
            
            p(
              "L'application construit automatiquement la formule statistique."
            )
            
          )
          
        ),
        
        column(
          
          4,
          
          wellPanel(
            
            h4("3. Interpréter les résultats"),
            
            p(
              "Analysez les coefficients, les tests statistiques et les composantes de variance."
            ),
            
            p(
              "Vérifiez les diagnostics et exportez vos résultats."
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      h2("Qu'est-ce qu'un modèle mixte ?"),
      
      p(
        "Un modèle mixte est un modèle statistique qui associe des effets fixes et des effets aléatoires."
      ),
      
      p(
        "Il est particulièrement utile lorsque les observations sont regroupées ou présentent une structure hiérarchique."
      ),
      
      h3("Exemple concret : analyse de mesures en laboratoire"),
      
      p(
        "Imaginons que plusieurs laboratoires analysent plusieurs échantillons à différentes températures."
      ),
      
      p(
        "On souhaite déterminer si la température influence la concentration mesurée."
      ),
      
      p(
        "Cependant, les laboratoires peuvent présenter des différences de fonctionnement et les échantillons peuvent avoir des caractéristiques propres."
      ),
      
      p(
        "Le modèle mixte permet de prendre en compte ces variations."
      ),
      
      br(),
      
      h3("Les trois notions fondamentales"),
      
      fluidRow(
        
        column(
          
          4,
          
          wellPanel(
            
            h4("Variable réponse"),
            
            p(
              "C'est la variable quantitative que l'on cherche à expliquer."
            ),
            
            em("Exemple : concentration en mg/L.")
            
          )
          
        ),
        
        column(
          
          4,
          
          wellPanel(
            
            h4("Effets fixes"),
            
            p(
              "Ce sont les effets moyens des variables explicatives que l'on souhaite étudier."
            ),
            
            em("Exemple : température.")
            
          )
          
        ),
        
        column(
          
          4,
          
          wellPanel(
            
            h4("Effets aléatoires"),
            
            p(
              "Ils représentent les variations associées aux groupes ou unités d'échantillonnage."
            ),
            
            em("Exemple : laboratoire ou échantillon.")
            
          )
          
        )
        
      ),
      
      br(),
      
      h3("Exemple de modèle"),
      
      wellPanel(
        
        h4("Formule mathématique"),
        
        helpText(
          "Yij = β0 + β1 Xij + bj + εij"
        ),
        
        p("Avec :"),
        
        tags$ul(
          
          tags$li("Yij : valeur observée."),
          
          tags$li("β0 : intercept moyen."),
          
          tags$li("β1 : coefficient de l'effet fixe."),
          
          tags$li("Xij : variable explicative."),
          
          tags$li("bj : effet aléatoire du groupe."),
          
          tags$li("εij : erreur résiduelle.")
          
        )
        
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # ONGLET 2 : DONNEES
  # ==========================================================
  
  tabPanel(
    
    "Données",
    
    fluidPage(
      
      br(),
      
      h2("Importation et exploration des données"),
      
      p(
        "Importez votre fichier ou utilisez les données pédagogiques."
      ),
      
      fluidRow(
        
        column(
          
          5,
          
          fileInput(
            
            "fichier",
            
            "Importer un fichier Excel ou CSV",
            
            accept = c(
              ".csv",
              ".xls",
              ".xlsx"
            )
            
          ),
          
          actionButton(
            
            "charger_exemple",
            
            "Charger les données pédagogiques",
            
            class = "btn-primary"
            
          ),
          
          br(),
          br(),
          
          verbatimTextOutput(
            "info_donnees"
          )
          
        ),
        
        column(
          
          7,
          
          h4("Aperçu des données"),
          
          DTOutput(
            "table_donnees"
          )
          
        )
        
      ),
      
      hr(),
      
      h3("Conseils pour préparer les données"),
      
      tags$ul(
        
        tags$li(
          "Chaque ligne doit représenter une observation."
        ),
        
        tags$li(
          "La variable réponse doit être numérique."
        ),
        
        tags$li(
          "Les variables de regroupement doivent identifier les groupes."
        ),
        
        tags$li(
          "Les valeurs manquantes des variables utilisées seront exclues."
        ),
        
        tags$li(
          "Il faut disposer de suffisamment de groupes et de répétitions pour estimer correctement les variances."
          
        )
        
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # ONGLET 3 : CONSTRUCTION DU MODELE
  # ==========================================================
  
  tabPanel(
    
    "Construire le modèle",
    
    fluidPage(
      
      br(),
      
      h2("Construction du modèle statistique"),
      
      p(
        "Sélectionnez les variables nécessaires à votre analyse."
      ),
      
      sidebarLayout(
        
        sidebarPanel(
          
          width = 4,
          
          uiOutput(
            "choix_reponse"
          ),
          
          uiOutput(
            "choix_fixes"
          ),
          
          uiOutput(
            "choix_aleatoires"
          ),
          
          radioButtons(
            
            "estimation",
            
            "Méthode d'estimation",
            
            choices = c(
              
              "REML : estimation des variances" = "REML",
              
              "ML : maximum de vraisemblance" = "ML"
              
            ),
            
            selected = "REML"
            
          ),
          
          actionButton(
            
            "analyser",
            
            "Ajuster le modèle",
            
            class = "btn-success"
            
          )
          
        ),
        
        mainPanel(
          
          width = 8,
          
          h3("Formule statistique"),
          
          verbatimTextOutput(
            "formule_modele"
          ),
          
          h3("Explication de la formule"),
          
          uiOutput(
            "explication_formule"
          ),
          
          h3("Résumé statistique"),
          
          verbatimTextOutput(
            "resume_modele"
          )
          
        )
        
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # ONGLET 4 : RESULTATS
  # ==========================================================
  
  tabPanel(
    
    "Résultats",
    
    fluidPage(
      
      br(),
      
      h2("Résultats statistiques"),
      
      h4("1. Analyse de variance des effets fixes"),
      
      p(
        "Cette analyse permet d'évaluer les effets fixes du modèle."
      ),
      
      DTOutput(
        "table_anova"
      ),
      
      br(),
      
      h4("2. Coefficients estimés"),
      
      p(
        "Les coefficients indiquent le sens et l'amplitude des relations estimées."
      ),
      
      DTOutput(
        "table_coefficients"
      ),
      
      br(),
      
      h4("3. Composantes de variance"),
      
      p(
        "Elles permettent d'évaluer la variabilité entre les groupes et la variabilité résiduelle."
      ),
      
      DTOutput(
        "table_variances"
      ),
      
      br(),
      
      h4("4. Indicateur de regroupement (ICC)"),
      
      verbatimTextOutput(
        "icc"
      ),
      
      p(
        "L'ICC présenté correspond à la part globale de variance attribuable aux intercepts aléatoires, dans cette paramétrisation."
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # ONGLET 5 : DIAGNOSTICS
  # ==========================================================
  
  tabPanel(
    
    "Diagnostics",
    
    fluidPage(
      
      br(),
      
      h2("Diagnostic du modèle"),
      
      p(
        "Les graphiques permettent de vérifier si les hypothèses du modèle sont raisonnablement satisfaites."
      ),
      
      fluidRow(
        
        column(
          
          6,
          
          h4("Résidus vs valeurs ajustées"),
          
          plotOutput(
            "graph_residus",
            height = "320px"
          )
          
        ),
        
        column(
          
          6,
          
          h4("Diagramme quantile-quantile"),
          
          plotOutput(
            "graph_qq",
            height = "320px"
          )
          
        )
        
      ),
      
      fluidRow(
        
        column(
          
          6,
          
          h4("Histogramme des résidus"),
          
          plotOutput(
            "graph_hist",
            height = "320px"
          )
          
        ),
        
        column(
          
          6,
          
          h4("Observé vs ajusté"),
          
          plotOutput(
            "graph_observe",
            height = "320px"
          )
          
        )
        
      ),
      
      br(),
      
      h4("Indicateurs de diagnostic"),
      
      verbatimTextOutput(
        "diagnostic_texte"
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # ONGLET 6 : FORMULES ET NOTIONS
  # ==========================================================
  
  tabPanel(
    
    "Formules & notions",
    
    fluidPage(
      
      br(),
      
      h2("Comprendre les formules statistiques"),
      
      # MODELE
      
      h3("1. Modèle linéaire mixte"),
      
      p(
        "Dans un modèle à intercept aléatoire, on peut écrire :"
      ),
      
      wellPanel(
        
        h4("Yij = β0 + β1 Xij + bj + εij"),
        
        p("Signification des paramètres :"),
        
        tags$ul(
          
          tags$li("Yij : observation mesurée."),
          
          tags$li("β0 : intercept moyen."),
          
          tags$li("β1 : coefficient de régression."),
          
          tags$li("Xij : variable explicative."),
          
          tags$li("bj : effet aléatoire du groupe."),
          
          tags$li("εij : erreur résiduelle.")
          
        )
        
      ),
      
      # HYPOTHESES
      
      h3("2. Hypothèses statistiques"),
      
      p(
        "Les effets aléatoires sont généralement supposés suivre une distribution normale centrée de variance σ²g."
      ),
      
      p(
        "Les erreurs résiduelles sont généralement supposées indépendantes, centrées, de variance constante σ² et approximativement normales pour l'inférence classique."
      ),
      
      # ICC
      
      h3("3. Coefficient de corrélation intraclasse (ICC)"),
      
      wellPanel(
        
        h4("Formule"),
        
        helpText(
          "ICC = σ²g / (σ²g + σ²)"
        ),
        
        p(
          "σ²g représente la variance entre groupes."
        ),
        
        p(
          "σ² représente la variance résiduelle."
        ),
        
        p(
          "L'ICC mesure la ressemblance entre deux observations appartenant au même groupe dans un modèle simple à intercept aléatoire."
        )
        
      ),
      
      # AIC BIC
      
      h3("4. Critères AIC et BIC"),
      
      wellPanel(
        
        h4("AIC"),
        
        helpText(
          "AIC = -2 log(L) + 2k"
        ),
        
        h4("BIC"),
        
        helpText(
          "BIC = -2 log(L) + k log(n)"
        ),
        
        p(
          "L représente la vraisemblance maximisée."
        ),
        
        p(
          "k représente le nombre de paramètres estimés."
        ),
        
        p(
          "n représente le nombre d'observations."
        ),
        
        p(
          "Pour des modèles comparables, les valeurs les plus faibles indiquent un meilleur compromis entre ajustement et complexité."
        )
        
      ),
      
      # REML ML
      
      h3("5. Méthodes d'estimation"),
      
      h4("REML"),
      
      p(
        "La méthode REML (Restricted Maximum Likelihood) permet d'estimer les composantes de variance en tenant compte de la perte de degrés de liberté liée à l'estimation des effets fixes."
      ),
      
      h4("ML"),
      
      p(
        "La méthode ML (Maximum Likelihood) estime les paramètres par maximisation de la vraisemblance. Elle est notamment utilisée pour comparer des modèles ayant des effets fixes différents."
      ),
      
      # ANOVA
      
      h3("6. Tests des effets fixes"),
      
      p(
        "L'ANOVA permet d'étudier si les effets fixes apportent une information statistiquement significative."
      ),
      
      p(
        "Une p-value inférieure à 0,05 est souvent considérée comme un indice statistique contre l'hypothèse nulle d'absence d'effet, sous les hypothèses du modèle."
      ),
      
      p(
        "Attention : une p-value ne mesure pas l'importance pratique d'un effet et ne démontre pas une relation causale."
      ),
      
      # DIAGNOSTICS
      
      h3("7. Diagnostics"),
      
      tags$ul(
        
        tags$li(
          
          strong("Résidus vs valeurs ajustées : "),
          
          "permet de rechercher une structure dans les erreurs et une éventuelle hétéroscédasticité."
          
        ),
        
        tags$li(
          
          strong("QQ-plot : "),
          
          "permet d'évaluer la normalité approximative des résidus."
          
        ),
        
        tags$li(
          
          strong("Histogramme : "),
          
          "permet d'observer la distribution des résidus."
          
        ),
        
        tags$li(
          
          strong("Observé vs ajusté : "),
          
          "permet d'apprécier visuellement la qualité de l'ajustement."
          
        )
        
      ),
      
      # LIMITES
      
      h3("8. Limites de l'application"),
      
      p(
        "Cette application utilise des modèles linéaires mixtes gaussiens avec intercepts aléatoires."
      ),
      
      p(
        "Elle ne prend pas encore en charge les modèles mixtes généralisés, les pentes aléatoires, les corrélations temporelles ou les structures de covariance complexes."
      ),
      
      p(
        "La structure des effets aléatoires doit être choisie en fonction du protocole expérimental."
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # ONGLET 7 : EXPORT PROFESSIONNEL
  # ==========================================================
  
  tabPanel(
    
    "Export",
    
    fluidPage(
      
      br(),
      
      h2("Rapport statistique professionnel"),
      
      p(
        "Téléchargez un classeur Excel regroupant les résultats de votre analyse."
      ),
      
      br(),
      
      fluidRow(
        
        column(
          
          6,
          
          wellPanel(
            
            h3("Export Excel"),
            
            p(
              "Un classeur organisé en cinq feuilles avec une présentation homogène."
            ),
            
            tags$ul(
              
              tags$li("Synthèse du modèle"),
              
              tags$li("Coefficients estimés"),
              
              tags$li("Analyse de variance"),
              
              tags$li("Composantes de variance"),
              
              tags$li("Diagnostics statistiques")
              
            ),
            
            downloadButton(
              
              "export_excel",
              
              "Télécharger le rapport Excel",
              
              class = "btn-success",
              
              style = "width:100%"
              
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      h3("Contenu du rapport"),
      
      tableOutput(
        "contenu_export"
      )
      
    )
    
  )
  
)


# ============================================================
# 5. SERVEUR
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # STOCKAGE DES DONNEES
  # ----------------------------------------------------------
  
  donnees <- reactiveVal(
    creer_donnees_exemple()
  )
  
  
  # ----------------------------------------------------------
  # IMPORTATION DES FICHIERS
  # ----------------------------------------------------------
  
  observeEvent(input$fichier, {
    
    req(input$fichier)
    
    extension <- tolower(
      tools::file_ext(
        input$fichier$name
      )
    )
    
    df <- tryCatch({
      
      if (extension == "csv") {
        
        read.csv(
          input$fichier$datapath,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
      } else {
        
        as.data.frame(
          read_excel(
            input$fichier$datapath
          )
        )
        
      }
      
    }, error = function(e) {
      
      showNotification(
        paste(
          "Erreur de lecture :",
          e$message
        ),
        type = "error"
      )
      
      NULL
      
    })
    
    if (!is.null(df)) {
      
      names(df) <- make.unique(
        names(df)
      )
      
      donnees(df)
      
      showNotification(
        "Importation réussie.",
        type = "message"
      )
      
    }
    
  })
  
  
  # ----------------------------------------------------------
  # CHARGEMENT DES DONNEES PEDAGOGIQUES
  # ----------------------------------------------------------
  
  observeEvent(input$charger_exemple, {
    
    donnees(
      creer_donnees_exemple()
    )
    
    showNotification(
      "Données pédagogiques chargées.",
      type = "message"
    )
    
  })
  
  
  # ----------------------------------------------------------
  # INFORMATIONS GENERALES
  # ----------------------------------------------------------
  
  output$info_donnees <- renderPrint({
    
    d <- donnees()
    
    cat(
      "Nombre de lignes :",
      nrow(d),
      "\n"
    )
    
    cat(
      "Nombre de colonnes :",
      ncol(d),
      "\n\n"
    )
    
    cat("Types des variables :\n")
    
    print(
      sapply(
        d,
        function(x) class(x)[1]
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # TABLEAU DES DONNEES
  # ----------------------------------------------------------
  
  output$table_donnees <- renderDT({
    
    datatable(
      
      donnees(),
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # CHOIX DE LA VARIABLE REPONSE
  # ----------------------------------------------------------
  
  output$choix_reponse <- renderUI({
    
    d <- donnees()
    
    numeriques <- names(d)[
      vapply(
        d,
        is.numeric,
        logical(1)
      )
    ]
    
    selectInput(
      
      "reponse",
      
      "Variable réponse quantitative",
      
      choices = numeriques,
      
      selected = if (
        "Concentration" %in% numeriques
      ) {
        
        "Concentration"
        
      } else if (
        length(numeriques) > 0
      ) {
        
        numeriques[1]
        
      } else {
        
        character(0)
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # CHOIX DES EFFETS FIXES
  # ----------------------------------------------------------
  
  output$choix_fixes <- renderUI({
    
    d <- donnees()
    
    choix <- setdiff(
      names(d),
      input$reponse
    )
    
    selectizeInput(
      
      "fixes",
      
      "Effets fixes",
      
      choices = choix,
      
      multiple = TRUE,
      
      selected = intersect(
        "Temperature",
        choix
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # CHOIX DES EFFETS ALEATOIRES
  # ----------------------------------------------------------
  
  output$choix_aleatoires <- renderUI({
    
    d <- donnees()
    
    choix <- setdiff(
      names(d),
      input$reponse
    )
    
    selectizeInput(
      
      "aleatoires",
      
      "Effets aléatoires (groupes)",
      
      choices = choix,
      
      multiple = TRUE,
      
      selected = intersect(
        c(
          "Echantillon",
          "Laboratoire"
        ),
        choix
      )
      
    )
    
  })
  
  
  # ==========================================================
  # AJUSTEMENT DU MODELE
  # ==========================================================
  
  resultat_modele <- eventReactive(
    input$analyser,
    {
      
      req(input$reponse)
      
      req(input$aleatoires)
      
      df <- donnees()
      
      reponse <- input$reponse
      
      fixes <- input$fixes
      
      aleatoires <- input$aleatoires
      
      validate(
        
        need(
          reponse %in% names(df),
          "Variable réponse invalide."
        ),
        
        need(
          is.numeric(df[[reponse]]),
          "La variable réponse doit être numérique."
        ),
        
        need(
          length(aleatoires) >= 1,
          "Choisissez au moins un effet aléatoire."
        ),
        
        need(
          !reponse %in% aleatoires,
          "La variable réponse ne peut pas être un effet aléatoire."
        ),
        
        need(
          !reponse %in% fixes,
          "La variable réponse ne peut pas être un effet fixe."
        )
        
      )
      
      variables <- unique(
        c(
          reponse,
          fixes,
          aleatoires
        )
      )
      
      d <- df[
        ,
        variables,
        drop = FALSE
      ]
      
      d <- d[
        complete.cases(d),
        ,
        drop = FALSE
      ]
      
      validate(
        
        need(
          nrow(d) >= 5,
          "Pas assez de données disponibles."
        )
        
      )
      
      # Conversion des effets aléatoires
      
      for (v in aleatoires) {
        
        d[[v]] <- droplevels(
          as.factor(d[[v]])
        )
        
        validate(
          
          need(
            nlevels(d[[v]]) >= 2,
            paste(
              "Le facteur",
              v,
              "doit avoir au moins deux groupes."
            )
          )
          
        )
        
      }
      
      # Conversion des variables fixes qualitatives
      
      for (v in fixes) {
        
        if (
          is.character(d[[v]]) ||
          is.logical(d[[v]])
        ) {
          
          d[[v]] <- as.factor(d[[v]])
          
        }
        
        if (is.factor(d[[v]])) {
          
          d[[v]] <- droplevels(d[[v]])
          
        }
        
      }
      
      # Construction de la formule
      
      f <- construire_formule(
        reponse,
        fixes,
        aleatoires
      )
      
      # Ajustement du modèle
      
      fit <- tryCatch(
        
        lmerTest::lmer(
          
          f,
          
          data = d,
          
          REML = identical(
            input$estimation,
            "REML"
          )
          
        ),
        
        error = function(e) e
        
      )
      
      validate(
        
        need(
          !inherits(fit, "error"),
          
          if (inherits(fit, "error")) {
            paste(
              "Erreur pendant l'ajustement :",
              fit$message
            )
          } else {
            ""
          }
          
        )
        
      )
      
      list(
        
        fit = fit,
        
        data = d,
        
        formule = f,
        
        formule_texte = paste(
          deparse(f),
          collapse = ""
        ),
        
        reponse = reponse,
        
        fixes = fixes,
        
        aleatoires = aleatoires
        
      )
      
    }
    
  )
  
  
  # ==========================================================
  # FORMULE DU MODELE
  # ==========================================================
  
  output$formule_modele <- renderPrint({
    
    m <- resultat_modele()
    
    cat(
      m$formule_texte
    )
    
  })
  
  
  # ==========================================================
  # EXPLICATION DU MODELE
  # ==========================================================
  
  output$explication_formule <- renderUI({
    
    m <- resultat_modele()
    
    fixe_txt <- if (
      length(m$fixes) > 0
    ) {
      
      paste(
        m$fixes,
        collapse = ", "
      )
      
    } else {
      
      "Aucun effet fixe explicatif"
      
    }
    
    alea_txt <- paste(
      m$aleatoires,
      collapse = ", "
    )
    
    tagList(
      
      p(
        
        strong("Variable réponse : "),
        
        m$reponse,
        
        " : variable quantitative que le modèle cherche à expliquer."
        
      ),
      
      p(
        
        strong("Effets fixes : "),
        
        fixe_txt,
        
        " : effets moyens étudiés."
        
      ),
      
      p(
        
        strong("Effets aléatoires : "),
        
        alea_txt,
        
        " : variabilité liée aux groupes."
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # RESUME DU MODELE
  # ==========================================================
  
  output$resume_modele <- renderPrint({
    
    m <- resultat_modele()
    
    print(
      summary(m$fit)
    )
    
    if (
      isSingular(
        m$fit,
        tol = 1e-4
      )
    ) {
      
      cat(
        "\nATTENTION : modèle singulier détecté.",
        "\nLa structure aléatoire doit être examinée."
      )
      
    }
    
  })
  
  
  # ==========================================================
  # ANOVA
  # ==========================================================
  
  obtenir_anova <- function(fit) {
    
    a <- as.data.frame(
      
      anova(
        fit,
        type = 3,
        ddf = "Satterthwaite"
      )
      
    )
    
    a$Effet <- rownames(a)
    
    rownames(a) <- NULL
    
    a <- a[
      ,
      c(
        "Effet",
        setdiff(
          names(a),
          "Effet"
        )
      ),
      drop = FALSE
    ]
    
    a
    
  }
  
  
  output$table_anova <- renderDT({
    
    m <- resultat_modele()
    
    a <- obtenir_anova(m$fit)
    
    datatable(
      
      a,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
      
    )
    
  })
  
  
  # ==========================================================
  # COEFFICIENTS
  # ==========================================================
  
  obtenir_coefficients <- function(fit) {
    
    tab <- as.data.frame(
      coef(
        summary(fit)
      )
    )
    
    tab$Terme <- rownames(tab)
    
    rownames(tab) <- NULL
    
    tab <- tab[
      ,
      c(
        "Terme",
        setdiff(
          names(tab),
          "Terme"
        )
      ),
      drop = FALSE
    ]
    
    tab
    
  }
  
  
  output$table_coefficients <- renderDT({
    
    m <- resultat_modele()
    
    tab <- obtenir_coefficients(
      m$fit
    )
    
    datatable(
      
      tab,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
      
    )
    
  })
  
  
  # ==========================================================
  # VARIANCES
  # ==========================================================
  
  obtenir_variances <- function(fit) {
    
    as.data.frame(
      VarCorr(fit)
    )
    
  }
  
  
  output$table_variances <- renderDT({
    
    m <- resultat_modele()
    
    tab <- obtenir_variances(
      m$fit
    )
    
    datatable(
      
      tab,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
      
    )
    
  })
  
  
  # ==========================================================
  # ICC
  # ==========================================================
  
  output$icc <- renderPrint({
    
    m <- resultat_modele()
    
    vc <- as.data.frame(
      VarCorr(m$fit)
    )
    
    var_groupes <- sum(
      
      vc$vcov[
        vc$grp != "Residual" &
          is.na(vc$var2)
      ],
      
      na.rm = TRUE
      
    )
    
    var_residuelle <- sigma(m$fit)^2
    
    total <- var_groupes + var_residuelle
    
    if (total > 0) {
      
      valeur <- var_groupes / total
      
      cat(
        "Part de variance associée aux intercepts aléatoires :",
        round(valeur, 3),
        "\n"
      )
      
      cat(
        "Pourcentage :",
        round(valeur * 100, 2),
        "%\n"
      )
      
    } else {
      
      cat(
        "Indicateur non calculable."
      )
      
    }
    
  })
  
  
  # ==========================================================
  # DIAGNOSTIC 1 : RESIDUS
  # ==========================================================
  
  output$graph_residus <- renderPlot({
    
    m <- resultat_modele()
    
    d <- data.frame(
      
      Ajustees = fitted(m$fit),
      
      Residus = resid(m$fit)
      
    )
    
    ggplot(
      
      d,
      
      aes(
        x = Ajustees,
        y = Residus
      )
      
    ) +
      
      geom_point(
        alpha = 0.65
      ) +
      
      geom_hline(
        yintercept = 0,
        linetype = 2
      ) +
      
      geom_smooth(
        method = "loess",
        se = FALSE
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Valeurs ajustées",
        y = "Résidus"
      )
    
  })
  
  
  # ==========================================================
  # DIAGNOSTIC 2 : QQ PLOT
  # ==========================================================
  
  output$graph_qq <- renderPlot({
    
    m <- resultat_modele()
    
    d <- data.frame(
      Residus = resid(m$fit)
    )
    
    ggplot(
      
      d,
      
      aes(
        sample = Residus
      )
      
    ) +
      
      stat_qq() +
      
      stat_qq_line() +
      
      theme_minimal() +
      
      labs(
        title = "QQ-plot des résidus"
      )
    
  })
  
  
  # ==========================================================
  # DIAGNOSTIC 3 : HISTOGRAMME
  # ==========================================================
  
  output$graph_hist <- renderPlot({
    
    m <- resultat_modele()
    
    d <- data.frame(
      Residus = resid(m$fit)
    )
    
    ggplot(
      
      d,
      
      aes(
        x = Residus
      )
      
    ) +
      
      geom_histogram(
        bins = 20,
        color = "white"
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Résidus",
        y = "Effectif"
      )
    
  })
  
  
  # ==========================================================
  # DIAGNOSTIC 4 : OBSERVE VS AJUSTE
  # ==========================================================
  
  output$graph_observe <- renderPlot({
    
    m <- resultat_modele()
    
    d <- data.frame(
      
      Observe = m$data[[m$reponse]],
      
      Ajuste = fitted(m$fit)
      
    )
    
    ggplot(
      
      d,
      
      aes(
        x = Observe,
        y = Ajuste
      )
      
    ) +
      
      geom_point(
        alpha = 0.65
      ) +
      
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = 2
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Valeurs observées",
        y = "Valeurs ajustées"
      )
    
  })
  
  
  # ==========================================================
  # INDICATEURS DE DIAGNOSTIC
  # ==========================================================
  
  output$diagnostic_texte <- renderPrint({
    
    m <- resultat_modele()
    
    fit <- m$fit
    
    cat(
      "Nombre d'observations :",
      nobs(fit),
      "\n"
    )
    
    cat(
      "AIC :",
      round(AIC(fit), 3),
      "\n"
    )
    
    cat(
      "BIC :",
      round(BIC(fit), 3),
      "\n"
    )
    
    cat(
      "Log-vraisemblance :",
      round(
        as.numeric(logLik(fit)),
        3
      ),
      "\n"
    )
    
    cat(
      "Variance résiduelle :",
      round(sigma(fit)^2, 4),
      "\n"
    )
    
    cat(
      "Modèle singulier :",
      
      ifelse(
        isSingular(fit),
        "Oui",
        "Non"
      ),
      
      "\n"
    )
    
    cat(
      "\nInterprétation :\n"
    )
    
    cat(
      "- Une structure en éventail peut signaler une variance non constante.\n"
    )
    
    cat(
      "- Des écarts importants à la droite du QQ-plot peuvent signaler une non-normalité des résidus.\n"
    )
    
    cat(
      "- Une courbure dans le graphique des résidus peut révéler une structure non expliquée.\n"
    )
    
  })
  
  
  # ==========================================================
  # EXPORT : CONTENU DU RAPPORT
  # ==========================================================
  
  output$contenu_export <- renderTable({
    
    data.frame(
      
      Feuille = c(
        "Synthèse",
        "Coefficients",
        "ANOVA",
        "Variances",
        "Diagnostics"
      ),
      
      Contenu = c(
        
        "Présentation générale du modèle",
        
        "Estimations, erreurs standards et tests",
        
        "Tests statistiques des effets fixes",
        
        "Composantes de variance",
        
        "AIC, BIC, résidus et indicateurs"
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # EXPORT EXCEL PROFESSIONNEL
  # ==========================================================
  
  output$export_excel <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Rapport_Modeles_Mixtes_",
        Sys.Date(),
        ".xlsx"
      )
      
    },
    
    content = function(file) {
      
      m <- resultat_modele()
      
      fit <- m$fit
      
      # ------------------------------------------------------
      # CREATION DU CLASSEUR
      # ------------------------------------------------------
      
      wb <- createWorkbook()
      
      
      # ------------------------------------------------------
      # STYLES
      # ------------------------------------------------------
      
      titre_style <- createStyle(
        
        fontSize = 16,
        
        fontColour = "#FFFFFF",
        
        fgFill = "#17365D",
        
        textDecoration = "bold",
        
        halign = "left",
        
        valign = "center"
        
      )
      
      entete_style <- createStyle(
        
        fontSize = 11,
        
        fontColour = "#FFFFFF",
        
        fgFill = "#305496",
        
        textDecoration = "bold",
        
        halign = "center",
        
        valign = "center",
        
        border = "Bottom",
        
        wrapText = TRUE
        
      )
      
      normal_style <- createStyle(
        
        fontSize = 10,
        
        valign = "center"
        
      )
      
      nombre_style <- createStyle(
        
        numFmt = "0.000",
        
        halign = "center"
        
      )
      
      note_style <- createStyle(
        
        fontColour = "#666666",
        
        italic = TRUE,
        
        wrapText = TRUE
        
      )
      
      sous_titre_style <- createStyle(
        
        fontSize = 12,
        
        fontColour = "#FFFFFF",
        
        fgFill = "#4472C4",
        
        textDecoration = "bold"
        
      )
      
      
      # ------------------------------------------------------
      # FONCTION DE CREATION DES FEUILLES
      # ------------------------------------------------------
      
      creer_feuille <- function(
    nom,
    titre,
    donnees
      ) {
        
        addWorksheet(
          wb,
          nom
        )
        
        ncolonne <- max(
          1,
          ncol(donnees)
        )
        
        writeData(
          wb,
          nom,
          titre,
          startRow = 1,
          startCol = 1
        )
        
        mergeCells(
          wb,
          nom,
          cols = 1:ncolonne,
          rows = 1
        )
        
        addStyle(
          wb,
          nom,
          titre_style,
          rows = 1,
          cols = 1:ncolonne,
          gridExpand = TRUE
        )
        
        setRowHeights(
          wb,
          nom,
          rows = 1,
          heights = 32
        )
        
        writeData(
          wb,
          nom,
          donnees,
          startRow = 3,
          withFilter = nrow(donnees) > 0
        )
        
        if (ncol(donnees) > 0) {
          
          addStyle(
            wb,
            nom,
            entete_style,
            rows = 3,
            cols = 1:ncol(donnees),
            gridExpand = TRUE
          )
          
          if (nrow(donnees) > 0) {
            
            addStyle(
              wb,
              nom,
              normal_style,
              rows = 4:(3 + nrow(donnees)),
              cols = 1:ncol(donnees),
              gridExpand = TRUE,
              stack = TRUE
            )
            
          }
          
          setColWidths(
            wb,
            nom,
            cols = 1:ncol(donnees),
            widths = "auto"
          )
          
          freezePane(
            wb,
            nom,
            firstActiveRow = 4
          )
          
        }
        
      }
      
      
      # ======================================================
      # FEUILLE 1 : SYNTHESE
      # ======================================================
      
      synthese <- data.frame(
        
        Indicateur = c(
          
          "Variable réponse",
          
          "Effets fixes",
          
          "Effets aléatoires",
          
          "Méthode d'estimation",
          
          "Nombre d'observations",
          
          "AIC",
          
          "BIC",
          
          "Log-vraisemblance",
          
          "Variance résiduelle",
          
          "Modèle singulier"
          
        ),
        
        Resultat = c(
          
          m$reponse,
          
          if (length(m$fixes) > 0) {
            paste(m$fixes, collapse = ", ")
          } else {
            "Aucun"
          },
          
          paste(
            m$aleatoires,
            collapse = ", "
          ),
          
          if (isREML(fit)) {
            "REML"
          } else {
            "ML"
          },
          
          as.character(nobs(fit)),
          
          as.character(round(AIC(fit), 3)),
          
          as.character(round(BIC(fit), 3)),
          
          as.character(
            round(
              as.numeric(logLik(fit)),
              3
            )
          ),
          
          as.character(
            round(
              sigma(fit)^2,
              4
            )
          ),
          
          ifelse(
            isSingular(fit),
            "Oui - à examiner",
            "Non"
          )
          
        ),
        
        stringsAsFactors = FALSE
        
      )
      
      creer_feuille(
        
        "Synthese",
        
        "RAPPORT STATISTIQUE - MODELES MIXTES",
        
        synthese
        
      )
      
      writeData(
        wb,
        "Synthese",
        "FORMULE DU MODELE",
        startRow = 15
      )
      
      addStyle(
        wb,
        "Synthese",
        sous_titre_style,
        rows = 15,
        cols = 1:2,
        gridExpand = TRUE
      )
      
      writeData(
        wb,
        "Synthese",
        m$formule_texte,
        startRow = 16
      )
      
      writeData(
        wb,
        "Synthese",
        "Date de génération",
        startRow = 18
      )
      
      writeData(
        wb,
        "Synthese",
        as.character(Sys.Date()),
        startRow = 18,
        startCol = 2
      )
      
      setColWidths(
        wb,
        "Synthese",
        cols = 1:2,
        widths = c(32, 65)
      )
      
      
      # ======================================================
      # FEUILLE 2 : COEFFICIENTS
      # ======================================================
      
      coefficients <- obtenir_coefficients(fit)
      
      creer_feuille(
        
        "Coefficients",
        
        "ESTIMATION DES COEFFICIENTS FIXES",
        
        coefficients
        
      )
      
      
      # ======================================================
      # FEUILLE 3 : ANOVA
      # ======================================================
      
      anova_tab <- obtenir_anova(fit)
      
      creer_feuille(
        
        "ANOVA",
        
        "ANALYSE DE VARIANCE - EFFETS FIXES",
        
        anova_tab
        
      )
      
      ligne_note <- nrow(anova_tab) + 6
      
      writeData(
        
        wb,
        
        "ANOVA",
        
        paste(
          "INTERPRETATION : les p-values permettent",
          "d'évaluer les éléments de preuve contre",
          "l'hypothèse nulle d'absence d'effet."
        ),
        
        startRow = ligne_note
        
      )
      
      addStyle(
        
        wb,
        
        "ANOVA",
        
        note_style,
        
        rows = ligne_note,
        
        cols = 1:6,
        
        gridExpand = TRUE
        
      )
      
      
      # ======================================================
      # FEUILLE 4 : VARIANCES
      # ======================================================
      
      variances <- obtenir_variances(fit)
      
      creer_feuille(
        
        "Variances",
        
        "COMPOSANTES DE VARIANCE",
        
        variances
        
      )
      
      
      # ======================================================
      # FEUILLE 5 : DIAGNOSTICS
      # ======================================================
      
      residus <- resid(fit)
      
      diagnostic <- data.frame(
        
        Indicateur = c(
          
          "Nombre d'observations",
          
          "Moyenne des résidus",
          
          "Ecart-type des résidus",
          
          "Variance résiduelle",
          
          "AIC",
          
          "BIC",
          
          "Log-vraisemblance",
          
          "Modèle singulier"
          
        ),
        
        Valeur = c(
          
          as.character(nobs(fit)),
          
          as.character(round(mean(residus), 4)),
          
          as.character(round(sd(residus), 4)),
          
          as.character(round(sigma(fit)^2, 4)),
          
          as.character(round(AIC(fit), 3)),
          
          as.character(round(BIC(fit), 3)),
          
          as.character(
            round(
              as.numeric(logLik(fit)),
              3
            )
          ),
          
          ifelse(
            isSingular(fit),
            "Oui",
            "Non"
          )
          
        )
        
      )
      
      creer_feuille(
        
        "Diagnostics",
        
        "INDICATEURS DE DIAGNOSTIC",
        
        diagnostic
        
      )
      
      writeData(
        
        wb,
        
        "Diagnostics",
        
        "AIDE A L'INTERPRETATION",
        
        startRow = 15
        
      )
      
      addStyle(
        
        wb,
        
        "Diagnostics",
        
        sous_titre_style,
        
        rows = 15,
        
        cols = 1:2,
        
        gridExpand = TRUE
        
      )
      
      notes <- c(
        
        "QQ-plot : examiner la normalité approximative des résidus.",
        
        "Résidus vs ajustés : examiner la présence d'une structure ou d'une variance non constante.",
        
        "Histogramme : observer la distribution des résidus.",
        
        "Un modèle singulier peut signaler une structure aléatoire trop complexe."
        
      )
      
      for (i in seq_along(notes)) {
        
        writeData(
          
          wb,
          
          "Diagnostics",
          
          notes[i],
          
          startRow = 15 + i,
          
          startCol = 1
          
        )
        
      }
      
      addStyle(
        
        wb,
        
        "Diagnostics",
        
        note_style,
        
        rows = 16:19,
        
        cols = 1:2,
        
        gridExpand = TRUE
        
      )
      
      
      # ======================================================
      # FINITION DU CLASSEUR
      # ======================================================
      
      couleurs <- c(
        
        "Synthese" = "#17365D",
        
        "Coefficients" = "#4472C4",
        
        "ANOVA" = "#70AD47",
        
        "Variances" = "#ED7D31",
        
        "Diagnostics" = "#A5A5A5"
        
      )
      
      for (nom in names(couleurs)) {
        
        setTabColour(
          
          wb,
          
          nom,
          
          couleurs[nom]
          
        )
        
      }
      
      # Sauvegarde du fichier
      
      saveWorkbook(
        
        wb,
        
        file,
        
        overwrite = TRUE
        
      )
      
    }
    
  )
  
}


# ============================================================
# 6. LANCEMENT DE L'APPLICATION
# ============================================================

shinyApp(
  
  ui = ui,
  
  server = server
  
)
