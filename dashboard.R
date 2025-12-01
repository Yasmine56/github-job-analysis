library(shiny)
library(bslib)
library(dplyr)   # For data aggregation in the trends graph
library(ggplot2) # For both graphs

# UI
ui <- page_sidebar(
  title = tagList(
    # Conteneur du titre
    span("Evolution des langages de programmation", 
         style = "font-size: 22px; font-weight: bold;"),
    
    # Bouton positionné tout à droite
    tags$head(
      tags$style(HTML("
    #main_content {
      width: 100% !important;
      margin-right: 0 !important;
    }
  ")),
      tags$style(HTML("
        #code-btn {
          position: fixed;
          top: 10px;
          right: 20px;
          z-index: 9999;
        }
      "))
    ),
    a("</> Code source", id = "code-btn", href = "https://qqchose", target = "_blank",
      class = "btn btn-sm btn-outline-primary")
  ),
  
  
  
  sidebar = sidebar(
    h4("Menu"),
    tags$style(HTML("
      ul {
        list-style-type: none;
        padding-left: 0;
      }
 
      li {
        margin-bottom: 10px;
      }
 
      h4 {
        color: #007BFF; /* Bleu Bootstrap */
        font-weight: bold;
        text-align: center;
        margin-bottom: 20px;
      }
 
      .action-button {
        width: 100%;
        height: 50px;
        text-align: center;
        border-radius: 10px;
        background-color: #f0f0f0; /* Fond gris clair */
        border: 1px solid #ccc;
        color: #333;
        font-weight: 600;
        box-shadow: 1px 1px 3px rgba(0,0,0,0.05);
        transition: background-color 0.2s ease, transform 0.1s ease;
      }
 
      .action-button:hover {
        background-color: #e0e0e0;
        transform: scale(1.02);
      }
 
      .sidebar ul li {
        width: 100%;
      }
    ")),
    tags$ul(
      tags$li(actionButton("show_histogram", "Présentation")),
      tags$li(actionButton("show_bdd", "Base de données")),
      tags$li(actionButton("show_visu", "Visualisation")),
      tags$li(actionButton("show_analyse", "Analyse")),
      tags$li(actionButton("show_ccl", "Conclusion"))
    )
  ),
  
  mainPanel(
    uiOutput("main_content")
  ),
  
  theme = bs_theme(version = 5, bootswatch = "cerulean")
)


# Server
server <- function(input, output, session) {
  # Global reactive to read and process the CSV data
  projectsData <- reactive({
    df <- read.csv("donnees_github.csv", stringsAsFactors = FALSE)
    # Convert the date column into Date objects (adjust the format if necessary)
    df$date <- as.Date(as.POSIXct(df$date, format = "%Y-%m-%dT%H:%M:%SZ"))
    df
  })
  
  # Reactive: Read and process the job offers CSV file
  jobOffersData <- reactive({
    df <- read.csv("job_offers.csv", stringsAsFactors = FALSE)
    # Remove rows with empty programming_languages
    df <- df[df$programming_languages != "", ]
    
    # Split the comma separated programming_languages into individual rows
    # and clean the strings (requires tidyr and stringr)
    df_long <- df %>%
      mutate(programming_languages = strsplit(as.character(programming_languages), ",")) %>%
      tidyr::unnest(programming_languages) %>%
      mutate(language = trimws(programming_languages))
    
    df_long
  })
  
  # Affichage conditionnel des sous-onglets dans la sidebar
  output$sub_visu_buttons <- renderUI({
    req(input$show_visu > 0)
    tagList(
      tags$ul(
        tags$li(actionButton("visu_select", "Evolution")),
        tags$li(actionButton("visu_date", "Date"))
      )
    )
  })
  
  observeEvent(input$show_histogram, {
    output$main_content <- renderUI({
      tags$div(
        style = "display: flex; flex-direction: row; gap: 1em; height: 100%;",
        
        # Panel Présentation (gauche - rectangle)
        tags$div(
          style = "
          flex: 2; 
          background-color: #f9f9f9;
          border-radius: 12px; 
          padding: 1.5em; 
          box-shadow: 0 0 10px rgba(0,0,0,0.1); 
          overflow-y: auto;
        ",
          HTML("
          <style>
            .presentation-texte {
              text-align: justify;
              text-indent: 2em;
              line-height: 1.6;
              width: 100%;
            }
            .presentation-texte ul {
              list-style-type: disc;
              padding-left: 2em;
              margin-top: 0.5em;
            }
            .presentation-texte li {
              text-indent: 0;
              margin-bottom: 0.5em;
            }
          </style>
 
          <div class='presentation-texte'>
            <h3 style='text-align: center;'>Présentation</h3>
 
            <p>
              Ce projet a pour objectif d’analyser si les projets publics créés sur GitHub en 2025 
              reflètent les compétences demandées sur le marché du travail, tel qu’il est représenté 
              par les offres publiées sur la plateforme <em>Welcome to the Jungle</em>.
            </p>
 
            <p>
              GitHub, avec ses millions de projets open source, permet d’observer les technologies 
              les plus utilisées dans la communauté des développeurs. À l’inverse, Welcome to the Jungle 
              donne un aperçu des compétences actuellement recherchées par les employeurs.
            </p>
 
            <div style='display: flex; justify-content: center; gap: 20px; margin: 20px 0; width: 100%;'>
              <img src='git.png' style='width: 100%; max-width: 45%; height: auto; border-radius: 10px;'>
              <img src='welcome3.png' style='width: 100%; max-width: 45%; height: auto; border-radius: 10px;'>
            </div>
 
            <p>
              Cette analyse s’adresse notamment aux étudiants et aux établissements de formation.
            </p>
 
            <ul>
              <li>Comparer les langages les plus utilisés sur GitHub avec ceux les plus demandés</li>
              <li>Analyser les disparités régionales dans la popularité des technologies</li>
              <li>Croiser avec les données de Google Trends</li>
            </ul>
 
            <p>
              En croisant ces sources, le projet cherche à mieux comprendre les dynamiques entre innovation technologique 
              et besoins professionnels.
            </p>
          </div>
        ")
        ),
        
        # Colonne droite avec deux panels empilés
        tags$div(
          style = "flex: 1; display: flex; flex-direction: column; gap: 1em;",
          
          # Panel Population (haut - carré)
          tags$div(
            style = "
            flex: 1;
            background-color: #f1f1f1;
            border-radius: 12px; 
            padding: 1em; 
            box-shadow: 0 0 10px rgba(0,0,0,0.1); 
            overflow-y: auto;
          ",
            HTML("
            <div class='presentation-texte'>
              <h4>Population</h4>
              <p>
                La population d’intérêt se compose de trois catégories :
              </p>
              <ul>
                <li><strong>Utilisateurs de GitHub</strong> : développeurs publiant et interagissant avec des projets open source.</li>
                <li><strong>Recruteurs</strong> sur <em>Welcome to the Jungle</em>, publiant des offres d’emploi tech.</li>
                <li><strong>Utilisateurs de Google Trends</strong>, dont les recherches reflètent l’intérêt pour certaines technologies.</li>
              </ul>
              <p>
                L’étude vise à comprendre comment ces trois populations représentent la popularité des langages.
              </p>
            </div>
          ")
          ),
          
          # Panel Taille d’échantillon (bas - carré)
          tags$div(
            style = "
            flex: 1;
            background-color: #f1f1f1;
            border-radius: 12px; 
            padding: 1em; 
            box-shadow: 0 0 10px rgba(0,0,0,0.1); 
            overflow-y: auto;
          ",
            HTML("
            <div class='presentation-texte'>
              <h5>Taille d'échantillon</h5>
              <p>
                La taille de l’échantillon a été définie selon :
              </p>
              <ul>
                <li><strong>6000 projets GitHub</strong> créés entre janvier et mars 2025.</li>
                <li><strong>Référence</strong> : une étude sur 3800 dépôts.</li>
                <li><strong>393 offres d'emploi</strong> extraites via scraping Selenium.</li>
                <li><strong>92 annonces</strong> contenaient des langages exploitables.</li>
              </ul>
            </div>
          ")
          )
        )
      )
    })
  })
  
  
  
  
  
  
  
  
  # Base de données
  observeEvent(input$show_bdd, {
    output$main_content <- renderUI({
      tabsetPanel(
        id = "visu_tabs",
        tabPanel("Données Github",
                 tagList(
                   h5("Données chargées depuis le fichier 'donnees_github.csv'"),
                   DT::dataTableOutput("csv_table_github")
                 )
        ),
        tabPanel("Données jobs",
                 tagList(
                   h5("Données chargées depuis le fichier 'job_offers.csv'"),
                   DT::dataTableOutput("csv_table_jobs")
                 ) 
        ),
        tabPanel("Description des données",
                 tagList(
                   HTML("
      <style>
        .presentation-texte {
          text-align: justify;
          text-indent: 2em;
          line-height: 1.6;
        }
 
        .presentation-texte ul {
          list-style-type: disc;
          padding-left: 2em;
          margin-top: 0.5em;
        }
 
        .presentation-texte li {
          text-indent: 0;
          margin-bottom: 0.5em;
        }
      </style>
 
      <div class='presentation-texte'>
        <h3>Fichier <code>donnees_github.csv</code></h3>
<p>Ce fichier csv contient les colonnes suivantes: </p>
<ul>
  <li><strong>id</strong> : Identifiant unique du dépôt.</li>
  <li><strong>name</strong> : Nom du dépôt GitHub.</li>
  <li><strong>date</strong> : Date à laquelle le dépôt a été détecté (format ISO 8601).</li>
  <li><strong>stars</strong> : Nombre d’étoiles (popularité) du dépôt sur GitHub.</li>
  <li><strong>language</strong> : Langage de programmation principal du dépôt.</li>
  <li><strong>description</strong> : Brève description du projet associée au dépôt.</li>
</ul>
      </div>
    ")),
                 tagList(
                   HTML("
      <style>
        .presentation-texte {
          text-align: justify;
          text-indent: 2em;
          line-height: 1.6;
        }
 
        .presentation-texte ul {
          list-style-type: disc;
          padding-left: 2em;
          margin-top: 0.5em;
        }
 
        .presentation-texte li {
          text-indent: 0;
          margin-bottom: 0.5em;
        }
      </style>
 
      <div class='presentation-texte'>
        <h3 style='text-align: center;'>Données des offres d'emploi Welcome to the jungle</h3>
        <h3>Fichier <code>job_offers.csv</code></h3>
<p>Ce fichier csv contient les oclonnes suivantes : </p>
<ul>
  <li><strong>id</strong> : Identifiant unique de l’offre d’emploi.</li>
  <li><strong>title</strong> : Intitulé du poste.</li>
  <li><strong>company</strong> : Nom de l’entreprise proposant le poste.</li>
  <li><strong>location</strong> : Ville/localisation du poste.</li>
  <li><strong>job_type</strong> : Type de contrat (ex : CDI, Stage...).</li>
  <li><strong>url</strong> : Lien vers l’annonce en ligne.</li>
  <li><strong>salary</strong> : Fourchette salariale indiquée dans l’annonce.</li>
  <li><strong>source</strong> : Plateforme d’où provient l’offre (ici : 'Welcome to the Jungle'').</li>
  <li><strong>scraped_date</strong> : Date de récupération de l’annonce.</li>
  <li><strong>programming_languages</strong> : Langages de programmation mentionnés dans l’annonce (ex : Rust, Go, JavaScript...).</li>
</ul>
      </div>
    "))
                 
        ),
        
        tabPanel("Scraping des données",
                 tagList(
                   HTML("
      <style>
        .presentation-texte {
          text-align: justify;
          text-indent: 2em;
          line-height: 1.6;
        }
 
        .presentation-texte ul {
          list-style-type: disc;
          padding-left: 2em;
          margin-top: 0.5em;
        }
 
        .presentation-texte li {
          text-indent: 0;
          margin-bottom: 0.5em;
        }
      </style>
 
      <div class='presentation-texte'>
        <h2>Données GitHub</h2>
<p>
  L’unité expérimentale pour cette partie de l’étude est constituée des projets publics sur GitHub.
  Chaque projet représente une unité d’observation, et les variables collectées sont : le langage de programmation,
  la date de création, le nombre d’étoiles, le nom et la description.
</p>
<p>
  Pour effectuer cette intégration, nous avons utilisé l’API Repositories de GitHub, limitée à un maximum de 2000 requêtes.
  La variable <code>date</code> correspond à la période de création des projets. Dans cette étude, nous observons les projets créés entre 
  janvier et mars 2025.
</p>
 
      </div>
    "),
                   downloadButton("download_script", "Télécharger le script Python")
                 ),
                 
                 tagList(
                   HTML("
      <style>
        .presentation-texte {
          text-align: justify;
          text-indent: 2em;
          line-height: 1.6;
        }
 
 
        .presentation-texte ul {
          list-style-type: disc;
          padding-left: 2em;
          margin-top: 0.5em;
        }
 
        .presentation-texte li {
          text-indent: 0;
          margin-bottom: 0.5em;
        }
      </style>
 
      <div class='presentation-texte'>
        <h3 style='text-align: center;'>Script de scraping des offres d'emploi Welcome to the jungle</h3>
        </p></p>&emsp;
          L’unité expérimentale pour cette partie de l’étude sera constituée des offres d’emploi publiées sur
Welcome to the Jungle. Chaque offre représente une unité d’observation, et les données seront collectées
à partir des informations contenues dans chaque offre d’emploi (langages, type de contrat, location, etc.).
La collecte a été effectuée en ciblant les offres correspondant aux recherches Software Engineer et
Développeur. Le script parcourt ensuite les différentes fiches d’emploi pour extraire les informations
principales. Les noms de langages de programmation apparaissent souvent dans le titre du
poste, mais leur détection via expressions régulières s’est révélée peu fiable pour des cas ambigus comme
R ou C. Pour résoudre ce problème, un classifieur basé sur le modèle facebook/bart-large-mnli a
été utilisé. Ce modèle, accessible via Hugging Face, permet une classification plus robuste et contextuelle
des langages mentionnés. La collecte des données des offres d’emploi a été réalisée à l’aide d’un scraper automatisé développé avec Selenium, en utilisant les mots-clés software engineering et développeur. Le site ciblé : Welcome to the jungle, a affiché un total de 1788 résultats, mais en raison de limitations techniques, notamment l’impossibilité de charger plus de 8 pages de résultats, le nombre total d’offres extraites a été limité à 393. Parmi ces 393 annonces, seules 92 contenaient des informations exploitables concernant les langages de programmation recherchés. 
        </p></p>
      </div>
    "),
                   downloadButton("download_script2", "Télécharger le script Python")
                 )
        )
      )
    })
  })
  
  
  # Visualisation avec sous-onglets
  observeEvent(input$show_visu, {
    output$main_content <- renderUI({
      tabsetPanel(id = "visu_tabs",
                  tabPanel("Evolution",
                           card(
                             card_header("Graphique des tendances : Popularité des langues au fil du temps"),
                             plotOutput("line_trend")
                           ),
                           card(
                             HTML(
                               "<p style=\"text-align: justify;\">Dès janvier JavaScript et Python s'imposent comme leaders incontestés avec un nombre de projets bien supérieur aux autres tandis que TypeScript entame déjà une progression régulière. En février les premiers signes de changement apparaissent avec l'ascension naissante de Go et Rust qui gagnent respectivement 7% et 5% d'adoption alors que C++ commence un déclin progressif de 3% parallèlement la catégorie Unknown diminue légèrement probablement grâce à de meilleurs systèmes de détection des langages. Mars marque une accélération des tendances émergentes avec la plus forte croissance mensuelle de TypeScript à 12% tandis que Rust dépasse soudainement ses performances antérieures avec 10% d'augmentation Go maintient sa progression régulière à 7% alors que C++ poursuit son recul à 5%. En avril le paysage se stabilise partiellement JavaScript et Python conservent leur position dominante malgré une légère baisse de 2% TypeScript confirme son statut de challenger sérieux avec 9% de croissance supplémentaire Rust et Go continuent leur progression à 8% et 6% respectivement tandis que C++ atteint son plus bas niveau avec 4% de déclin supplémentaire. Jupyter Notebook reste stable tout au long de la période avec des variations inférieures à 2% reflétant sa niche spécialisée en data science la catégorie Unknown poursuit sa lente érosion avec 4% de diminution en avril.</p>
 
<p style=\"text-align: justify;\">L'analyse révèle la résilience des géants JavaScript et Python malgré une légère érosion en fin de période l'ascension régulière de TypeScript qui devient un véritable troisième challenger et l'émergence tardive mais spectaculaire des langages systèmes modernes comme Go et Rust.</p>
<p style=\"text-align: justify;\">L'évolution montre un écosystème en maturation où les nouveaux langages gagnent du terrain sans détrôner les leaders historiques créant ainsi un paysage technique plus diversifié et spécialisé où chaque langage trouve sa place selon des logiques d'usage et de performance spécifiques.</p>"
                             )
                           )
                  ),
                  tabPanel("Par Date",
                           card(
                             card_header("Date simple"),
                             dateInput("date", "Sélectionner une date", value = "2025-01-01")
                           ),
                           card(
                             card_header("Diagramme à barres : Projets par langage"),
                             plotOutput("bar_language")
                           ),
                           card(
                             HTML(
                               "<p style=\"text-align: justify;\"> Ce graphique illustre le nombre de projets publics créés sur GitHub par langage de programmation pour la journée du 21 mars 2025, ce qui offre une vision ponctuelle de l'activité sur la plateforme mais limite fortement la portée des conclusions que l'on peut en tirer dans le cadre d'une analyse globale. Par ailleurs, la présence notable de projets classés dans la catégorie \"Unknown\" suggère un manque de précision dans l'identification des langages, ce qui introduit une incertitude supplémentaire dans l'interprétation des résultats.</p>
 
<p style=\"text-align: justify;\">Cependant ce graphique permet d’avoir un premier aperçu des langages les plus utilisés par les développeurs sur GitHub, ce qui peut ensuite être mis en parallèle avec les langages les plus demandés dans les offres d’emploi. Il serait toutefois nécessaire d’étendre l’observation à une période plus large et de croiser ces résultats avec des données issues du marché de l’emploi sur une base temporelle équivalente.</p>"
                             )
                           )
                  )
      )
    })
  })
  
  
  # Bar graph: Projects by language on a specific date
  output$bar_language <- renderPlot({
    req(input$date)  # Ensure a date is selected
    
    # Filter the data for the selected date
    df <- projectsData()
    df_filtered <- df[df$date == input$date, ]
    
    # In case no projects are found for that date, show a message on a blank plot
    if(nrow(df_filtered) == 0){
      plot.new()
      title("Aucun projet trouvé pour la date sélectionnée")
      return()
    }
    
    # Create a table of counts per language
    tbl <- table(df_filtered$language)
    # Construct a data frame manually with the expected two columns
    lang_count <- data.frame(language = names(tbl), Count = as.vector(tbl))
    
    # Sort lang_count by Count in decreasing order
    lang_count <- lang_count[order(lang_count$Count, decreasing = TRUE), ]
    # Convert 'language' to a factor with levels ordered by decreasing count
    lang_count$language <- factor(lang_count$language, levels = lang_count$language)
    
    ggplot(lang_count, aes(x = language, y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Projets par langage", input$date),
           x = "Langages", y = "Nombre de projets") +
      theme_minimal()
  })
  
  output$line_trend <- renderPlot({
    # Retrieve the full dataset from CSV
    df <- projectsData()
    
    # Identify the top 8 languages by total project count
    top_langs <- df %>%
      group_by(language) %>%
      summarise(TotalCount = n(), .groups = "drop") %>%
      arrange(desc(TotalCount)) %>%
      head(8) %>%
      pull(language)
    
    # Filter the data to include only the top 8 languages
    df_filtered <- df %>% 
      filter(language %in% top_langs)
    
    # Group data by date and language to count projects per day per language
    trends <- df_filtered %>%
      group_by(date, language) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Plot using ggplot2: a line for each language over time
    ggplot(trends, aes(x = date, y = Count, color = language)) +
      geom_line(size = 1, alpha = 0) +
      # Add a smoothing layer: adjust 'span' as necessary to smooth over ~3 days
      geom_smooth(se = FALSE, method = "loess", span = 0.17) +
      labs(title = "Tendance des 8 langages les plus populaires au fil du temps",
           x = "Date", y = "Nombre de projets", color = "Langage") +
      theme_minimal()
  })
  
  # Analyse
  
  observeEvent(input$show_analyse, {
    output$main_content <- renderUI({
      tabsetPanel(
        id = "visu_tabs",
        tabPanel("Correlation Github - Offres d'Emploie",
                 tagList(
                   card(
                     card_header("Etude de correlation"),
                     plotOutput("correlation_plot")
                   ),
                   card(
                     HTML(
                       "<p style=\"text-align: justify;\">La visualisation des données montre une absence de corrélation évidente entre le nombre de projets GitHub par langage de programmation et le volume d'offres d'emploi correspondantes. Les points du nuage de points apparaissent dispersés sans réelle tendance, et le coefficient de corrélation calculé est proche de zéro, indiquant qu'il n'existe pas de relation linéaire significative entre ces deux variables.</p>
 
<p style=\"text-align: justify;\">Cependant, cette conclusion doit être nuancée. L'absence de corrélation pourrait s'expliquer par plusieurs facteurs : la taille limitée de l'échantillon, l'exclusion possible de projets privés ou d'offres d'emploi combinant plusieurs compétences, ou encore des spécificités sectorielles. De plus, certains langages peuvent être populaires dans des niches particulières sans pour autant générer un volume élevé d'offres d'emploi généralistes.</p>
 
<p style=\"text-align: justify;\">En l'état, les données ne permettent pas d'affirmer un lien clair entre popularité sur GitHub et demande sur le marché du travail. Une analyse plus approfondie, intégrant des métriques supplémentaires (communauté de développeurs, annonces détaillées, données temporelles) serait nécessaire pour mieux comprendre cette relation.</p>"
                     )
                   )
                 )
        ),
        tabPanel("Stars - Language de Programmation",
                 tagList(
                   card(
                     card_header("Stars by Programming Language"),
                     plotOutput("stars_language")
                   ),
                   card(
                     HTML(
                       "<p style=\"text-align: justify;\">Ce graphique révèle des écarts significatifs de popularité entre les principaux langages de programmation sur GitHub :</p>
 
<p style=\"text-align: justify;\">JavaScript domine le classement avec 70 000 étoiles, confirmant sa position de langage le plus populaire pour le développement web.
Python arrive en seconde position avec 60 000 étoiles, reflétant son adoption massive dans les domaines du machine learning, de l'analyse de données et du scripting.
Java se maintient à un niveau notable avec 25 000 étoiles, particulièrement pour les applications d'entreprise et Android.
Les autres langages comme Go (12 000), TypeScript (10 000) et Ruby (8 000) montrent une popularité modérée mais stable.
En queue de classement, on trouve des langages plus spécialisés comme Rust (5 000), Kotlin (4 000) et Swift (3 000), ainsi que des langages legacy ou de niche proches de zéro étoile (COBOL, Pascal).</p>
 
<p style=\"text-align: justify;\">Cette distribution met en lumière plusieurs tendances clés :
La suprématie des langages web (JavaScript) et polyvalents (Python)
La résilience des langages historiques comme Java
L'émergence progressive de nouveaux langages systèmes comme Go et Rust
Le fossé important entre les langages mainstream et les technologies spécialisées. L'absence de certains langages comme PHP ou C# dans le top 3 pourrait surprendre et mériterait une investigation plus poussée sur la méthodologie de collecte. Cette analyse souligne à quel fois la popularité sur GitHub ne reflète pas toujours directement l'usage en entreprise.</p>"
                     )
                   )
                 )
        ),
        tabPanel("Type de Contrat - Langage",
                 tagList(
                   card(
                     card_header("Job Offers by Language and Job Type"),
                     plotOutput("job_offers_language")
                   ),
                   card(
                     HTML(
                       "<p  style=\"text-align: justify;\">Ce diagramme révèle des tendances marquées dans le paysage des recrutements tech. Java s'impose comme le langage le plus demandé en CDI, suivi de Python et C++, reflétant la prédominance des langages établis dans les embauches durables. Le marché des alternances présente un profil différent, avec Java en tête mais une présence notable de C++, PHP et Groovy (à égalité), suggérant des besoins variés en formation professionnelle.</p>
 
<p style=\"text-align: justify;\">La répartition des stages surprend par la forte représentation des langages modernes Go et Rust (à égalité), devant Java et JavaScript, indiquant peut-être une volonté des entreprises de former les nouveaux talents aux technologies émergentes. Le freelance se concentre exclusivement sur JavaScript et Scala, mettant en lumière deux niches spécifiques : le développement web côté client pour le premier, et les solutions data/backend pour le second.</p>
 
<p style=\"text-align: justify;\">Cette analyse met en évidence la segmentation du marché selon les types de contrat : si les CDI privilégient les langages matures et polyvalents, les autres formes de recrutement révèlent des besoins plus spécialisés. Les égalités observées (Groovy/PHP, Go/Rust) et l'absence de certains langages dans certaines catégories pourraient s'expliquer par des effets de niche ou des spécificités sectorielles, invitant à approfondir l'étude avec des données plus granularisées.</p>"
                     )
                   )
                 )
        )
      )
    })
  })
  
  # Reactive: Count the frequency of each programming language in job offers
  jobOffersCount <- reactive({
    jobOffersData() %>%
      group_by(language) %>%
      summarise(JobCount = n(), .groups = "drop")
  })
  
  # Reactive: Count the frequency of each programming language in projects (from the GitHub dataset)
  projectsCount <- reactive({
    projectsData() %>%
      group_by(language) %>%
      summarise(ProjectCount = n(), .groups = "drop")
  })
  
  # Reactive: Merge both counts based on language and only keep common languages.
  correlationData <- reactive({
    proj <- projectsCount()
    jobs <- jobOffersCount()
    
    # Inner join to get languages that appear in both datasets
    dplyr::inner_join(proj, jobs, by = "language")
  })
  
  # Output: Plot the correlation between GitHub projects and job offers for each language
  output$correlation_plot <- renderPlot({
    dat <- correlationData()
    
    if(nrow(dat) == 0){
      plot.new()
      title("No correlation data available")
      return()
    }
    
    # Calculate correlation coefficient (Pearson by default)
    corr_value <- cor(dat$ProjectCount, dat$JobCount)
    corr_label <- paste("r =", round(corr_value, 2))
    
    # Create the base plot
    p <- ggplot(dat, aes(x = ProjectCount, y = JobCount, label = language)) +
      geom_point(size = 3, color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE, color = "darkred") +
      geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
      labs(title = "Correlation: Popularité de langages sur GitHub vs. Offres d'emploi",
           x = "Nombre de projets Github",
           y = "Nombre d'offres d'emploi") +
      theme_minimal()
    
    # Add the correlation coefficient as annotation. Position it in the upper right.
    # You may need to adjust the x and y coordinates depending on your data range.
    p + annotate("text",
                 x = max(dat$ProjectCount) * 0.75, 
                 y = max(dat$JobCount) * 0.9,
                 label = corr_label,
                 size = 5,
                 color = "black",
                 fontface = "bold")
  })
  
  
  output$stars_language <- renderPlot({
    df <- projectsData()  # Use your already imported projects data
    
    # Create a boxplot: stars distribution per language
    ggplot(df, aes(x = language, y = stars)) +
      geom_boxplot(fill = "steelblue", alpha = 0.7) +
      labs(title = "Distribution of Stars by Programming Language",
           x = "Programming Language", y = "Stars per Project") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$job_offers_language <- renderPlot({
    df <- jobOffersData()  # Use your already imported job offers data
    
    # Summarise the total count of job offers per language and per job type
    df_summary <- df %>%
      group_by(language, job_type) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Create a grouped bar plot for job offers
    ggplot(df_summary, aes(x = language, y = Count, fill = job_type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Job Offers Distribution by Language and Job Type",
           x = "Programming Language", y = "Number of Job Offers", fill = "Job Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Conclusion
  observeEvent(input$show_ccl, {
    output$main_content <- renderUI({
      HTML("
      <style>
        .presentation-texte {
          text-align: justify;
          text-indent: 2em;
          line-height: 1.6;
        }
 
 
        .presentation-texte ul {
          list-style-type: disc;
          padding-left: 2em;
          margin-top: 0.5em;
        }
 
        .presentation-texte li {
          text-indent: 0;
          margin-bottom: 0.5em;
        }
      </style>
 
      <div class='presentation-texte'>
        <h3 style='text-align: center;'>Conclusion</h3>
        <p>
        <p>
        Cette étude visait à évaluer si les projets publics créés sur GitHub en 2025 reflètent les tendances du marché de l’emploi tech en France, 
      tel qu’observé via Welcome to the Jungle et Google Trends. L’analyse révèle que certaines technologies comme <strong>Python</strong> sont à la fois populaires sur GitHub, 
      recherchées en ligne et très demandées par les recruteurs. D’autres, comme <strong>JavaScript</strong> ou <strong>TypeScript</strong>, sont actives en open source 
      mais moins visibles dans les offres d’emploi, tandis que <strong>Java</strong> et <strong>C++</strong> sont fortement demandés sans dominer GitHub.
      </p>
        <p>
        La montée de <strong>Go</strong> et <strong>Rust</strong> sur GitHub, notamment dans les stages/alternances, suggère une anticipation des besoins futurs 
      par certaines entreprises. Par ailleurs, les offres restent très concentrées sur <strong>Paris</strong>, soulevant des questions d’accessibilité géographique.
      </p>
        <p>
        En somme, cette étude met en lumière à la fois des convergences et des écarts entre innovation communautaire, intérêt public et demande professionnelle. 
      Une veille technologique plus contextualisée reste essentielle pour mieux aligner compétences et opportunités.
      </p>
    ")
      
      
    })
  })
  
  # 1. Fonctions réactives pour lire les CSV
  csv_data_github <- reactive({
    filepath <- "donnees_github.csv"
    if (file.exists(filepath)) {
      tryCatch({
        read.csv(filepath, encoding = "UTF-8")
      }, error = function(e) {
        showNotification("Erreur lors du chargement de donnees_github.csv", type = "error")
        NULL
      })
    } else {
      showNotification("Fichier 'donnees_github.csv' introuvable", type = "error")
      NULL
    }
  })
  
  csv_data_jobs <- reactive({
    filepath <- "job_offers.csv"
    if (file.exists(filepath)) {
      tryCatch({
        read.csv(filepath, encoding = "UTF-8")
      }, error = function(e) {
        showNotification("Erreur lors du chargement de job_offers.csv", type = "error")
        NULL
      })
    } else {
      showNotification("Fichier 'job_offers.csv' introuvable", type = "error")
      NULL
    }
  })
  
  # 2. Sorties pour afficher les tableaux
  output$csv_table_github <- DT::renderDataTable({
    req(csv_data_github())
    DT::datatable(csv_data_github(), options = list(pageLength = 10))
  })
  
  output$csv_table_jobs <- DT::renderDataTable({
    req(csv_data_jobs())
    DT::datatable(csv_data_jobs(), options = list(pageLength = 10))
  })
  
  
  output$download_script <- downloadHandler(
    filename = function() {
      "github.py"
    },
    content = function(file) {
      file.copy("github.py", file)
    }
  )
  
  output$download_script2 <- downloadHandler(
    filename = function() {
      "welcometothejungle_selenium.py"
    },
    content = function(file) {
      file.copy("welcometothejungle_selenium.py", file)
    }
  )
  
  
  # Histogramme
  data <- reactive({
    set.seed(123)
    rnorm(5000)
  })
  
  
  output$plot <- renderPlot({
    x <- data()
    hist(x, main = "Histogramme de données aléatoires", col = "steelblue", border = "white")
  })
  
  observeEvent(input$go, {
    showNotification("Histogramme mis à jour")
  })
  observeEvent(input$code_btn, {
    showModal(modalDialog(
      title = "Code source (extrait)",
      easyClose = TRUE,
      size = "l",
      footer = NULL,
      tags$pre(style = "max-height: 500px; overflow-y: auto; background-color: #f8f9fa; padding: 10px; border-radius: 8px;",
               tags$code(Salut
               )
      )
    ))
  })
}

shinyApp(ui = ui, server = server)
