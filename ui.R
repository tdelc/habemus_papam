library(shiny)
library(DT)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)

options(DT.options = list(
	language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
))

# Titre de Mazette
header <- function(){ 
  dashboardHeader(title = "") }

sidebar <- function(){
  dashboardSidebar(
    collapsed=TRUE,
    sidebarMenu(
      id = "sidebar",
      menuItem("Login", tabName = "connexion", icon = icon("arrow-right-to-bracket")),
      menuItem("User", tabName = "enquete", icon = icon("magnifying-glass")),
      menuItem("Admin", tabName = "admin", icon = icon("lock"))
    )
  )
}

body <- function(){
  dashboardBody(
    tabItems(
      tabItem_connexion,
      tabItem_enquete,
      tabItem_admin
    )
  )
}

tabItem_connexion <- tabItem(tabName = "connexion",
                             conditionalPanel(
                               condition = "output.logged == 'not_ok'",
                               fluidRow(
                                 box(width = 6, title = "Connexion",
                                     status = "primary", solidHeader = TRUE,
                                     div(h5(textOutput("info_log")), style = "color:red"),
                                     br(),
                                     textInput("username", "Identifiant", ""),
                                     passwordInput("password", "Mot de passe", ""),
                                     actionButton("boutton_log", "Go!")
                                 )
                               )
                           ),
                           conditionalPanel(
                             condition = "output.logged == 'not_connected'",
                             fluidRow(
                               box(width = 6, title = "Interface informatique pour Habemus Papam",
                                   status = "primary", solidHeader = TRUE,
                                   p("Bienvenue dans l'interface informatique permettant de gérer les indices durant la murder party Habemu Papam"),
                                   p("Comme vous avez pu le lire dans les règles de jeu, les personnages ont la possibilité d’obtenir des indices via des enquêtes. Vous pouvez gérer ces actions vous mêmes (auquel cas vous devez imprimer les indices en plusieurs exemplaires ainsi que des points d’indices, et réaliser les actions) mais cela prend beaucoup de temps (nous vous conseillons dans ce cas d’être deux)."),
                                   p("Vous pouvez également vous munir d’ordinateurs gérant cette tâche à votre place. Deux ou trois ordinateurs sont nécessaires dans ce cas, répartis sur l’ensemble de l’espace de jeu. Vous n’avez rien à installer pour les ordinateurs devant gérer les indices, toutes les actions sont réalisées avec cette interface web."),
                                   p("En vous rendant sur l'url https://tdelc.shinyapps.io/habemus_new/?new_server , Vous allez pouvoir créer le serveur qui servira à stocker les actions des joueurs durant le jeu. Choisissez un nom sans espace ni caractères spéciaux. Vous cliquez ensuite sur “Créer Serveur” et l’interface vous indiquera si le serveur a été créé, ou si vous avez choisi un nom qui existe déjà."),
                                   p("Une fois le serveur créé, vous pouvez lancer cette url sur tous les ordinateurs de la session : https://tdelc.shinyapps.io/habemus_new/?server=XXX en remplaçant le terme XXX par le nom que vous aurez choisi pour votre serveur."),
                                   p("Cette url donne accès aux joueurs à une interface complète permettant d’envoyer des enquêteurs sur les indices disponibles dans leur feuille de personnage (et d’ajouter des points de pouvoir pour améliorer leur chance de réussite parfaite), de copier ou d’intercepter des enquêtes réalisées par les autres ou de s’envoyer des messages entre eux ou elles. Toutes ces actions sont expliquées directement dans l’interface. Pensez quand même à faire un tutoriel aux joueur.euse.s avant le début du jeu et/ou à les accompagner lors de leur première action.")
                                   
                               )
                             )
                           )
                           )

tabItem_enquete <- tabItem(tabName = "enquete",
                           conditionalPanel(
                             condition = "output.logged == 'ok'",
                           fluidRow(
                             div(h5(textOutput("info")), style = "color:red"),
                             
                             box(width = 3, title = "Informations",
                                 status = "info", solidHeader = TRUE,
                             h3(textOutput("currentTime")),
                             h3(textOutput("user_name")),
                             h4(textOutput("user_PA")),
                             helpText("(Vous regagnerez quatre points de pouvoir par heure)"),
                             h4(HTML("<A HREF=\"javascript:history.go(0)\">Quitter l'interface</A>"))
                             ),
                             box(width = 3, title = "Lancer une enquête",
                               status = "primary", solidHeader = TRUE,
                                tabsetPanel(
                                         tabPanel("Recherche classique",
                                                  selectInput("choix_enquete", "Choisir l'enquête:",c("dynamique"),selectize=FALSE,size=10,width='100%'),
                                                  actionButton("boutton_enquete", "Lancer la recherche"),
                                                  actionButton("boutton_enquete_help",label="",icon = icon("circle-question"),style="color: #000; background-color: #fff; border-color: #fff")
                                                  ),
                                         tabPanel("Recherche avancée",
                                                  helpText("(n'oubliez pas les accents)"),
                                                  textInput("advanced_1", "Premier mot-clef"),
                                                  textInput("advanced_2", "Deuxième mot-clef"),
                                                  textInput("advanced_3", "Troisième mot-clef"),
                                                  actionButton("boutton_enquete_advanced", "Lancer la recherche"),
                                                  actionButton("boutton_enquete_advanced_help",label="",icon = icon("circle-question"),style="color: #000; background-color: #fff; border-color: #fff")
                                         )
                                       ),
                               br(),
                               column(width=5,
                                      numericInput("PA", "Points de pouvoir :", 1, min = 1, max = 10,step=1)),
                               column(width=7,
                                      h5("Probabilité de réussite"),
                                      tags$head(tags$style(type="text/css", "#table_proba table td {line-height:50%;}")),
                                      tableOutput("table_proba"))
                               # plotOutput("plot_proba",width="100%")
                           ),
                           box(width = 3, title = "Copier ou Intercepter une enquête",
                               status = "primary", solidHeader = TRUE,
                                    selectInput("choix_user_copie", "Choisir la cible",
                                                c("dynamique"),selectize=FALSE,size=10),
                                    actionButton("boutton_enquete_copie", "Récupérer une copie de l'enquête"),
                               actionButton("boutton_enquete_copie_help",label="",icon = icon("circle-question"),style="color: #000; background-color: #fff; border-color: #fff"),br(),
                               actionButton("boutton_enquete_interception", "Intercepter l'enquête"),
                               actionButton("boutton_enquete_interception_help",label="",icon = icon("circle-question"),style="color: #000; background-color: #fff; border-color: #fff"),
                               helpText("(coûte 2 points de pouvoir)")
                           ),
                           box(width = 3, title = "Envoyer un message privé",
                               status = "success", solidHeader = TRUE,
                               
                               selectInput("choix_user_chat", "Choisir la cible",
                                           c("dynamique"),selectize=FALSE,size=12),
                               textAreaInput("message_chat","Message :",width='100%'),
                               numericInput("timer_chat", "Délai d'attente (en minute) pour envoyer le message", 1, min = 0, max = 15),
                               column(7,checkboxInput("ano_chat", "Message anonyme ? (1 PA)")),
                               column(5,actionButton("boutton_chat", "Envoyer le message"))
                           )),
                           fluidRow(
                             box(width = 9, title = "Liste des indices obtenus",
                                 status = "primary", solidHeader = TRUE,
                                 DT::dataTableOutput("tableau_indices")),
                             box(width = 3, title = "Liste des messages obtenus",
                                 status = "success", solidHeader = TRUE,
                                 DT::dataTableOutput("tableau_chat"))
                             
                           ),
                           fluidRow(
                             conditionalPanel(
                               condition = "output.pv_copie_classement == 'oui'",
                               box(width = 6, title = "Obtenir une copie du classement",
                                   status = "warning", solidHeader = TRUE,
                                   p("Vous pouvez obtenir une copie du classement actuel des entités. Attention, il s'agit d'une copie à cet instant et le classement peut évoluer ensuite."),
                                   p("La copie sera disponible dans les 10 minutes et vous coûtera 2 points de pouvoir."),
                                   p("Attention, nous vous prévenons que les services de Andromalius pourraient détecter cette intrusion dans leur système."),
                                   br(),
                                   actionButton("boutton_copie_classement", "Obtenir le classement"),
                                   h4("(coûte 2 points de pouvoir)")
                                ),
                             conditionalPanel(
                               condition = "output.pv_modif_classement == 'oui'",
                               box(width = 6, title = "Modifier le classement",
                                   status = "warning", solidHeader = TRUE,
                               p("Vous pouvez modifier le classement actuel des entités pour choisir la personne à mettre en tête. Attention, il s'agit d'une modification à cet instant et le classement peut évoluer ensuite."),
                               p("La modification sera immédiate et vous coûtera 3 points de pouvoir."),
                               p("Attention, nous vous prévenons que les services de Andromalius détecteront à coup sûr cette intrusion dans leur système. De plus, il y a encore une chance sur 6 qu'ils puissent contrer cette intrusion avant qu'elle puisse être réalisée."),
                               br(),
                               selectInput("choix_user_classement", "Choisir la cible",
                                           c("dynamique"),selectize=FALSE,size=10),
                               actionButton("boutton_modif_classement", "Modifier le classement"),
                               h4("(coûte 3 points de pouvoir)")
                             )
                           )
                             )
                           )
                           )
)

tabItem_admin <- tabItem(tabName = "admin",
                         conditionalPanel(
                           condition = "output.logged == 'server'",
                           textInput("server_name", "Nom du serveur", ""),
                           actionButton("create_server", "Créer le serveur"),
                           br(),
                           textOutput("info_server")
                         ),
                         conditionalPanel(
                           condition = "output.logged == 'admin'",
                           div(h5(textOutput("info_admin")), style = "color:red"),
                           h4(HTML("<A HREF=\"javascript:history.go(0)\">Quitter l'interface</A>")),
                         tabsetPanel(
                           tabPanel("Accueil",
                                    fluidRow(
                                      box(width = 4, title = "Messages privés avec les PJ",
                                          status = "primary", solidHeader = TRUE,
                                          
                                          selectInput("choix_user_chat_admin", "Choisir la cible",c("dynamique"),selectize=FALSE,size=12),
                                          textAreaInput("message_chat_admin","Message :",width='100%'),
                                          actionButton("boutton_chat_admin", "Envoyer le message"),
                                          DT::dataTableOutput("tableau_chat_admin")
                                          
                                      ),
                                      box(width = 4, title = "Votre rôle de MJ",
                                          status = "primary", solidHeader = TRUE,
                                      p("Bienvenue dans l'interface de gestion de la murder. Vous avez ici de quoi influencer toute la murder concernant les indices, les classements, les points d'indices. Normalement, tout peut être consulté et modifié à l'aide de cette interface. Vous devez lire le document d'organisatin pour comprendre tout ce que vous devrez faire."),
                                      p("Globalement, durant la murder, vous devez ajouter des points d'action toutes les heures afin de permettre à vos PJ d'enquêter. Vous pouvez tout à fait accélérer ou ralentir ce rythme selon votre envie, pensez juste à ne pas trop laisser attendre vos PJ sans rien avoir à faire."),
                                      p("Vous avez accès à plusieurs onglets, chacun permettant un impact sur la murder :"),
                                      tags$ol(
                                        tags$li("Infos PJ : Cet onglet permet de vérifier et/ou modifier les mots de passe des PJ. C'est également ici que vous devrez ajouter les points de pouvoirs. Il est possible d'ajouter des points de pouvoir à l'ensemble des PJ ou à un.e joueur.euse en particulier. Il est enfin possible d'observer et modifier le classement des entités ;"), 
                                        tags$li("Vérifier et ajouter indices : Cet onglet permet de lire durant la murder les différents indices, de savoir si les PJ les ont obtenu, et éventuellement d'en ajouter un à l'un·e de vos PJ si cela est pertinent ;"), 
                                        tags$li("Actions spéciales : Cet onglet permet de modifier les probabilités de réussites des enquêtes. Cet onglet permet également d'activer les enquêtes spéciales (les enquêtes les plus complexes, dépendantes d’une autre enquête, et qui vont sans aucun doute provoquer de grandes révélations en fin de partie) et la transformation des enquêtes par chatGPT (avant de personnaliser un peu les réponses de chaque équipe d'enquête)"),
                                        tags$li("Modifier Indices : Cet onglet permet de modifier tous les indices de la murder. Soit en le modifiant manuellement dans le tableau, soit en téléchargeant le fichier complet, en le modifiant, puis en l'uploadant. Attention, ces modifications doivent être réalisées avant la murder (du moins avant qu'une enquête soit réalisée) ;"),
                                        tags$li("Vérifier actions : L'onglet le plus 'expert', il permet d'observer toutes les actions réalisées par les PJ, en ordre décroissant avoir d'avoir toujours une vue sur les derniers ;")
                                      )
                                    ),
                                    box(width = 4, title = "Déroulement de votre murder",
                                        status = "primary", solidHeader = TRUE,
                                        h4("Durée de la murder"),
                                        h5(textOutput("text_timer_admin")),
                                        actionButton("bouton_timer_admin","Lancer le timer de la murder"),
                                        br(),br(),
                                        h4("Durée depuis les derniers points d'indices"),
                                        h5(textOutput("text_timer_PA_admin")),
                                        actionButton("bouton_PA_all","Envoyer tout de suite 4 PA à tous les PJ")
                                    )
                                    )
                           ),
                           tabPanel("Infos PJ",
                                    fluidRow(
                                      box(width = 8, title = "Information sur les personnages, les statistiques d'enquête et le classement des entités",
                                          status = "primary", solidHeader = TRUE,
                                          DT::dataTableOutput("table_admin_PJ")
                                      ),
                                      column(4,
                                             box(width = 12, title = "Modification du mot de passe",
                                                 status = "primary", solidHeader = TRUE,
                                                 column(6,
                                                        selectInput("choix_user_password", "Choisir le personnage",c("dynamique"))),
                                                 column(6,
                                                        textInput("password_admin", "Nouveau mot de passe")),
                                                 actionButton("boutton_admin_password", "Changer mot de passe")
                                             ),
                                             box(width = 12, title = "Ajouter des points d'action",
                                                 status = "primary", solidHeader = TRUE,
                                                 column(6,
                                                        selectInput("choix_user_PA", "Choisir le personnage",
                                                                    c("dynamique")
                                                        )),
                                                 column(6,
                                                        numericInput("PA_admin", "Nombre de PA à ajouter", 1, min = -5, max = 5)),
                                                 actionButton("boutton_admin_PA", "Ajouter/Retirer les PA")),
                                             conditionalPanel(
                                               condition = "output.pv_classement == 'oui'",
                                               box(width = 12, title = "Modifier le classement des entités",
                                                   status = "primary", solidHeader = TRUE,
                                                   column(6,selectInput("user_classement_admin", "Choisir le personnage",c("dynamique"))),
                                                   column(6,numericInput("points_classement_admin", "Nombre de points à ajouter", 1, min = -10000, max = 10000)),
                                                   actionButton("boutton_admin_classement", "Ajouter/Retirer les points")
                                               )
                                             )
                                      )
                                    )
                           ),
                           tabPanel("Vérifier et Ajouter Indices",
                                    fluidRow(
                                      box(width = 9, title = "Infos sur les enquêtes",
                                          status = "primary", solidHeader = TRUE,
                                          column(3,
                                          selectInput("choix_enquete_admin", "Choisir l'enquête:",
                                                      c("dynamique"),selectize=FALSE,size=28,width='100%')),
                                          column(9,
                                                 div(h5(textOutput("admin_info_lettre")), style = "color:red"),
                                                 h4("Enquête râtée"),
                                                 textOutput("admin_indice_1"),
                                                 h4("Enquête réussie"),
                                                 textOutput("admin_indice_2"),
                                                 h4("Enquête parfaite"),
                                                 textOutput("admin_indice_3")
                                          )
                                      ),
                                      column(3,
                                          box(width = 12, title = "Qui a eu cette enquête ?",
                                          status = "primary", solidHeader = TRUE,
                                          tableOutput("table_indices_admin")
                                          ),
                                          box(width = 12, title = "Ajouter cette enquête",
                                          status = "primary", solidHeader = TRUE,
                                          selectInput("choix_user_indice", "Choisir le personnage",
                                                      c("dynamique")),
                                          selectInput("choix_variation", "Choisir le résultat:",
                                                         c("dynamique")),
                                          actionButton("boutton_admin_indice", "Ajouter l'indice")
                                          )
                                      )
                                      )
                           ),
                           tabPanel("Actions spéciales",
                                    fluidRow(
                                      box(width = 6, title = "Modifier le dé pour les enquêtes",
                                          status = "primary", solidHeader = TRUE,
                                          column(3,
                                          numericInput("Nb_echec", "Nombre d'échec:", 1, min = 0, max = 10)),
                                          column(3,
                                          numericInput("Nb_reussite", "Nombre de réussite:", 4, min = 0, max = 10)),
                                          column(3,
                                          numericInput("Nb_reussite_parfaite", "Nombre de réussite parfaite:", 1, min = 0, max = 10)),
                                          column(3,
                                          actionButton("boutton_admin_des", "Changer les dés")),
                                          plotOutput("plot_proba_admin",width="100%")
                                      ),
                                      box(width = 6, title = "Autres actions",
                                          status = "primary", solidHeader = TRUE,
                                      h3("Enquêtes spéciales"),
                                      p("Les enquêtes spéciales sont à celles que les joueur·euse·s peuvent avoir dans leur liste s'iels obtiennent un résultat parfait sur une autre enquête. Cependant ces enquêtes contiennent beaucoup de révélations, et il est préférable qu'elles ne soient disponibles qu'en fin de soirée. Vous devez donc cliquer pour activer les enquêtes spéciales au moment où vous pensez que cela est nécessaire. A partir de là, les résultats de ces enquêtes sont accessibles."),
                                      actionButton("enquete_speciale_on", "Activer les enquêtes spéciales ?"),
                                      actionButton("enquete_speciale_off", "Desactiver les enquêtes spéciales ?"),
                                      br(),
                                      h3("Modification des indices par ChatGPT"),
                                      p("Afin que toustes n'aient pas les mêmes indices, vous pouvez demander une reformulation des indices avec l'aide automatique de ChatGPT. Une fois activée, tous les indices sont envoyés à ChatGPT avec comme instruction de modifier légèrement l'écriture tout en conservant les informations principales. Cela semble efficace, mais une modification sensible d'un indice reste possible. A vos risques et périls."),
                                      
                                      actionButton("enquete_chat_gpt_on", "Activer les enquêtes par Chat GPT ?"),
                                      actionButton("enquete_chat_gpt_off", "Desactiver les enquêtes par Chat GPT ?"),
                                      textOutput("admin_enquete_speciale"),
                                      br(),
                                      tableOutput("admin_table_autre_actions")
                                      )
                                    )
                                    # br(),
                                    # actionButton("boutton_admin_reset", "Remettre a réro la BDD"),
                                    # actionButton("boutton_admin_reset_chat", "Remettre a réro le chat"),
                                    # actionButton("boutton_admin_reset_normale", "Remettre a réro les enquêtes normales"),
                                    # actionButton("boutton_admin_reset_speciale", "Remettre a réro les actions spéciales")
                           ),
                           tabPanel("Modifier Indices",
                                    h2("Changer tous les indices via Excel"),
                                    p("Vous pouvez télécharger la liste des indices, la modifier, puis l'uploader pour remplacer la liste actuelle. Attention à garder les mêmes lettres d'indices, 3 résultats par lettre (enquête ratée, réussie ou parfaite)."),
                                    downloadButton('downloadIndices', 'Télécharger les indices actuels'),
                                    fileInput('loadIndices', 'Charger de nouveaux indices',accept='.csv'),
                                    br(),
                                    helpText("(Vous pouvez changer les textes en direct)"),
                                    DT::dataTableOutput("table_admin_indices")
                           ),
                           tabPanel("Vérifier actions",
                                    fluidRow(
                                      box(width = 12, title = "Liste des toutes les actions",
                                          status = "primary", solidHeader = TRUE,
                                          DT::dataTableOutput("table_admin")
                                      ))
                           )
                         )
                         )
)


controlbar <- function(){
  dashboardControlbar(
    id = "controlbar",
    skin = "dark",width = 250,
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        "Enquête",
        p("Vous pouvez nous envoyer enquêter pour obtenir plus d'informations sur un sujet précis. Cela vous coûtera de base 1 point de pouvoir. Pour chaque point supplémentaire, vous augmenterez nos chances de faire une enquête parfaite."),
        p("Vous pouvez enquêter de deux manières :"),
        p("1- Recherche Classique : Vous utilisez les ragots que vous avez."),
        p("2- Recherche Avancée : Vous nous donnez un ou plusieurs mots-clefs concernant des sujets que vous avez entendu durant la soirée. Attention, si vous nous envoyez vers une piste illogique, le résultat le sera également."),
        helpText("(L'enquête prendra entre 2 et 10 minutes)")
      ),
      controlbarItem(
        "Copie",
        p("Selon vos ordres, nous pouvons aller récupérer une copie du rapport d'enquête obtenu par un Archange ou Prince Démon."),
        p("Vous devez nous demander cela dans les 3 minutes après que votre cible ait obtenu son rapport d'enquête. Nous serons discrets, il n'en saura rien."),
        p("Cela vous coûtera 2 points de pouvoir.")
      ),
      controlbarItem(
        "Interception",
        p("Si vous remarquez, via une conversation ou une indiscrétion, qu'un Archange ou Prince Démon se prépare à enquêter sur un sujet qui ne vous arrange pas (bref, qu'on cherche à vous nuire...), vous pouvez nous demander d'intercepter le rapport."),
        p("Vous devez nous envoyer en interception avant qu'il ne lance son enquête. S'il fait ensuite une enquête dans les 15 minutes, nous serons en mesure de l'intercepter et de vous la transmettre."),
        p("Cela vous coûtera 2 points de pouvoir. Mais nous préférons vous prévenir, nous avons environ 1 chance sur 6 de nous faire capter par nos concurrents et de ne pas pouvoir intercepter convenablement l'indice.")
      )
    )
  )
}



ui <- dashboardPage(
  header = header(),
  sidebar = sidebar(),
  controlbar = controlbar(),
  body = body()
)
