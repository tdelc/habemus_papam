# Nouvelle ligne
prepare_row_drive <- function(id_session,user,action,cible,PA,
                              timer,resultat,timer_ok,texte=""){
  tibble(id_session=id_session,user=user,action=action,cible=cible,PA=PA,
         timer=timer,resultat=resultat,timer_ok=timer_ok,texte=texte)
}

# Création d'un server Dieu est mort
creation_server_dieu <- function(id_drive,server_name,donnees_joueurs,info_indices,indices_dispo){
  
  # Création du dé et actions spéciales
  actions <- prepare_row_drive(server_name,"admin","alea","1",1,NA,"",NA)
  actions <- actions %>% 
    add_row(prepare_row_drive(server_name,"admin","alea","2",4,NA,"",NA)) %>% 
    add_row(prepare_row_drive(server_name,"admin","alea","3",1,NA,"",NA)) %>% 
    add_row(prepare_row_drive(server_name,"admin","enq_spe","",0,NA,"Non",NA)) %>% 
    add_row(prepare_row_drive(server_name,"admin","chat_gpt","",0,NA,"Non",NA))
  
  # Ajout des identifiants/mots de passe et des PA
  for (i in 1:nrow(donnees_joueurs)){
    user <- donnees_joueurs$user[i]
    password <- donnees_joueurs$password[i]
    PA <- donnees_joueurs$PA[i]
    
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,"admin","password",user,
                                PA,NA,password,NA))
  }

  # Ajout Pouvoir
  actions <- actions %>% 
    add_row(prepare_row_drive(server_name,"admin","pouvoir",
                              "copie_classement",0,NA,"non",NA)) %>% 
    add_row(prepare_row_drive(server_name,"admin","pouvoir",
                              "modif_classement",0,NA,"non",NA))
  
  # Mélange des indices de départ pour être sûr de pas faire apparaître les indices importants en premier
  indices_dispo <- indices_dispo[sample(nrow(indices_dispo)),]
  
  for (i in 1:nrow(indices_dispo)){
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,indices_dispo$user[i],"init",
                                indices_dispo$indice[i],0,NA,indices_dispo$titre[i],NA))
  }
  
  # Ajout du texte des indices
  for (i in 1:nrow(info_indices)){
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,"admin","init",
                                info_indices$indice[i],
                                info_indices$variation[i],NA,
                                info_indices$titre[i],NA,
                                info_indices$texte[i]))
  }
  
  sheet_append(id_drive, data = actions)
  
}

# Création d'un server Habemus Papam
creation_server_habemus <- function(id_drive,server_name,donnees_joueurs,
                                    info_indices,indices_dispo){
  
  # Création du dé et actions spéciales
  actions <- prepare_row_drive(server_name,"admin","alea","1",1,NA,"",NA)
  actions <- actions %>% 
    add_row(prepare_row_drive(server_name,"admin","alea","2",4,NA,"",NA)) %>% 
    add_row(prepare_row_drive(server_name,"admin","alea","3",1,NA,"",NA)) %>% 
    add_row(prepare_row_drive(server_name,"admin","enq_spe","",0,NA,"Non",NA)) %>% 
    add_row(prepare_row_drive(server_name,"admin","chat_gpt","",0,NA,"Non",NA))
  
  # Ajout des identifiants/mots de passe, du classement, et des PA
  for (i in 1:nrow(donnees_joueurs)){
    user <- donnees_joueurs$user[i]
    password <- donnees_joueurs$password[i]
    PA <- donnees_joueurs$PA[i]
    
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,"admin","password",user,
                                PA,NA,password,NA))
    
    if (user %in% c("admin","TEST","HACK")) next
    
    resultat <- as.character(donnees_joueurs$points_classement[i])
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,"admin","classement",user,
                                0,NA,resultat,NA))
  }
  
  # Ajout faux perso
  for (user in c("Blandine","Jean","Andrealphus","Valefor")){
    resultat <- as.character(round(100000+runif(1,-5000,20000),0))
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,"admin","classement",
                                user,0,NA,resultat,NA))
  }
  
  # Ajout Pouvoir
  actions <- actions %>% 
    add_row(prepare_row_drive(server_name,"admin","pouvoir",
                              "copie_classement",0,NA,"oui",NA)) %>% 
    add_row(prepare_row_drive(server_name,"admin","pouvoir",
                              "modif_classement",0,NA,"oui",NA)) %>%
    add_row(prepare_row_drive(server_name,"Yves","pouvoir",
                              "copie_classement",0,NA,"oui",NA)) %>% 
    add_row(prepare_row_drive(server_name,"Marc","pouvoir",
                              "modif_classement",0,NA,"oui",NA)) %>% 
    add_row(prepare_row_drive(server_name,"Andromalius","pouvoir",
                              "copie_classement",0,NA,"oui",NA)) %>% 
    add_row(prepare_row_drive(server_name,"Andromalius","pouvoir",
                              "modif_classement",0,NA,"oui",NA))
  
  # Mélange des indices de départ pour être sûr de pas faire apparaître les indices importants en premier
  indices_dispo <- indices_dispo[sample(nrow(indices_dispo)),]
  
  for (i in 1:nrow(indices_dispo)){
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,indices_dispo$user[i],"init",
                                indices_dispo$indice[i],0,NA,
                                indices_dispo$titre[i],NA))
  }
  
  # Faire apparaitre les indices bonus
  bonus <- info_indices %>% filter(bonus != "")
  for (i in 1:nrow(bonus)){
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,"admin","bonus",bonus$indice[i]
                                ,3,NA,bonus$bonus[i],NA))
  }
  
  # Ajout du texte des indices
  for (i in 1:nrow(info_indices)){
    actions <- actions %>% 
      add_row(prepare_row_drive(server_name,"admin","init",
                                info_indices$indice[i],
                                info_indices$variation[i],NA,
                                info_indices$titre[i],NA,
                                info_indices$texte[i]))
  }
  
  sheet_append(id_drive, data = actions)
  
}


# Charger les données actions
load_actions <- function(id_drive,id_session_local){
  googlesheets4::read_sheet(id_drive) %>% 
    filter(str_to_lower(id_session) == str_to_lower(id_session_local)) %>% 
    mutate_if(is.list,as.character) %>% 
    mutate(timer = as.numeric(timer),
           timer_ok = as.numeric(timer_ok))
}

# Charger les indices
info_indices <- function(actions){
  actions %>% 
    filter(user == "admin" & action ==  "init") %>% 
    group_by(cible,PA,resultat) %>% 
    filter(row_number() == n()) %>% 
    ungroup() %>% 
    select(titre = resultat,texte,variation=PA,indice=cible) %>% 
    arrange(indice,variation)
}

# Charger les user/password
info_user <- function(actions){
  actions %>% 
    filter(user == "admin" & action ==  "password") %>% 
    group_by(cible) %>% 
    filter(row_number() == n()) %>% 
    ungroup() %>% 
    select(user=cible,password=resultat,PA)
}


# Fonction de cr?ation BDD d?s
creation_bdd_des <- function(values,bonus_recherche=0){
  
  # Extraire la liste des r?sultats
  rate <- values$actions[values$actions$cible == 1,]$PA
  ok <- values$actions[values$actions$cible == 2,]$PA
  parfait <- values$actions[values$actions$cible == 3,]$PA
  
  rate <- rate[!is.na(rate)]
  ok <- max(ok[!is.na(ok)]-bonus_recherche,0)
  parfait <- parfait[!is.na(parfait)]+bonus_recherche

  # Prendre le dernier r?sultat
  valeur_alea <- tibble(resultat=numeric(),valeur=numeric()) %>% 
    add_row(resultat=1,valeur=rate[length(rate)]) %>% 
    add_row(resultat=2,valeur=ok[length(ok)]) %>% 
    add_row(resultat=3,valeur=parfait[length(parfait)]) %>% 
    mutate(pc=valeur/sum(valeur),
           label = case_when(resultat == 1 ~ "Ratée",
                             resultat == 2 ~ "Réussie",
                             resultat == 3 ~ "Parfaite",
                             TRUE ~ "Erreur"))

  return(valeur_alea)
}

# Fonction de graphique pour le dé
plot_bdd_des <- function(valeur_alea){
  ggplot(valeur_alea) +
    aes(x = pc, y = reorder(label, pc), fill = label) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(label, " : ",round(pc*100,1), "%")),
              position = position_stack(vjust = 0.5)) + 
    scale_fill_manual(values = c("Parfaite" = "palegreen", "Ratée" = "orangered", "Réussie" = "skyblue")) +
    labs(x = "", y = "") +
    theme_void()+
    theme(legend.position = "none")+
    labs(title = "Probabilité de succès \nde l'enquête")
}


# Fonction de recherche indices
recherche_indice <- function(id_drive,values,user_name,
                             enquete,PA,resultat=NA,duree=NA){
  n_new <- 0
  
  if (is.na(duree)){ 
    # Dur?e al?atoire de recherche
    alea_duree <- sample(c(2:10),1)
    if (nchar(enquete) == 1){
      # Enqu?te normale
      message_output <- paste("L'équipe de recherche est partie se renseigner. Vous devriez avoir une réponse dans ",alea_duree," minutes environ",sep="")
    }else{
      # Enquète spéciale
      message_output <- paste("L'équipe de recherche est partie se renseigner. Cette enquête sera particulièrement longue. Nous espérons avoir des réponses 1 heure avant la fin de votre soirée.",sep="")
    }
  }else{
    alea_duree <- duree
    # Enqu?te admin
    message_output <- paste("Enquête lancée",sep="")
  }
  timer_ok <- Sys.time()+alea_duree*60
  
  print(message_output)
  
  if (is.na(resultat)){
    # Cr?ation de la BDD d?s
    valeur_alea <- creation_bdd_des(values) 
    
    # Lancement du d?
    liste_resultats <- rep(valeur_alea$resultat,valeur_alea$valeur)
    id_resultat <- sample(1:length(liste_resultats),1)
    resultat <- liste_resultats[id_resultat]
    # Echec critique
    if (resultat != 1){
      id_variation <- id_resultat + PA - 1
      resultat <- liste_resultats[min(id_resultat,length(liste_resultats))]
    }
  }
  
  # Savoir s'il y a une interception
  recup_indice <- values$actions %>% 
    filter(cible == user_name, action %in% c("interception","interception ok"),
           timer < Sys.time())
  
  if (nrow(recup_indice) > 0){
    if (pull(recup_indice[nrow(recup_indice),"action"]) != "interception ok"){
      
      recup_indice <- recup_indice[nrow(recup_indice),]
      
      recup_indice$timer <- as.POSIXct(recup_indice$timer, 
                                       origin = "1970-01-01",tz = "Europe/Paris")
      recup_indice$timer_ok <- as.POSIXct(recup_indice$timer, 
                                       origin = "1970-01-01",tz = "Europe/Paris")
      
      new_row <-  prepare_row_drive(values$id_session,user_name,
                                    "Enquete interceptee",enquete,PA,Sys.time(),
                                    resultat,timer_ok) %>% 
        add_row(prepare_row_drive(values$id_session,recup_indice$user,
                                  "interception ok",user_name,0,recup_indice$timer,
                                  resultat,recup_indice$timer_ok))
      sheet_append(id_drive, data = new_row)
      
      user_effectif <- pull(recup_indice[,'user'])
      PA_effectif <- 0
    }else{
      # Normal
      user_effectif <- user_name
      PA_effectif <- PA  
    }
  }else{		
    # Normal
    user_effectif <- user_name
    PA_effectif <- PA
  }
  
  texte_indice <- info_indices(values$actions) %>% 
    filter(indice == enquete & variation == resultat) %>% 
    pull(texte)
  
  # Chat GPT
  fl_chat_gpt <- values$actions[values$actions$action == "chat_gpt",]$resultat
  
  if (fl_chat_gpt[length(fl_chat_gpt)] == "Oui"){
    generic <- "Tu es une aide de jeu pour une soirée enquête. Il s'agit d'un scénario dans l'univers de INS/MV. Le joueur vient de demander une enquête à son équipe d'ange ou de démons, je vais te donner l'indice qu'il récupère, tu dois reformuler un peu le message, tout en gardant exactement les mêmes informations de scénario. S'il y a une prophétie dans l'indice, copie là exactement. Voici l'indice à reformuler : "
    
    try({
      #https://platform.openai.com/settings/proj_0cGdt4GJlVJwbTdYKtjE1WVW/limits
      set_chatlog(chatlog_id = ".__CURRENTCHAT__",initial_content = generic)
      answer <- chat(texte_indice,model = "gpt-4o-mini",output = "response_object")
      texte_indice <- answer$choices$message$content
    },silent = TRUE)
  }
  
  # Enregistrement du r?sultat
  new_row <-  prepare_row_drive(values$id_session,user_effectif,"enquete",enquete,
                    PA_effectif,Sys.time(),resultat,timer_ok,texte_indice)
  sheet_append(id_drive, data = new_row)
  
  # Vérification des indices bonus
  verif_bonus(id_drive,values,new_row)
  
  showModal(modalDialog(
    h3('Enquête'),
    span(message_output),
    footer = tagList(modalButton("OK"))
  ))
  
  return(message_output)
}

copie_indice <- function(id_drive,values,user_name,cible_name,PA,variation=NA,duree=NA){
  
  timer_now <- Sys.time()
  new_row <- prepare_row_drive(values$id_session,user_name,"copie",cible_name,
                               PA,timer_now,0,timer_now)
  sheet_append(id_drive, data = new_row)

  recup_indice <- values$actions %>% 
    filter(user == cible_name, action == "enquete",timer_now <= timer_ok+10*60)
  
  if (nrow(recup_indice) > 0){
    recup_indice <- recup_indice[nrow(recup_indice),]
    new_row <-  prepare_row_drive(values$id_session,user_name,"enquete",
                                  recup_indice$cible,0,timer_now,
                                  recup_indice$resultat,timer_now,
                                  recup_indice$texte)
    
    sheet_append(id_drive, data = new_row)
    
    # Vérification des indices bonus
    verif_bonus(id_drive,values,new_row)
    
    titre_indice <- info_indices(values$actions) %>% 
      filter(indice == recup_indice$cible & variation == 3) %>% 
      pull(titre)
    
    message_output <- paste("Nous avons obtenu une copie de l'enquête suivante : ",titre_indice,sep='')
    
  }else{
    message_output <- "Nous n'avons rien pu copier comme enquête"
  }
  
  showModal(modalDialog(
    h3("Copie d'enquête"),
    span(message_output),
    footer = tagList(modalButton("OK"))
  ))
  
  return(message_output)
  
}

interception_indice <- function(id_drive,values,user_name,cible_name,PA,variation=NA,duree=NA){
  timer_now <- Sys.time()
  
  if (sample(1:10,1) == 1){
    new_row <-  prepare_row_drive(values$id_session,user_name,"interception ratee",
                                  cible_name,PA,timer_now,0,timer_now)
    sheet_append(id_drive, data = new_row)
    message_output <- "Malheureusement, nos équipes se sont faites directement captées par l'équipe concurrente. L'interception est un échec. Heureusement, votre identité est restée dissimulée."	
  }else{
    new_row <-  prepare_row_drive(values$id_session,user_name,"interception",
                                  cible_name,PA,timer_now,0,timer_now+20*60)
    sheet_append(id_drive, data = new_row)
    
    message_output <- "Nous avons lancé l'interception du message, si votre cible enquête d'ici 15 minutes, vous recevrez directement le rapport d'enquête à sa place."
  }
  
  showModal(modalDialog(
    h3("Interception d'Enquête"),
    span(message_output),
    footer = tagList(modalButton("OK"))
  ))
  
  return(message_output)
}

bdd_classement  <- function(values){
  
  classement <- values$actions %>% 
    filter(action == "classement" & cible != "HACK")
  
  # classement <- subset(values$actions,action == "classement")
  classement$resultat <- as.numeric(classement$resultat)
  
  classement %>% 
    mutate(resultat = as.numeric(resultat)) %>% 
    group_by(cible) %>% 
    summarise(resultat = sum(resultat,na.rm = TRUE)) %>% 
    arrange(-resultat)
}

copie_classement <- function(id_drive,values,user_name,cible,PA,variation=NA,duree=NA){
  timer_now <- Sys.time()
  timer_ok <- Sys.time()+ifelse(is.na(duree),sample(c(2:10),1)*60,duree*60)
  
  # Accès au classement
  bdd <- bdd_classement(values)

  bdd$phrase <- paste(bdd$cible,bdd$resultat,sep = " : ")
  
  indice <- paste("Classement actuel des 10 premières entités :",paste(bdd$phrase,collapse = "\n"),sep = "\n")
  
  message <- "Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Malheureusement, nous n'avons pas su bloquer cette tentative, et une copie du classement a été effectuée."
  
  new_row <-  prepare_row_drive(values$id_session,user_name,"enquete",
                                "classement",PA,timer_now,indice,timer_ok) %>% 
    add_row(prepare_row_drive(values$id_session,"Andromalius","enquete",
                              "classement",0,timer_now,message,timer_now))

  sheet_append(id_drive, data = new_row)
  
  message_output <- "Nous avons lancé la copie du classement. Vous devriez l'obtenir dans les 10 minutes avec vos indices."
  
  showModal(modalDialog(
    h3("Modification du classement"),
    span(message_output),
    footer = tagList(modalButton("OK"))
  ))
  
  return(message_output)
}

modification_classement <- function(id_drive,values,user_name,cible,PA,variation=NA,duree=NA){
  timer_now <- Sys.time()
  
  if (sample(1:10,1) == 1){
    # Détection de l'intrusion
    message <- paste("Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Nos services ont été efficaces et ont bloqué l'intrusion. Nous savons que cette tentative provient de ",user_name,".",sep="")
    
    new_row <-  prepare_row_drive(values$id_session,user_name,"modification rate",
                                  cible,PA,timer_now,"",timer_now) %>% 
      add_row(prepare_row_drive(values$id_session,"Andromalius","enquete",
                                "classement",0,timer_now,message,timer_now))
    
    sheet_append(id_drive, data = new_row)
    
    message_output <- "Malheureusement, notre tentative d'intrusion a été détectée par les services d'Andromalius. Malheureusement, iel va savoir qu'il s'agit de vous."

  }else{
    # Modification du classement
    bdd <- bdd_classement(values)
    
    print(bdd)
    max_points <- bdd %>% summarise(max(resultat)) %>% pull
    point_cible <- pull(bdd[bdd$cible == cible,"resultat"])
    
    ecart <- max_points - point_cible
    ecart <- as.character(round(ecart + runif(1,100,500),0))
    
    message <- "Monseigneur Andromalius, nous vous informons que votre système de classement des entités vient de subir une tentative de piratage. Malheureusement, nous n'avons pas su bloquer cette tentative, et le classement a été modifié."

    new_row <-  prepare_row_drive(values$id_session,user_name,"classement",
                                  cible,PA,timer_now,ecart,timer_now) %>% 
      add_row(prepare_row_drive(values$id_session,"Andromalius","enquete",
                                "classement",0,timer_now,message,timer_now))
    
    sheet_append(id_drive, data = new_row)
    
    message_output <- "Nous avons modifié le classement comme vous l'avez souhaité."
  }
  
  showModal(modalDialog(
    h3("Modification du classement"),
    span(message_output),
    footer = tagList(modalButton("OK"))
  ))
  
  return(message_output)
}

verif_bonus <- function(id_drive,values,new_row){
  
  bonus <- values$actions %>% filter(user == "admin",action ==  "bonus") %>% 
    select(cible,PA,resultat) %>% mutate(PA = as.character(PA))
  print(bonus)
  
  last_indice <- new_row %>% filter(row_number() == n()) %>% 
    filter(action == "enquete") %>% 
    select(id_session,user,PA=resultat,cible,timer,timer_ok) %>% 
    mutate(PA = as.character(PA)) %>% left_join(bonus)
  
  # S'il existe un indice pour lequel faire un bonus
  if (nrow(last_indice)>0){
    server_name <- last_indice$id_session
    user <- last_indice$user
    indice <- last_indice$cible
    timer <- last_indice$timer
    timer_ok <- last_indice$timer_ok
    new_indice <- bonus %>% filter(cible == indice) %>% pull(resultat)
    if (length(new_indice) > 0){
      new_titre <- values$actions %>% 
        filter(user == "admin",action ==  "init") %>% 
        filter(cible == new_indice,PA == 2) %>% pull(resultat)
      
      new_row <-  prepare_row_drive(server_name,user,"bonus",
                                    new_indice,0,timer,new_titre,timer_ok)
      sheet_append(id_drive, data = new_row)
    }
  }
}


liste_indices <- function(user_name,values){
  
  # Liste des indices
  info_indices <- info_indices(values$actions) %>% 
    select(titre,texte,variation,indice)
  
  # Indice du User
  temp_indices <- values$actions %>% 
    filter(action %in% c("init","bonus") & user == user_name) %>% 
    pull(cible) %>% unique()
  
  # Vérification des indices en cascade
  # temp_enquetes_parfaites <- values$actions %>% 
  #   filter(user == user_name & resultat ==  3) %>% 
  #   mutate(indice = paste0(cible,cible)) %>% 
  #   select(indice) %>% 
  #   left_join(info_indices %>% select(indice,variation)) %>% 
  #   filter(!is.na(variation)) %>% 
  #   pull(indice) %>% unique()
  # 
  # liste_indices <- c(temp_indices,temp_enquetes_parfaites)
  liste_indices <- temp_indices

  option_indices <- list()
  for (i in liste_indices){
    titre <- info_indices %>% 
      filter(indice==i & variation == 3) %>% 
      pull(titre)
    option_indices[[titre]] <- i
  }
  
  return(option_indices)
}

# Fonction pour afficher la durée de manière pertinente
display_duration <- function(diff_seconds) {
  diff_seconds <- as.numeric(diff_seconds)
  hours <- floor(diff_seconds / 3600)  # Calculer le nombre d'heures entières
  minutes <- floor((diff_seconds %% 3600) / 60)  # Calculer le nombre de minutes restantes
  label_hours <- ifelse(hours > 1,"heures","heure")
  label_minutes <- ifelse(minutes > 1,"minutes","minute")
  if (hours > 0 && minutes > 0) {
    paste(hours, label_hours, minutes, label_minutes)
  } else if (hours > 0) {
    paste(hours, label_hours)
  } else {
    paste(minutes, label_minutes)
  }
}
