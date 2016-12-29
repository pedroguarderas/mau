# Password menu-------------------------------------------------------------------------------------
getPassword <- function(users,users_short,proceso,proceso_short){
  main <- gwindow(title='Credenciales base CEAACES',width=400,height=200,
                visible=TRUE,parent=c(400,200)) 
  container <- ggroup(container=main,horizontal = FALSE)
  lbl_users <- glabel(container=container, text='Usuario: ') 
  select_users <- gcombobox(users,selected=-1,container=container,editable=FALSE)
  lbl_password <- glabel(container=container, text='Password: ') 
  txt_password <- gedit(container=container) 
  visible(txt_password) <- FALSE
  lbl_procesos <- glabel(container=container, text='Proceso: ') 
  select_proceso <- gcombobox(proceso,selected=-1,container=container,editable=FALSE)
  btn_login <- gbutton(container=container, text='Login') 
  
  do_login <- function(btn_login){ 
    if(length(svalue(select_users))!=0 & length(svalue(txt_password))!=0 & 
       length(svalue(select_proceso))!=0){
      
      cat('Login para:',svalue(select_users),'\nEn el proceso:',
          svalue(select_proceso),
          '\nVerifique la información.\nPresione [enter] para continuar...\n')
      
      map<-setNames(users_short,users)
      usuario_base <<- as.character(map[unlist(svalue(select_users))])
      
      clave_base <<- as.character(svalue(txt_password))
      
      map<-setNames(proceso_short,proceso)
      proceso_selec <<- as.character(map[unlist(svalue(select_proceso))])
      
      dispose(main) }
    else
      galert('No sea bestia!\nIngrese todos los campos!',title='Advertencia',delay=10,
             visible=TRUE,parent=c(500,300)) }
  
  invisible(addHandlerClicked(btn_login,do_login))
  cat('')
}