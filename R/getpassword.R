getPassword <- function( text ){
  tt<-tktoplevel()
  pass<-tclVar("")
  label.widget<-tklabel(tt, text = text )
  password.widget<-tkentry(tt,show="*",textvariable=pass)
  ok<-tkbutton(tt,text="Ok",command=function()tkdestroy(tt))
  tkpack(label.widget, password.widget,ok)
  tkwait.window(tt)
  return( tclvalue(pass) )
}
