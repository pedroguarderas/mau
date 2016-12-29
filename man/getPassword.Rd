\name{getPassword}
\alias{getPassword}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interface para obtener credenciales de usuario}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPassword(users, users_short, proceso, proceso_short)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{users}{
%%     ~~Describe \code{users} here~~
}
  \item{users_short}{
%%     ~~Describe \code{users_short} here~~
}
  \item{proceso}{
%%     ~~Describe \code{proceso} here~~
}
  \item{proceso_short}{
%%     ~~Describe \code{proceso_short} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (users, users_short, proceso, proceso_short) 
{
    main <- gwindow(title = "Credenciales base CEAACES", width = 400, 
        height = 200, visible = TRUE, parent = c(400, 200))
    container <- ggroup(container = main, horizontal = FALSE)
    lbl_users <- glabel(container = container, text = "Usuario: ")
    select_users <- gcombobox(users, selected = -1, container = container, 
        editable = FALSE)
    lbl_password <- glabel(container = container, text = "Password: ")
    txt_password <- gedit(container = container)
    visible(txt_password) <- FALSE
    lbl_procesos <- glabel(container = container, text = "Proceso: ")
    select_proceso <- gcombobox(proceso, selected = -1, container = container, 
        editable = FALSE)
    btn_login <- gbutton(container = container, text = "Login")
    do_login <- function(btn_login) {
        if (length(svalue(select_users)) != 0 & length(svalue(txt_password)) != 
            0 & length(svalue(select_proceso)) != 0) {
            cat("Login para:", svalue(select_users), "\nEn el proceso:", 
                svalue(select_proceso), "\nVerifique la información.\nPresione [enter] para continuar...\n")
            map <- setNames(users_short, users)
            usuario_base <<- as.character(map[unlist(svalue(select_users))])
            clave_base <<- as.character(svalue(txt_password))
            map <- setNames(proceso_short, proceso)
            proceso_selec <<- as.character(map[unlist(svalue(select_proceso))])
            dispose(main)
        }
        else galert("No sea bestia!\nIngrese todos los campos!", 
            title = "Advertencia", delay = 10, visible = TRUE, 
            parent = c(500, 300))
    }
    invisible(addHandlerClicked(btn_login, do_login))
    cat("")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
