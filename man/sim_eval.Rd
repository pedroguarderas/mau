\name{sim_eval}
\alias{sim_eval}

\title{
sim_eval
}

\description{
  Macro para simular diferentes modelos MAUT al cambiar los pesos siguiendo una distribución de 
  Dirichlet.
}

\usage{
sim_eval( N = 1, 
          W = stop("W not supplied"), 
          S = stop("S not supplied"), 
          F = stop("F not supplied"), 
          p = stop("p not supplied") )
}

\arguments{
  \item{N}{
  Número de simulaciones.
  }
  \item{W}{
  Pesos simulados con distribución de Dirichlet y concentrados según el valor de los indicadores.
  }
  \item{S}{
  Simulaciones.
  }
  \item{F}{
  Matriz de indicadores evaluados.
  }
  \item{p}{
  Vector de pesos extraídos de Logical Decisions.
  }
}
\details{
}
\value{
  La función \code{sim_eval} retorna los siguientes objetos:

  \item{S}{Data frame de Simulaciones.}
  \item{W}{Matriz de Pesos simulados.}
}
\references{
}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar.
}
\note{
}

\seealso{
}

\examples{
# Matriz de indicadores
indic<-data.frame(cod=c('A', 'B', 'C', 'D'), 
                  ind1=c(0.3428570, 1, 1, 1),
                  ind2=c(0.5, 0.5, 1, 0.5), 
                  ind3=c(0, 0.2696746, 0.6751261, 0.7401660),
                  ind4=c(0.2797259, 0.2981198, 1, 0.1952864))

# Matriz de pesos
rank<-data.frame(cod=c('A', 'B', 'C', 'D'), 
                 weigth=c(0.0376573868, 0.0264447427, 0.0240244958, 0.0179347528))

N<-10
p<-rank$weigth

sim_eval( N, W, S, indic, p )
}
