#___________________________________________________________________________________________________
# Carga de librerías necesarias para evalpack
library(data.table)
library(dplyr)
library(gtools)
library(ggplot2)
library(igraph)
library(nloptr)
library(optimx)
library(reshape2)
library(RColorBrewer)
library(stringr)
library(xlsx)
library(plotrix)
library(grid)

#___________________________________________________________________________________________________
# Funciones para corregir caracteres
source( 'R/corchar.R' )

#___________________________________________________________________________________________________
# Función para leer funciones de utilidad que provienen del Logical Decision
source( 'R/funcion_utilidad.R' )

#___________________________________________________________________________________________________
# Incluyendo simulaciones
source( 'R/simulacion.R' )

#___________________________________________________________________________________________________
# Porcentaje fechas
source( 'R/dates.R' )

#___________________________________________________________________________________________________
# Carga árbol Logical Decisions
source( 'R/loadtree.R' )

#___________________________________________________________________________________________________
# Modelo AHP
source( 'R/ahpmatrix.R' )

#___________________________________________________________________________________________________
# Análisis de sensitividad
source( 'R/sensitivity.R' )

#___________________________________________________________________________________________________
# Análisis de saltos
source( 'R/analisis_salto.R' )

#___________________________________________________________________________________________________
# Evaluación de criterios y subcriterios
source( 'R/eval_criteria.R' )

#___________________________________________________________________________________________________
# Funciones para análisis basado en la teoría de imposibilidad de Arrow
source( 'R/arrow.R' )

#___________________________________________________________________________________________________
# Función que evalua el modelo criterios y subcriterios a partir de utilidades y árbol
source( 'R/modelo_tabla.R' )

#___________________________________________________________________________________________________
# Funciones para graficos

source( 'R/evalgraphs.R' )

