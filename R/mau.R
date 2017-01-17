# Loading libraries for mau package ----------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(gtools)
library(gWidgets2tcltk)
library(igraph)
library(nleqslv)
library(nloptr)
library(optimx)
# library(plotrix)
library(RColorBrewer)
library(reshape2)
library(stringr)
library(xlsx)

# Funciones para corregir caracteres ---------------------------------------------------------------
source( 'R/corchar.R' )

# Función para leer funciones de utilidad que provienen del Logical Decision -----------------------
source( 'R/Read.Utilities.R' )
source( 'R/Eval.Utilities.R' )

# Incluyendo simulaciones --------------------------------------------------------------------------
source( 'R/simulacion.R' )

# Porcentaje fechas --------------------------------------------------------------------------------
source( 'R/dates.R' )

# Carga árbol Logical Decisions --------------------------------------------------------------------
source( 'R/loadtree.R' )

# Modelo AHP ---------------------------------------------------------------------------------------
source( 'R/ahpmatrix.R' )

# Análisis de sensitividad -------------------------------------------------------------------------
source( 'R/sensitivity.R' )

# Análisis de saltos -------------------------------------------------------------------------------
source( 'R/analisis_salto.R' )

# Evaluación de criterios y subcriterios -----------------------------------------------------------
source( 'R/eval_criteria.R' )

# Funciones para análisis basado en la teoría de imposibilidad de Arrow ----------------------------
source( 'R/arrow.R' )

# Función que evalua el modelo criterios y subcriterios a partir de utilidades y árbol -------------
source( 'R/modelo_tabla.R' )

# Funciones para graficos --------------------------------------------------------------------------
source( 'R/evalgraphs.R' )

# Spider plot --------------------------------------------------------------------------------------
source( 'R/spider_plot.R' )

# Obetner password ---------------------------------------------------------------------------------
source( 'R/getpassword.R' )
