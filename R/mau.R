# Loading libraries for mau package ----------------------------------------------------------------
library(data.table)
library(gtools)
library(ggplot2)
library(grid)
library(igraph)
library(RColorBrewer)
library(reshape2)
library(stringr)
library(xlsx)


# Funciones para corregir caracteres ---------------------------------------------------------------
source( 'R/Stand.String.R' )

# Función para leer funciones de utilidad que provienen del Logical Decision -----------------------
source( 'R/Read.Utilities.R' )
source( 'R/Eval.Utilities.R' )

# Incluyendo simulaciones --------------------------------------------------------------------------
source( 'R/Simulate.Weights.R' )

# Carga árbol Logical Decisions --------------------------------------------------------------------
source( 'R/Read.Tree.R' )
source( 'R/Make.Tree.R' )
source( 'R/Write.Tree.R' )

# Análisis de sensitividad -------------------------------------------------------------------------
source( 'R/Weight.Sensitivity.R' )

# Análisis de saltos -------------------------------------------------------------------------------
source( 'R/analisis_salto.R' )

# Evaluación de criterios y subcriterios -----------------------------------------------------------
source( 'R/eval_criteria.R' )

# Funciones para análisis basado en la teoría de imposibilidad de Arrow ----------------------------
# source( 'R/arrow.R' )

# Función que evalua el modelo criterios y subcriterios a partir de utilidades y árbol -------------
source( 'R/modelo_tabla.R' )

# Funciones para graficos --------------------------------------------------------------------------
source( 'R/evalgraphs.R' )

# Spider plot --------------------------------------------------------------------------------------
source( 'R/spider_plot.R' )

