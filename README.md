# Predicting-dengue-spread
Proyecto final BDML 2022

Este repositorio alberga el desarrollo de una herramienta que permite hacer uso de una serie de variables medioambientales para predecir la cantidad de casos de dengue en San Juan (Puerto Rico) e Iquitos (Perú).
Autores:
[Camilo Bonilla](https://github.com/cabonillah),  [Nicolás Velásquez](https://github.com/Nicolas-Velasquez-Oficial) y  [Rafael Santofimio](https://github.com/rasantofimior/)

# Resumen
Seg ́un la OPS (2020) en Am ́erica Latina cerca de 500 millones de personas est ́an en riesgo de contraer dengue, tanto que en la  ́ultima d ́ecada se registraron 16.2 millones de casos. En el continente se encuentran cuatro (4) serotipos (DENV-1, DENV-2, DENV-3 y DEN-V 4) que pueden ocasionar la muerte cuando un individuo contrae m ́as de uno. Es por esto que los gobiernos generan planes de contingencia, vigilancia y control de brotes y epidemias de esta enfermedad. En cuanto al impacto econ ́omico, se estima que, para el 2016, la región de las Am ́ericas tuvo un costo anual de USD 3 billones asociado al dengue (OPS, 2018). Es por esto que resulta indispensable desarrollar herramientas que permitan predecir el brote y esparcimiento de esta enfermedad con la mayor precisi ́on posible, dado que no solo representa un costo en t ́erminos de tratamiento sino tambi ́en la perdida de bienestar social y destrucci ́on del aparato productivo cuando se presentan muertes. Estas deben propender por enfocar los esfuerzos de los gobiernos en mejorar la vigilancia en zonas apartadas o con altos factores riesgo, y a su vez ser de f ́acil acceso y uso tanto para las autoridades como cualquier ciudadano.

Bajo este panorama, la propuesta para el proyecto final de Big Data and Machine Learning corresponde a generar una herramienta que permita hacer uso de una serie de variables medioambientales para predecir la cantidad de casos de dengue en San Juan (Puerto Rico) e Iquitos (Per ́u). Esta comprende no solo el desarrollo modelos con diferentes especificaciones sino ir m ́as all ́a, al desplegar un aplicativo web que permita capturar par ́ametros ingresados por un usuario (autoridades de salud y comunidad en general) y con base en estos, realizar predicciones del brote o esparcimiento de esta enfermedad. Es importante aclarar, que el proyecto se enmarca en una competencia en curso promovida por DrivenData, de manera que la base de datos ha sido provista por esta plataforma.
 
Este repositorio contiene las siguientes carpetas:

## Carpeta Document

## Carpeta Stores
Esta carpeta alberga bases de datos relacionadas con el entrenamiento y testeo. Así como los resultados de cada modelo propuesto.
- dengue_features_test.csv
- dengue_features_train.csv
- dengue_labels_train.csv

## Carpeta scripts:

## Carpeta views:

## Carpeta biblio:

Notas:

-   Si se ejecutan los scripts desde programas como R Studio, se debe asegurar antes que el directorio base se configure en "poverty_pred\scripts".
-   Se recomienda enfacticamnete seguir las instrucciones y comentarios del código (en orden y forma). Así mismo, es importante que antes de verificar la              replicabilidad del código, se asegure de tener **todos** los requerimientos informáticos previamente mencionados (i.e. se prefieren versiones de **R** menores a la 4.2.1. para evitar que paquetes, funciones y métodos que han sido actualizados no funcionen). Además, la velocidad de ejecución dependerá de las características propias de su máquina, por lo que deberá (o no) tener paciencia mientras se procesa.*
