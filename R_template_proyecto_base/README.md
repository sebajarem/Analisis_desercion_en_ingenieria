# templateTest

¡Bienvenido a ProjectTemplate!

Este archivo te presenta ProjectTemplate, pero eventualmente deberías reemplazar
El contenido de este archivo con una introducción a su proyecto. 
Gente que trabajar con sus datos en el futuro lo agradecerá, incluido su futuro
yo.

ProjectTemplate es un paquete R que te ayuda a organizar tus estadísticas Análisis de proyectos. 
Como estás leyendo este archivo, asumiremos que has ya llamado `create.project ()` 
para configurar este proyecto y todos sus contenido.

Para cargar su nuevo proyecto, primero necesitará `setwd ()` en el directorio
donde se encuentra este archivo README. Entonces necesitas ejecutar los siguientes dos
líneas de código R:

	library('ProjectTemplate')
	load.project()

Después de ingresar la segunda línea de código, verá una serie de
mensajes a medida que ProjectTemplate realiza su trabajo. 
Este trabajo implica:
* Lectura en el archivo de configuración global contenido en `config`.
* Carga de cualquier paquete R que haya enumerado en el archivo de configuración.
* Lectura en cualquier conjunto de datos almacenado en `data` o` cache`.
* Preprocesando sus datos usando los archivos en el directorio `munge`.

Una vez hecho esto, puede ejecutar cualquier código que desee. Para cada análisis
creas, te recomendamos poner un archivo separado en el directorio `src`.
Si los archivos comienzan con las dos líneas mencionadas anteriormente:

biblioteca ('ProjectTemplate')
load.project ()

Tendrá acceso a todos sus datos, ya completamente preprocesados, y
todas las bibliotecas que quieras usar.

Para obtener más detalles sobre ProjectTemplate, consulte http://projecttemplate.net
