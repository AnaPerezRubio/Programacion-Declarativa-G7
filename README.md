# Programación Declarativa grupo 7

**Repositorio para el trabajo de la asignatura.**

Este proyecto contiene las diferentes propuestas de solución desarrolladas por los miembros del grupo. Se ha configurado un flujo de trabajo basado en ramas individuales y **Pull Requests (PR)** para garantizar que solo la mejor solución consensuada llegue a la rama principal (`master`).

## Miembros del Equipo

- [Ana Pérez Rubio]
- [Lucía Novalio Rodríguez]
- [Fernando Manuel Jiménez Ramos]
- [Javier Arroyo Marcos]
- [Tommasso Sacenti]

---

## Flujo de Trabajo

La rama `master` de este repositorio está **protegida**. No se permite hacer `push` directamente sobre ella. Para aportar una solución se deben seguir estos pasos:

### 1. Clonar el repositorio y crear tu rama individual
Cada participante debe desarrollar su solución en una rama propia. Abre una terminal y ejecuta:

```bash
# 1. Clona el repositorio en tu ordenador
git clone <URL_DEL_REPOSITORIO>

# 2. Entra en la carpeta del proyecto
cd <NOMBRE_DEL_REPOSITORIO>

# 3. Crea una nueva rama con tu nombre y muévete a ella
git checkout -b solucion-<tu-nombre>
```

### 2. Escribir el código y subir tu solución
Una vez que hayas terminado la tarea individual, debes subir los archivos únicamente a tu rama:

```Bash
# 1. Prepara todos los archivos que has creado/modificado
git add .

# 2. Crea un commit explicando qué has subido
git commit -m "Añadida la propuesta de solución de <tu-nombre>"

# 3. Sube tu rama a GitHub
git push origin solucion-<tu-nombre>
```
### 3. Crear un Pull Request
Una vez todas las propuestas estén subidas y decidamos la que irá a la rama principal, se siguen estos pasos:
1. Ir a la página principal de este repositorio en GitHub.
2. Tras haber hecho el push, habrá un aviso amarillo con un botón verde que dice "Compare & pull request". Hacer clic ahí.
3. Asegurar que la rama base es master y la rama compare es la rama elegida.
4. Ponerle un título y hacer clic en **Create pull request**.
(Ahora deberíamos tener 5 Pull Requests abiertos en el repositorio, uno por cada miembro).

Una vez estén las 5 soluciones subidas, el grupo se reunirá para debatir y revisar el código en la pestaña de Pull requests.
Cuando se decida de forma unánime cuál es la solución ganadora, un compañero distinto al autor debe entrar en ese PR y aprobarlo.
Tras la aprobación, hacer clic en el botón verde **Merge pull request**.
