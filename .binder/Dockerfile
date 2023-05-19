FROM rocker/binder:4.3.0
LABEL maintainer='Denisse Fierro Arcos'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}



RUN wget https://github.com/lidefi87/SEE_R_Datos-Espaciales/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2023-05-19/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
