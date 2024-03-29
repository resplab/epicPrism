FROM opencpu/base:v2.2.7
RUN apt-get update
RUN apt-get install -y libcurl4-gnutls-dev
RUN R -e 'install.packages("survminer")'
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("resplab/epicR")'
RUN R -e 'remotes::install_github("resplab/epicPrism")'
RUN echo "opencpu:opencpu" | chpasswd
