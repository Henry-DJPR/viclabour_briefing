FROM rocker/r-base:4.3.1
COPY data /data 
COPY metadata /metadata 
COPY R /R 
COPY metadata /metadata 
COPY renv /renv 
COPY assets /assets 
COPY renv.lock table_index.csv update_data.R viclabour_briefing.qmd .Rprofile /
COPY assets/VIC-Regular.ttf /usr/local/share/fonts/VIC-Regular.ttf
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.450/quarto-1.3.450-linux-amd64.deb 
RUN dpkg -i quarto-1.3.450-linux-amd64.deb
RUN apt-get update
RUN apt-get -y install libxml2-dev
RUN fc-cache -fv
RUN quarto install tinytex
RUN echo "RENV_CONFIG_SANDBOX_ENABLED = FALSE" >> .Rprofile
RUN Rscript -e "renv::restore()"
RUN quarto render viclabour_briefing.qmd
RUN rm quarto-1.3.450-linux-amd64.deb viclabour_briefing.pdf
RUN Rscript -e "writeLines(gsub('latex-auto-install: true', 'latex-auto-install: false', readLines('viclabour_briefing.qmd')), 'viclabour_briefing.qmd')"
CMD quarto render viclabour_briefing.qmd
