FROM fredhutch/r-shiny-base:latest
RUN apt-get update
RUN apt-get install -y pandoc
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny
RUN R -q -e 'install.packages("stringi")'
RUN R -q -e 'install.packages("r2d3")'
RUN R -q -e 'install.packages("ggplot2")'
RUN R -q -e 'install.packages("shiny")'
ADD app/. /home/shiny/
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
USER shiny
EXPOSE 7777
CMD Rscript start.R 
