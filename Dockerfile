FROM rocker/shiny
RUN echo "deb http://ftp.us.debian.org/debian/ testing main contrib non-free" >> /etc/apt/sources.list
RUN apt-get update -y
RUN apt-get install -y pandoc supervisor nginx
RUN R -q -e 'install.packages("stringi")'
RUN R -q -e 'install.packages("r2d3")'
RUN R -q -e 'install.packages("ggplot2")'
RUN R -q -e 'install.packages("shiny")'

RUN R -q -e 'install.packages("shinyjs")'
RUN R -q -e 'install.packages("data.table")'
RUN R -q -e 'install.packages("shinythemes")'

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/01_hello
ADD system/. /home/shiny/system/
EXPOSE 8888
CMD ["/bin/sh", "-c", "/usr/bin/supervisord -c /home/shiny/system/sup.conf"]
