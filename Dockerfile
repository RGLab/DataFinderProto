FROM fredhutch/r-shiny-base:latest
RUN echo "deb http://ftp.us.debian.org/debian/ testing main contrib non-free" >> /etc/apt/sources.list
RUN apt-get update -y
RUN apt-get install -y pandoc supervisor nginx
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny
RUN R -q -e 'install.packages("stringi")'
RUN R -q -e 'install.packages("r2d3")'
RUN R -q -e 'install.packages("ggplot2")'
RUN R -q -e 'install.packages("shiny")'
ADD app/. /home/shiny/
ADD system/. /home/shiny/system/
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
EXPOSE 8888
# try to avoid greying out of the apps
# https://stackoverflow.com/questions/44397818/shiny-apps-greyed-out-nginx-proxy-over-ssl
# RUN mkdir /etc/shiny-server && echo 'sanitize_errors off;disable_protocols xdr-streaming xhr-streaming iframe-eventsource iframe-htmlfile;' >> /etc/shiny-server/shiny-server.conf
# TODO REMOVE vvv
EXPOSE 7777
# USER shiny
# CMD Rscript start.R
# TODO REMOVE ^^^ and uncomment vvv
CMD ["/bin/sh", "-c", "/usr/bin/supervisord -c /home/shiny/system/sup.conf"]
