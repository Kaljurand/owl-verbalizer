FROM swipl:8.5.5

RUN mkdir /app
COPY make_exe.sh *.pl /app/


WORKDIR /app

RUN /app/make_exe.sh

CMD /app/owl_to_ace.exe -httpserver -port 5123

EXPOSE 5123
