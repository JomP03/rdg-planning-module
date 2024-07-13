FROM swipl

COPY . /app
EXPOSE 2226

ENTRYPOINT ["swipl"]
CMD ["/app/server.pl",  "--user=daemon", "--fork=false", "--port=2226"]