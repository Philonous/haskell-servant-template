FROM philonous/stack-run:latest

ADD ./dist/* /app/
ADD scripts/run.sh /run.sh

ENTRYPOINT ["/run.sh"]
