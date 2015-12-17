FROM voidlock/erlang:18.1.3-onbuild

RUN apt-get update -qq -y && \
    apt-get install -qq -y socat supervisor && \
    apt-get clean

ADD ./scripts/start_socat.sh /start_socat.sh
ADD ./scripts/run.sh /run.sh
ADD ./scripts/supervisord-socat.conf /etc/supervisor/conf.d/supervisord-socat.conf

RUN chmod 755 /*.sh

EXPOSE 5555/udp

VOLUME /root/.cache
VOLUME /usr/src/app/_build

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord-socat.conf"]