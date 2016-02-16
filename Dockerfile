FROM ubuntu:15.10
MAINTAINER Phil Toland <phil@hippware.com>

ENV HOME /opt/wocky
ENV PATH /opt/wocky/bin/:$PATH

# install required packages
RUN apt-get update && apt-get install -y libssl-dev libexpat1 libexpat1-dev

ADD _build/default/rel/wocky /opt/wocky
RUN rm -rf /opt/wocky/log && ln -s /data/log /opt/wocky/log

ADD ./start.sh start.sh

# expose xmpp, s2s, epmd, distributed erlang
EXPOSE 5222 5269 4369 9100

# Define mount points.
VOLUME ["/data/mnesia", "/data/log"]

ENTRYPOINT ["./start.sh"]
