# Registrator

Service Registrator for Docker with DNS interface.

[![Build Status](https://travis-ci.org/barbct5/registrator.svg)](https://travis-ci.org/barbct5/registrator)
[![Docker Hub](https://img.shields.io/badge/docker-ready-blue.svg)](https://registry.hub.docker.com/u/barbct5/registrator/)

Registrator automatically registers and deregisters services for any Docker
container by inspecting containers as they come online. Registrator provides a
DNS interface for external systems to query for the IP address and exposed port
of a registered service.

## Running Registrator

The quickest way to see Registrator in action is running locally with
[docker-machine](https://docs.docker.com/machine/). Typically, running
Registrator looks like this:

    $ docker run -d \
      	     --name=registrator \
	     --volume=/var/run/docker.sock:/tmp/docker.sock \
	     -p 5555:5555/udp \
	     barbct5/registrator:latest

First we run the container detached and name it `registrator`. We also mount
the Docker socket inside in the container so the Registrator service can listen
for Docker lifecycle events. You should be able see the logs for Registrator

    $ docker logs registrator


## Running Redis

As you start containers, if they provide a service, they will be added to the
service registry. Let's try running redis in a container:

    $ docker -d --name=redis -P redis

We run the redis container detached and expose all defined ports in the Dockefile
and let Docker assign a random port on the host. Since Registrator provides
service discovery for external systems, the host port doesn't matter. You can
find the newly started redis service:

```
   $ dig @$(docker-machine ip dev) -p 5555 ANY _redis._tcp.example.com

   ;; Warning: query response not set

   ; <<>> DiG 9.8.3-P1 <<>> @127.0.0.1 -p 5555 ANY _redis._tcp.example.com
   ; (1 server found)
   ;; global options: +cmd
   ;; Got answer:
   ;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 6333
   ;; flags: rd; QUERY: 1, ANSWER: 2, AUTHORITY: 0, ADDITIONAL: 0
   ;; WARNING: recursion requested but not available

   ;; QUESTION SECTION:
   ;_redis._tcp.example.com.	IN	ANY

   ;; ANSWER SECTION:
   _redis._tcp.example.com. 0	IN	A	192.168.99.100
   _redis._tcp.example.com. 0	IN	SRV	10 10 32770 192.168.99.100.

   ;; Query time: 0 msec
   ;; SERVER: 127.0.0.1#5555(127.0.0.1)
   ;; WHEN: Mon Dec 21 12:29:53 2015
   ;; MSG SIZE  rcvd: 91
```

With the IP address and port for the redis service running the container we can
connect via the cli:

    $ redis-cli -h 192.168.99.100 -p 32770

## Building

We use rebar3 as the standard build tool for Registrator. To compile and build
a release:

    $ ./rebar3 release

We can build a Docker image:

    $ docker build --tag registrator:latest .
