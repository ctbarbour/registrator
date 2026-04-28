# Registrator

Service Registrator for Docker with DNS interface.

Registrator automatically registers and deregisters services for any Docker
container by inspecting containers as they come online. Registrator provides a
DNS interface for external systems to query for the IP address and exposed port
of a registered service.

Multiple Registrator instances form a cluster: peers discover each other over
SWIM and replicate the service registry as a CRDT (op-based ORSet) so any
node can answer DNS lookups for any service in the cluster.

## Running Registrator

The quickest way to see Registrator in action is running locally with
[docker-machine](https://docs.docker.com/machine/). Typically, running
Registrator looks like this:

    $ docker run -d \
      	     --name=registrator \
	     --volume=/var/run/docker.sock:/tmp/docker.sock \
	     -p 5555:5555/udp \
	     ghcr.io/ctbarbour/registrator:latest

Images are published from `main` to GitHub Container Registry, tagged
`latest` and the long commit SHA.

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

## Clustering

Registrator nodes gossip membership via SWIM and replicate the service
registry as a CRDT. Each node has three convergence paths:

  - **Live op-gossip.** Each `register`/`unregister` is broadcast as a
    SWIM user event tagged `(Actor, Seq)`; peers dedupe via a seen map.
  - **Startup pull.** On boot, a node joins one of its configured swim
    seeds and pulls the full ORSet from up to three seeds in parallel
    over TCP, then merges. If no seed is reachable the join is retried
    every 30s.
  - **Periodic anti-entropy.** Every 5 minutes each node pulls a
    random swim peer's full ORSet and merges, catching up any
    envelopes lost from gossip.

State-sync runs over TCP on `swim_port + 1000` (default `6000`).
Configure seeds either in `config/sys.config`:

    {groups, [
      {lan, #{port    => 5000,
              seeds   => [{"10.0.0.1", 5000}, {"10.0.0.2", 5000}],
              key     => <<"...base64 AES-256...">>}}
    ]}

or via the `REGISTRATOR_SEEDS` env var (`host:port,host:port,...`),
which appends to the config list. Hosts may be IP literals or DNS
names. Leave seeds empty on the first cluster node.

A two-node smoke cluster on podman:

    $ just smoke-cluster

spins up `reg1` (no seeds) and `reg2` (`REGISTRATOR_SEEDS=<reg1_ip>:5000`)
on a shared podman network and prints verification commands.

## Building

The toolchain is pinned via Nix flake. Enter the dev shell:

    $ nix develop          # or: direnv allow

Then use the `justfile`:

    $ just compile         # rebar3 compile
    $ just test            # rebar3 ct (incl. PropEr properties)
    $ just check           # rebar3 do xref, dialyzer
    $ just build           # podman image
    $ just smoke-cluster   # two-node cluster on podman

## CI

GitHub Actions runs two workflows:

  - **PR checks** (`.github/workflows/pr.yml`) — on every pull request
    to `main`: `rebar3 compile`, `rebar3 do xref, dialyzer`, and
    `rebar3 ct` (which exercises the PropEr properties via
    `ct_property_test`).
  - **Build and publish** (`.github/workflows/main.yml`) — on push to
    `main`: same build/test job, then builds the Dockerfile and pushes
    `ghcr.io/ctbarbour/registrator` tagged with the long commit SHA
    and `latest`.
