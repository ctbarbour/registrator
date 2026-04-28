gar_registry := "europe-west2-docker.pkg.dev/soulbeet/soulbeet"
gar_image := gar_registry + "/registrator"
image := "registrator"

default:
    @just --list

compile:
    rebar3 compile

check: compile
    rebar3 do xref, dialyzer

test: compile
    rebar3 ct

# Build image (no push)
build tag="latest":
    podman build -t {{ image }}:{{ tag }} .

# Build and publish to Artifact Registry. Pass "true" as second arg to also
# tag :latest.
publish tag="latest" also_latest="false": (build tag)
    #!/usr/bin/env bash
    set -euo pipefail
    if [ -z "${GAR_KEY_FILE:-}" ]; then
        echo "FAIL: GAR_KEY_FILE not set"
        echo "Run 'direnv allow' or check .envrc sops decryption"
        exit 1
    fi
    IMAGE="{{ gar_image }}:{{ tag }}"
    echo "==> Authenticating crane with Artifact Registry..."
    crane auth login europe-west2-docker.pkg.dev -u _json_key -p "$(cat "${GAR_KEY_FILE}")"
    echo "==> Saving image to tar..."
    rm -f /tmp/registrator.tar
    podman save "{{ image }}:{{ tag }}" -o /tmp/registrator.tar
    echo "==> Pushing ${IMAGE}..."
    crane push /tmp/registrator.tar "${IMAGE}"
    rm -f /tmp/registrator.tar
    echo "==> Published ${IMAGE}"
    if [ "{{ also_latest }}" = "true" ]; then
        echo "==> Tagging ${IMAGE} as latest..."
        crane tag "${IMAGE}" latest
        echo "==> Published {{ gar_image }}:latest"
    fi

# Build + publish a versioned release (e.g. `just release v1.2.3`) and tag :latest
release version: (publish version "true")

# Run the latest local image against the rootless podman socket.
# Requires `systemctl --user start podman.socket` once.
run-podman tag="latest":
    #!/usr/bin/env bash
    set -euo pipefail
    SOCKET="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/podman/podman.sock"
    if [ ! -S "$SOCKET" ]; then
        echo "FAIL: podman socket not found at $SOCKET"
        echo "Start it with: systemctl --user enable --now podman.socket"
        exit 1
    fi
    echo "==> Using podman socket: $SOCKET"
    podman run --rm -d --name registrator \
        -e REGISTRATOR_DOCKER_SOCKET="$SOCKET" \
        -v "$SOCKET:$SOCKET" \
        -p 5555:5555/udp \
        {{ image }}:{{ tag }}
    podman logs -f registrator

# Run the latest local image against /var/run/docker.sock (Docker default).
run tag="latest":
    podman run --rm -d --name registrator \
        -v /var/run/docker.sock:/var/run/docker.sock:ro \
        -p 5555:5555/udp \
        {{ image }}:{{ tag }}
    podman logs -f registrator

# Boot two registrators on a shared podman network so we can verify SWIM
# replication. After this returns, attach to either node's remote_console
# and call swim:join(lan, {Peer_Ip_Tuple, 7946}) to bootstrap, then register
# a service on one and verify it appears via lookup on the other.
smoke-cluster tag="latest": (build tag)
    #!/usr/bin/env bash
    set -euo pipefail
    SOCKET="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/podman/podman.sock"
    if [ ! -S "$SOCKET" ]; then
        echo "FAIL: podman socket not found at $SOCKET"
        exit 1
    fi
    podman rm -f reg1 reg2 2>/dev/null || true
    podman network exists swim-cluster || podman network create swim-cluster
    podman run -d --name reg1 --network swim-cluster \
        -e REGISTRATOR_DOCKER_SOCKET="$SOCKET" \
        -v "$SOCKET:$SOCKET" \
        {{ image }}:{{ tag }} > /dev/null
    REG1_IP=$(podman inspect reg1 | jq -r '.[0].NetworkSettings.Networks["swim-cluster"].IPAddress')
    podman run -d --name reg2 --network swim-cluster \
        -e REGISTRATOR_DOCKER_SOCKET="$SOCKET" \
        -e REGISTRATOR_SEEDS="${REG1_IP}:7946" \
        -v "$SOCKET:$SOCKET" \
        {{ image }}:{{ tag }} > /dev/null
    REG2_IP=$(podman inspect reg2 | jq -r '.[0].NetworkSettings.Networks["swim-cluster"].IPAddress')
    echo
    echo "==> reg1 ip: $REG1_IP   (bootstrap, no seeds)"
    echo "==> reg2 ip: $REG2_IP   (REGISTRATOR_SEEDS=${REG1_IP}:7946)"
    echo
    echo "Next steps:"
    echo "  Terminal A: podman exec -it reg1 /opt/registrator/bin/registrator remote_console"
    echo "  Terminal B: podman exec -it reg2 /opt/registrator/bin/registrator remote_console"
    echo
    echo "  In either terminal (verify auto-join):"
    echo "    swim:members(lan).      %% should list both nodes within ~1s of reg2 boot"
    echo
    echo "  In Terminal A (register a service):"
    echo "    Svc = #{id => <<\"smoke-1\">>, port => 9999, address => {127,0,0,1},"
    echo "            service => <<\"smoke\">>, protocol => <<\"tcp\">>}."
    echo "    registrator_nodes:register(Svc)."
    echo
    echo "  In Terminal B (verify replication via op-gossip):"
    echo "    registrator_nodes:lookup(<<\"smoke\">>, <<\"tcp\">>)."
    echo "    %% expect [#{id => <<\"smoke-1\">>, ...}]"
    echo
    echo "  Cleanup: podman rm -f reg1 reg2; podman network rm swim-cluster"
