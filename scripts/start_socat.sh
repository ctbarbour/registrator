#!/bin/bash

socat TCP-LISTEN:2376,bind=127.0.0.1,fork,reuseaddr,range=127.0.0.1/8 UNIX-CLIENT:/tmp/docker.sock
