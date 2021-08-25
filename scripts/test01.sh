#!/bin/sh

# Simple script: we run server and client and check the -s output for server
# Also collect eventlog and convert it to html

cabal build all

# TODO: --eventlog-flush-interval=⟨seconds⟩
$(cabal-plan list-bin collatzer-server) +RTS -N8 -l -s -RTS --quit-after 40 &
sleep 5
$(cabal-plan list-bin collatzer-client) --quit-after 30 --threads 5

wait

eventlog2html collatzer-server.eventlog
