#!/bin/bash
erlc batch_read.erl && \
erl -pa '/usr/lib/erlang/lib/mnesia-4.3.3/ebin' \
    -mnesia dir '"/tmp/mnesia.test"' \
    -s batch_read test \
    -s init stop