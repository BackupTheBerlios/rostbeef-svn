#!/bin/bash
echo "" > /var/log/ejabberd/ejabberd.log && ./build.sh && /etc/init.d/ejabberd restart && tail -f /var/log/ejabberd/ejabberd.log
