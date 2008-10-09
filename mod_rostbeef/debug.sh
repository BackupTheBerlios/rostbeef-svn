#!/bin/bash
echo "" > /var/log/ejabberd/ejabberd.log &&make && /etc/init.d/ejabberd restart && tail -f /var/log/ejabberd/ejabberd.log
