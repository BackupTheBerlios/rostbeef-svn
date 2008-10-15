#!/bin/bash
/etc/init.d/ejabberd restart
tail -f /var/log/ejabberd/ejabberd.log
