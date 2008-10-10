#!/bin/bash
cd ..
./build.sh
cd utils
/etc/init.d/ejabberd restart
tail -f /var/log/ejabberd/ejabberd.log
