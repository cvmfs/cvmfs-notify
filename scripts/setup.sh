#!/bin/sh

#-------------------------------------------------------------------
#
# This file is part of the CernVM File System.
#
#-------------------------------------------------------------------

set -e

# Install syslog configuration file
 if [ x"$(which systemctl)" != x"" ]; then
    systemctl daemon-reload
    systemctl restart rsyslog
fi

if [ ! -e /etc/cvmfs/notify ]; then
    mkdir /etc/cvmfs/notify
fi
if [ ! -e /etc/cvmfs/notify/config.json ]; then
    cp /usr/libexec/cvmfs-notify/etc/config.json /etc/cvmfs/notify/
fi

