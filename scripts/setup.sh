#!/bin/sh

#-------------------------------------------------------------------
#
# This file is part of the CernVM File System.
#
#-------------------------------------------------------------------

set -e

SCRIPT_LOCATION=$(cd "$(dirname "$0")"; pwd)

# Install syslog configuration file
 if [ x"$(which systemctl)" != x"" ]; then
    systemctl daemon-reload
    systemctl restart rsyslog
fi

if [ ! -e /etc/cvmfs/notify ]; then
    mkdir /etc/cvmfs/notify
fi
if [ ! -e /etc/cvmfs/notify/config.json ]; then
    cp $SCRIPT_LOCATION/../etc/config.json /etc/cvmfs/notify/
fi

