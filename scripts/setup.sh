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
    sudo cp -v $SCRIPT_LOCATION/90-cvmfs-notify-rotate-systemd /etc/logrotate.d/
    sudo systemctl restart rsyslog
    sudo cp $SCRIPT_LOCATION/cvmfs-notify.service /etc/systemd/system/cvmfs-notify.service
    sudo systemctl daemon-reload
fi

if [ ! -e /etc/cvmfs/notify ]; then
    sudo mkdir /etc/cvmfs/notify
fi
if [ ! -e /etc/cvmfs/notify/config.json ]; then
    sudo cp $SCRIPT_LOCATION/../etc/config.json /etc/cvmfs/notify/
fi

