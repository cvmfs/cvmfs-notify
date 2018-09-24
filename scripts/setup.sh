#!/bin/sh

#-------------------------------------------------------------------
#
# This file is part of the CernVM File System.
#
#-------------------------------------------------------------------

set -e

SCRIPT_LOCATION=$(cd "$(dirname "$0")"; pwd)

export RUNNER_LOG_DIR=/var/log/cvmfs-notify-runner

# Install syslog configuration file
if [ x"$(which systemctl)" != x"" ]; then
    echo "  - restarting rsyslog"
    sudo cp -v $SCRIPT_LOCATION/90-cvmfs-notify-rotate-systemd /etc/logrotate.d/
    sudo systemctl restart rsyslog

    echo "  - installing systemd service file"
    sudo cp -v $SCRIPT_LOCATION/cvmfs-notify.service /etc/systemd/system/cvmfs-notify.service
    sudo systemctl daemon-reload
else
    echo "  - restarting rsyslog"
    sudo cp -v $SCRIPT_LOCATION/90-cvmfs-notify-rotate /etc/logrotate.d/
    sudo service rsyslog restart
fi

# Symlink the configuration directory into /etc/cvmfs/notify
if [ ! -e /etc/cvmfs/notify ]; then
    echo "Creating onfiguration file directory to /etc/cvmfs/notify"
    sudo mkdir /etc/cvmfs/notify
fi
if [ ! -e /etc/cvmfs/notify/config.json ]; then
    echo "Copying config.json to /etc/cvmfs/notify"
    sudo cp -v $SCRIPT_LOCATION/../etc/config.json /etc/cvmfs/notify/
fi

