#!/bin/sh

#-------------------------------------------------------------------
#
# This file is part of the CernVM File System.
#
#-------------------------------------------------------------------

set -e

#SCRIPT_LOCATION=$(cd "$(dirname "$0")"; pwd)

DB_DIR=/var/lib/cvmfs-notify

echo "  - creating persistant storage directory at $DB_DIR"
sudo mkdir -p $DB_DIR
sudo chown -R `whoami` $DB_DIR
