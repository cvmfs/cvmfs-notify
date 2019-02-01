#!/bin/sh

set -e

export RUNNER_LOG_DIR=/var/log/cvmfs-notify-runner

wait_for_app_start() {
    local reply=$($SCRIPT_LOCATION/../bin/cvmfs_notify ping | awk {'print $1'})
    local num_iter=1
    while [ $reply != "pong" ]; do
        sleep 1
        reply=$(/usr/libexec/cvmfs-notify/bin/cvmfs_notify ping | awk {'print $1'})
        num_iter=$((num_iter + 1))
        if [ $num_iter -eq 10 ]; then
            echo "Error: Could not start cvmfs-notify"
            exit 1
        fi
    done
    echo $reply
}

SCRIPT_LOCATION=$(cd "$(dirname "$0")"; pwd)

action=$1

if [ x"$action" = xstart ]; then
    $SCRIPT_LOCATION/../bin/cvmfs_notify start
    wait_for_app_start
    echo "CVMFS repository notify started."
elif [ x"$action" = xstop ]; then
    $SCRIPT_LOCATION/../bin/cvmfs_notify stop
    pkill epmd
    echo "CVMFS repository notify stopped."
elif [ x"$action" = xreload ]; then
    $SCRIPT_LOCATION/../bin/cvmfs_notify escript scripts/reload_repo_config.escript
    echo "CVMFS repository notify configuration reloaded."
elif [ x"$action" = xrestart ]; then
    $SCRIPT_LOCATION/../bin/cvmfs_notify stop
    pkill epmd
    $SCRIPT_LOCATION/../bin/cvmfs_notify start
    wait_for_app_start
    echo "CVMFS repository notify restarted."
elif [ x"$action" = xstatus ]; then
    $SCRIPT_LOCATION/../bin/cvmfs_notify ping
else
    echo "Unknown action: $action"
    echo "Usage: run_cvmfs_notify.sh <start|stop|reload|restart|status>"
    exit 1
fi
