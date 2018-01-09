#!/bin/sh

###-------------------------------------------------------------------
### This file is part of the CernVM File System.
###-------------------------------------------------------------------

rebar3 auto \
    --config config/sys.config.dev \
    --sname cvmfs_notify \
    --setcookie cvmfs \
    --apps cvmfs_notify,runtime_tools,sasl

