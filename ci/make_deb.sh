#!/bin/bash

set -e

TARBALL=$1
BUILD_LOCATION=$2
PLATFORM=$3
VERSION=$4
RELEASE=$5

echo "Tarball: $TARBALL"
echo "Build location: $BUILD_LOCATION"
echo "Platform: $PLATFORM"
echo "Package version: $VERSION"
echo "Release: $RELEASE"

SCRIPT_LOCATION=$(cd "$(dirname "$0")"; pwd)

if [ x"$PLATFORM" = xubuntu1604 ]; then
    PACKAGE_NAME_SUFFIX="+ubuntu16.04_amd64"
elif [ x"$PLATFORM" = xubuntu1804 ]; then
    PACKAGE_NAME_SUFFIX="+ubuntu18.04_amd64"
fi
PACKAGE_NAME=cvmfs-notify_$VERSION~$RELEASE$PACKAGE_NAME_SUFFIX.deb

mkdir -p ${BUILD_LOCATION}/DEBS

if [ -e /etc/profile.d/rvm.sh ]; then
    . /etc/profile.d/rvm.sh
fi

WORKSPACE=${BUILD_LOCATION}/pkg_ws
mkdir -p $WORKSPACE

mkdir -p $WORKSPACE/usr/libexec/cvmfs-notify
tar xzf ${BUILD_LOCATION}/$TARBALL -C $WORKSPACE/usr/libexec/cvmfs-notify

mkdir -p $WORKSPACE/etc/systemd/system
cp -v ${BUILD_LOCATION}/scripts/cvmfs-notify.service \
    $WORKSPACE/etc/systemd/system/

pushd $WORKSPACE
fpm -s dir -t deb \
    --package ../DEBS/$PACKAGE_NAME \
    --version $VERSION \
    --name cvmfs-notify \
    --maintainer "Radu Popescu <radu.popescu@cern.ch>" \
    --description "CernVM-FS Notification Server" \
    --url "http://cernvm.cern.ch" \
    --license "BSD-3-Clause" \
    --depends "cvmfs-server > 2.5.1" \
    --directories usr/libexec/cvmfs-notify \
    --config-files etc/systemd/system/cvmfs-notify.service \
    --exclude etc/systemd/system \
    --no-deb-systemd-restart-after-upgrade \
    --chdir $WORKSPACE \
    ./
popd

mkdir -p ${BUILD_LOCATION}/pkgmap
PKGMAP_FILE=${BUILD_LOCATION}/pkgmap/pkgmap.${PLATFORM}_x86_64
echo "[${PLATFORM}_x86_64]" >> $PKGMAP_FILE
echo "notify=$PACKAGE_NAME" >> $PKGMAP_FILE

rm -rf $WORKSPACE
