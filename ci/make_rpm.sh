#!/bin/sh

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
echo "Script location: $SCRIPT_LOCATION"

# Create togo project
echo "Creating togo project"
cd $BUILD_LOCATION
mkdir -p togo
cd togo
togo project create cvmfs-notify

echo "Creating unpacking package tarball into togo project"
mkdir -p cvmfs-notify/root/usr/libexec/cvmfs-notify
cd cvmfs-notify/root/usr/libexec/cvmfs-notify
tar xf $BUILD_LOCATION/$TARBALL
cd $BUILD_LOCATION/togo/cvmfs-notify
togo file exclude root/usr/libexec

# Place and flag config files in the togo workspace

# systemlog configuration
mkdir -p root/etc/logrotate.d
togo file exclude root/etc/logrotate.d

mkdir -p root/etc/systemd/system
togo file exclude root/etc/systemd/system
cp -v root/usr/libexec/cvmfs-notify/scripts/cvmfs-notify.service \
        root/etc/systemd/system/
togo file flag config-nr root/etc/systemd/system/cvmfs-notify.service

cp -v root/usr/libexec/cvmfs-notify/scripts/90-cvmfs-notify-rotate-systemd \
    root/etc/logrotate.d
togo file flag config-nr root/etc/logrotate.d/90-cvmfs-notify-rotate-systemd

# cvmfs-notify configuration files
mkdir -p root/etc/cvmfs/notify
togo file exclude root/etc
togo file exclude root/etc/cvmfs
cp -v root/usr/libexec/cvmfs-notify/etc/config.json root/etc/cvmfs/notify/
togo file flag config-nr root/etc/cvmfs/notify/config.json

# Copy spec file fragments into place
echo "Copying RPM spec file fragments"
cp -v $SCRIPT_LOCATION/spec/* ./spec/

# Replace template values in spec file header
echo "Configuring RPM spec file header"
sed -i -e "s/<<CVMFS_NOTIFY_VERSION>>/$VERSION/g" $BUILD_LOCATION/togo/cvmfs-notify/spec/header
sed -i -e "s/<<CVMFS_NOTIFY_RELEASE>>/$RELEASE/g" $BUILD_LOCATION/togo/cvmfs-notify/spec/header

# Build package
echo "Building RPM package"
togo build package

# Copy RPM and SRPM into place
echo "Copying RPMs to output location"
mkdir -p $BUILD_LOCATION/RPMS
cp -v ./rpms/*.rpm $BUILD_LOCATION/RPMS
cp -v ./rpms/src/*.rpm $BUILD_LOCATION/RPMS

# Create pkgmap
echo "Creating package map"
mkdir -p ${BUILD_LOCATION}/pkgmap
PKGMAP_FILE=${BUILD_LOCATION}/pkgmap/pkgmap.${PLATFORM}_x86_64
PACKAGE_NAME=cvmfs-notify-${VERSION}-${RELEASE}$(rpm --eval "%{?dist}").x86_64.rpm
echo "[${BUILD_PLATFORM}_x86_64]" >> ${PKGMAP_FILE}
echo "notify=${PACKAGE_NAME}" >> ${PKGMAP_FILE}

# Cleanup
cd $BUILD_LOCATION
rm -rf togo
