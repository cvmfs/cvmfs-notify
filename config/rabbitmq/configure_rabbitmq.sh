#!/bin/sh

admin_user=$1
admin_pass=$2
worker_user=$3
worker_pass=$4

# Enable management and last-value-cache exchange plugins
rabbitmq-plugins enable rabbitmq_management

# Delete default guest user
if [ x"$(rabbitmqctl list_users | grep 'guest' | wc -l)" != x"0" ]; then
    rabbitmqctl delete_user guest
fi

# Add the CVMFS vhost if needed
if [ x"$(rabbitmqctl list_vhosts | grep /cvmfs | wc -l)" = x"0" ]; then
    rabbitmqctl add_vhost /cvmfs
fi

# Add and configure the administrator user
if [ x"$(rabbitmqctl list_users | grep '^${admin_user}' | wc -l)" = x"0" ]; then
    rabbitmqctl add_user ${admin_user} ${admin_pass}
    rabbitmqctl set_permissions -p /cvmfs ${admin_user} ".*" ".*" ".*"
    rabbitmqctl set_user_tags ${admin_user} administrator
fi

# Add and configure the publisher user
if [ x"$(rabbitmqctl list_users | grep '^${worker_user}' | wc -l)" = x"0" ]; then
    rabbitmqctl add_user ${worker_user} ${worker_pass}
    rabbitmqctl set_permissions -p /cvmfs ${worker_user} "^(amq\.gen.*|repository\.activity)$" "^(amq\.gen.*|repository\.activity)$" ".*"
fi

