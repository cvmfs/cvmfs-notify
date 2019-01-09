#!/bin/sh

admin_user=$1
admin_pass=$2
publisher_user=$3
publisher_pass=$4
subscriber_user=$5
subscriber_pass=$6


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
if [ x"$(rabbitmqctl list_users | grep '^${publisher_user}' | wc -l)" = x"0" ]; then
    rabbitmqctl add_user ${publisher_user} ${publisher_pass}
    rabbitmqctl set_permissions -p /cvmfs ${publisher_user} "^(amq\.gen.*)$" "^(amq\.gen.*|repository\_activity)$" ".*"
fi

# Add and configure the subscriber user
if [ x"$(rabbitmqctl list_users | grep '^${subscriber_user}' | wc -l)" = x"0" ]; then
    rabbitmqctl add_user ${subscriber_user} ${subscriber_pass}
    rabbitmqctl set_permissions -p /cvmfs ${subscriber_user} "^(amq\.gen.*)$" "^(amq\.gen.*)$" ".*"
fi

