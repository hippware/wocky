#!/bin/bash
WOCKYDIR="/opt/wocky/"

if [ -n "$HOSTNAME" ]; then
    VMARGS=/opt/wocky/releases/0.0.0+build.301.refc28f19b/vm.args
    echo "-kernel inet_dist_listen_min 9100 inet_dist_listen_max 9100" >> $VMARGS
    SEDARG="-i 's/sname wocky@localhost/sname wocky@$HOSTNAME/g' $VMARGS"
    eval sed "$SEDARG"
fi

if [ -n "$CLUSTER_WITH"  ]; then
   # checking this to be able to gently handle updates, when we want to preserve content
   if [ -d "/data/mnesia/Mnesia.wocky@$HOSTNAME" ]; then
       ## verify if we are in cluster ?
       echo "the node is probably part of a cluster"
   else
       $WOCKYDIR/bin/wockyctl add_to_cluster wocky@$CLUSTER_WITH
       mv -f "$WOCKYDIR/Mnesia.wocky@$HOSTNAME" "/data/mnesia/Mnesia.wocky@$HOSTNAME"
   fi
fi

if [ "$#" -ne 1 ]; then
   $WOCKYDIR/bin/wocky live --noshell -noinput +Bd  -mnesia dir \"/data/mnesia/Mnesia.wocky@$HOSTNAME\"
else
   $WOCKYDIR/bin/wockyctl $1
fi
