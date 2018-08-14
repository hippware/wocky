#!/bin/sh

bin/wocky eval 'Wocky.Tasks.Predeploy.run_migrate_only()'
bin/wocky foreground
