#!/bin/sh

bin/wocky command Elixir.Wocky.Tasks.Predeploy run_migrate_only
bin/wocky foreground
