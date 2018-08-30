#!/usr/bin/env bash

release_ctl eval 'Wocky.Tasks.Migrate.run()'
bin/wocky foreground
