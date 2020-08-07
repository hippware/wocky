#!/usr/bin/env bash

release_ctl eval --mfa "Wocky.Tasks.Migrate.migrate/1" --argv -- "$@"
bin/wocky foreground
