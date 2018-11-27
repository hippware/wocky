#!/usr/bin/env bash

release_ctl eval 'Wocky.Tasks.Notify.start()'
release_ctl eval --mfa "Wocky.Tasks.Migrate.migrate/1" --argv -- "$@"
