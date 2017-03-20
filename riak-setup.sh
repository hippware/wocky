#!/bin/sh

RIAK_HOST="http://localhost:8098"

curl -XPUT $RIAK_HOST/search/schema/vcard \
    -H 'Content-Type:application/xml' \
    --data-binary @db/vcard_search_schema.xml

curl -XPUT $RIAK_HOST/search/index/vcard \
    -H 'Content-Type: application/json' \
    -d '{"schema":"vcard"}'

#MAM
curl -XPUT $RIAK_HOST/search/schema/mam \
    -H 'Content-Type:application/xml' \
    --data-binary @db/mam_search_schema.xml

curl -XPUT $RIAK_HOST/search/index/mam \
    -H 'Content-Type: application/json' \
    -d '{"schema":"mam"}'

# users
curl -XPUT $RIAK_HOST/search/index/users \
    -H 'Content-Type: application/json' \
    -d '{"schema":"_yz_default"}'

riak-admin bucket-type create users '{"props":{"datatype":"map", "search_index":"users"}}'
riak-admin bucket-type activate users

# tokens
riak-admin bucket-type create tokens '{"props":{"datatype":"map"}}'
riak-admin bucket-type activate tokens

# rosters
riak-admin bucket-type create rosters '{"props":{"datatype":"map"}}'
riak-admin bucket-type activate rosters
riak-admin bucket-type create roster_versions '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate roster_versions

# private storage
riak-admin bucket-type create private '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate private

# vCard
riak-admin bucket-type create vcard '{"props":{"last_write_wins":true, "search_index":"vcard", "dvv_enabled":false}}'
riak-admin bucket-type activate vcard

riak-admin bucket-type create mam_yz '{"props":{"datatype":"map", "search_index":"mam"}}'
riak-admin bucket-type activate mam_yz

# Last activity
riak-admin bucket-type create last '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate last

# Offline messages
riak-admin bucket-type create offline '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate offline

# Privacy/blocking lists
riak-admin bucket-type create privacy_defaults '{"props":{"last_write_wins":true, "dvv_enabled":false}}'
riak-admin bucket-type activate privacy_defaults

riak-admin bucket-type create privacy_lists_names '{"props":{"datatype":"set"}}'
riak-admin bucket-type activate privacy_lists_names

riak-admin bucket-type create privacy_lists '{"props":{"last_write_wins":true,"dvv_enabled":false}}'
riak-admin bucket-type activate privacy_lists
