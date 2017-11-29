# Change Log

Also: [Deployment history](https://github.com/hippware/tr-wiki/wiki/Server-deployment-history)

Ticket numbers refer to the ticket tracker for this project if not specified. 


# 2017.11.29+r8fea483

* Add `new` attribute for new HS item async updates (#1114)
* Block handles containing reserved words (#1113)
* Various fixes to ensure tests run cleanly (#1116)
  * Allow apostrophe in user names.
* Remove user FK constraint on traffic_logs (#1117)
* Add roles to roster response (#1118)
* Quick fix for bot integration tests (PR #1121)
* Rework (v2) / Revert (HS) Publishing protocol (#1123)
  * Reverts #1043
* Push notifications rework and more diagnostics (#1087)
* Fix regression in xmpp_reg_SUITE (PR #1126)
* Convert wocky_api to Phoenix (PR #1127)


# 2017.11.17+r68d76fd

* Implement new user validation rules (server) (#1086)
* Fix lat/lon swap in GeoUtils.point (#1110)
* Adjust prepopulation logic to four weeks, min 10 items (#1081)


# 2017.11.16+r0447d36

* **Replace Mnesia as the MIM session store with Redis (#1107)**
* Update the staging SSL certificate (PR #1075)
* Set timeout of Algolia geosearch call to 30s (PR #1077)
* Rework (HS) Publishing protocol (#1043)
* Update tzdata version (PR #1093)
* HS items to include bot object if relevant (#962)
* Avatars being wiped when other, non-avatar user changes are made (#1084)
* Add befriend/2 and follow/2 operations to Wocky.RosterItem (PR #1096)
* If a bot changes, all referencing HS items should be updated (#1026)
* Change timestamp columns to use timestamptz (PR #1100)


# 2017.11.3+r5c0a757

* Overhaul logging (PR #1061)
* Fix DB maintenance tasks (PR #1066)
* Miscellaneous fixes
* Fix: HS prepopulation is not working on Staging (#1062)
* Fix some unit tests (PR 1068, PR #1070)
* Clarify bot subscription system (#896)
* Update updated_at when home stream items are marked as deleted (PR #1074)
  * Fixes: Long delays in async HS delete notifications upon deleting a bot (#1063)


# 2017.10.24+ra8aefef

* Fix bot item indices (PR #1036)
* Fix roster integration tests
* HS prepopulation user is appearing in list of followers (#1028)
* Filter nil-handled users from other-user roster queries
  * Fixes: Paged retrieval of user's contacts gives dubious results (#1041)
* Update to Erlang 20.1, Elixir 1.5 (PR #1049)
* Add owner field to (old) geosearch api (#1023)
* Enable bot reports (PR #1050)
* Push notifications improvement and more logging (#1044)


# 2017.10.17+r4890609

* Update Mandrill API key (PR #1014)
* Add K8s templates for a load test environment (PR #1015)
* Re-implement bot reports (#1013)
  * And associated cron job infrastructure
* Restore Slack deployment notifications (#1017)
* Database maintenance tasks (#788)
* Changes to support AMOC load testing (PR #1029)


# 2017.9.22+r3e3ec07

* Logging changes (PR #980)
* Send a welcome email to new users (#952)
* Allow filtering of deleted HS items (#987)
* k8s updates (PR #990)
* Fix handling of nil user handle in user contacts (#985)


# 2017.9.20+r535d2cb

* Home stream prepopulation (PR #978)


# 2017.9.18+rabf481e

* Blocking fixes (PR #971, PR #972)
  * Bots searched for by owner were not being correctly filtered.
  * Conversations between blocked users were not cleaned up.
  * Re-enable roster read access for blocked users
  * Include __blocked_by__ roster group


# 2017.9.15+r8f753bb

* Do not publish item revocation events (#923)
* When bots are deleted or made private, referencing HS items should be deleted (#919)
* Fix a crash on Firebase auth failure (PR #968)


# 2017.9.15+r4c68618f

* Support for Kubernetes deployment (PR #908)
* Don't validate binary UUIDs (PR #942)
* More Kubernetes configuration (PR #941)
  - Closes ports 80 and 5222.
* Blocking
  * Blocking: Roster and presence (#924)
  * Blocking: User Profile (#925)
  * Blocking: Third party follower/followee lists (#926)
  * Blocking: HS (#927)
  * Blocking: Bots (#928)
  * Blocking: Bot content (#929)
* Add updated APNS push certificates (#945)
* New bot field 'address_data' to store location/address metadata as json (#915)


# 1.0.2630-r012b3c0 - 2017-09-06

* Fix Firebase ID (PR #922)


# 1.0.2627-r611e801 - 2017-09-05

* Fix crash on certain invalid TROS IDs (#900)
* Firebase baseline integration (#877)
* Fix missing fields in roster updates and add tests (#876)
* Fix dialyzer error in roster (PR #907)
* Broaden initial contacts to allow followers, followees and friends (#880)
* Add configurable S3 region (PR #911)
* Update dependencies and fix a few small issues (PR #914)
* Fix the Lager config (PR #920)


# 1.0.2582-62e3cb6 - 2017-08-28

* New bot field: Number of content items (#867)
* Bot content items should include author data (#892)
* Allow all bot viewers to publish items (#887)
* Add more author fields to bot item (#898)


# 1.0.2566-287968c - 2017-08-21

* Algolia reindexing fixes (#883, #886).


# 1.0.2557-10851ce - 2017-08-21

* Build environment changes (for Codeship and other)
* Location indexing in algolia broken after location changes (#878)


# 2017.8.17+2529.8eecea6

* Fix a migration script (#871)


# 2017.8.17+2527.951e779

* Fix crash on insertion of invalid roster item. Fixes #856.
* Don't generate a HS item if bot description becomes empty (#863)
* Fix a crash relating to pending bots. Fixes #859.
* Only delete user db entry after all other delete operations are done. Fixes #851.
* Use PostGIS to manage bot location data.
  * Experimental, incomplete port of 'explore nearby' to use PostGIS. (#857, #864)
* Bot lists to be sortable by distance (#763)
* Fixed some tests (#869, #870)


# 2017.7.18+2476.1c4ac8a

* Initialise new accounts with a set of followees (#841)
* Fix: Can't get user: No access to field (#842)
* Makes user roles field public (#843)


# 2017.7.13+2471.782e996

* Add an operation to retrieve any user's followers and followees (#829)
* Remove limit on message size for notification log (#834)
* Write integration tests for SNS, S3 and Algolia (#207)
* Fix auth disconnect crash (#835)
* Bot lists to be sortable by simple criteria (#757)


# 2017.7.4+2445.cd82a75

* Fix the primary key on the user_locations table (#826)
* Create a database log of push notifications (#793)


# 2017.6.30+2439.e6f5eda

* Update bot modification time when items are published to them. (#825)
* Fix: Retrieving bots by user id always returns subscribed bots (#822)


# 2017.6.29+2434.e9f14f3

* Make user handles unique regardless of case (#794)
* Work around DB race condition in register operation (#796)
* Move bot query RSM filtering to the DB (#785)
* Add permissions check for creation of bot with preallocated ID (#805)
* Fix us1 APNS cert filename (#811)
* Fix compiler warnings (#808)
* Re-enable location upload API, revisited (#814)


# 2017.6.21+2413.14e2b67

* Bot owners should receive bot creation notification within hs (#744)
* Fix a function clause crash in PushEventHandler (#791)
* More push notification logging (#797)


# 2017.6.20+2404.07f57c0

* Fix a crash on testing when calling geosearch (#764)
* Reduce the logging threshold for user IQ errors (#769)
* Change remaining time formats to Z-suffix form (#723)
* Add roster timestamps. Addresses: Expire followers from being new (#606)
* Optimise roster item queries (#751)
* Make all indexer operations synchronous with respect to each other (#771)
* Re-enable location upload API (#766)
* Hash authentication tokens (#483)
* Add user roles to distinguish privileged accounts (#748)
* Replace SNS with direct calls to APNS (#767)
* Push notifications for image-only messages (#743)
* Maintain temporary subscriptions across stream resumption (#776)
* Use OS environment variables to configure 3rd party libraries (#780)
* Add geosearch implementation for testing (#782)
* Refactor RSM queries into common code (#429)
* Filter pending bots from report (#786)


# 2017.6.9+2319.ac1c9e4

* Fix TROS download error for thumbnails


# 2017.6.9+2316.7038371

* Fix a crash in RosterItem.maybe_sort_pair (#759)
* Request to download image file should wait until post-processing is complete (#643)


# 2017.6.8+2311.b907e49

* Remove null values before migration #756


# 2017.6.7+2303.8e6072a

* Add user fields for number of bots, followers, and followees (#686)
* Geo-search API not returning all expected bots (#641)
* Delete SNS endpoint when client de-registers for push notifications. 
  * Add logging.
* Fix up string handling in bot report generation (#740)
* Fix: ETS errors on shutdown (#734)
* Changes in bot description to be published to Home Stream (#646)
* Add new user field: 'tagline' (#689)


# 2017.6.2+2263.74bd7a4

* Bot aren't indexed correctly on Algolia (#729)


# 2017.6.1+2251.80891f8

* Conversation id fix (#730)
* Various migration fixes. 


# 2017.5.31+2242.dd22001

* Rework: Inconsistent ordering of bots (#711)


# 2017.5.30+2237.d83ae56

* Fix: Conversations being stored with non-bare JID (#720)
* Fix: Weekly reports not being generated on staging after migration (#713)
* Fix: Inconsistent ordering of bots (#711)


# 2017.5.29+2225.a8ec3ff

* Fix digits bypass


# 2017.5.26+2222.84833a7

* Lots of small fixes.


# 2017.5.24+2186.ae67e94

* Conversations query tweaks (#650)
* Enable special bypass phone number prefix on Staging (#668)


# 2017.5.23+2177.2daf71f

**DATABASE MIGRATION** from Cassandra to postgresql-compatible. 

* Lots of refactoring and rewrites. 


# 17.04.26+1768.151aff7

* Push notifications restructure
  * Indirectly resolves: Generalise push notification generation (#592)
* Fix handling of empty bot shortname field (#629)
* Better input handling for GeoUtils and multicast stanzas. 
  * Fixes: ArgumentError: argument error (#635)


# 17.04.12+1736.ec69c83

* New API: User Bulk Query (#614)
* Upgrade: escalus, exml (2.4.0 to 2.4.1)
* Add 'subscribed' field to bot (#620)


# 17.04.05+1724.8a6709a

* Consolidate bot visibility (only Public and Private) and sharing (#442)
* Remove HTTP REST support. No longer needed. 
  * Close port 80
* More cron related fixes.


# 17.03.31+1707.7751e35

* Fix a badarith error during bot search
  * Fixes: ArithmeticError: bad argument in arithmetic expression (#599)
  * Fixes: Staging: Can't Sign In (rn-chat #507)


# 17.03.31+1704.491f1d9

* Add reindex CLI command
* Normalise latitude, longitude values before passing to Algolia. (#589)
* Fix up weekly bot reports
  * Upgrade: slackex (Change file upload to use POST)
  * Upgrade: crone
  * Fixes: mod_wocky_cron startup error prevents ejabberd from starting (#555)
* Bot share notifications to generate push notifications (#590)


# 17.03.21+1676.6dfc479

* Allow publishing to preallocated bot ID (#564)
* Fix crash when digits gives us an empty number (#563)
* Remove 100 row limit on standard selects.
  * Fixes: Staging: Bot disappears after kill/reload (#567)
* Add error handling for unconfigured Algolia indices (#570)
* Add owner field to 'geosearch for bots' API (#573)


# 17.03.08+1639.1ad8af1

* Update exometer_cloudwatch to get a crucial bugfix (#553)
* Disable mod_wocky_cron until the bugs can be fixed (#555)
* Fix SNS result handler (#559, #561)
* Minor fixes for server-side image processing (playbooks#66, playbooks#67)


# 17.03.08+1630.d3c1e18

* Images are processed and sanitised after upload (#452)
  * Images are downsized to Nx1920 (landscape) or 1920xN (portrait) pixels. 
* Image thumbnail generation (#517)
* File metadata moved from S3 to database (#495)
* File upload content length header is now enforced (#528)
* Add `reprocess_images` CLI operation to (re)generate thumbnails and sanitised
  images.
* Add missing cron module. 
  * Was preventing weekly bot report from running. 
* Fix spelling in bot CLI command.
* Update component `tzdata` from 0.5.10 to 0.5.11 to fix a crash (#547). 


# 17.02.28+1578.5066239

* Fixed/Rework: Startup can fail due to mnesia transform error (#488)
* Changed: Remove owner from subscribers list (#524)
* Add owner column to bot report CLI command. Minor fixes. (Rework #490)
* Add lexicographic ordering on id as tie-breaker for bot item ordering (#531)


# 17.02.22+1560.6fce18b

* Fixed typo in configuration.


# 17.02.22+1558.29f3688

* Added: bot CLI command (#490)
  * Introduced a typo in configuration.


# 17.02.14+1549.1b6c728

* Changed: Update cqerl
* Re-enable dialyzer on Jenkins build


# 17.02.14+1545.001dfdc

* Not fixed: Startup can fail due to mnesia transform error (#488)
* Fixed: Add day and week durations, milisecond resolution to traffic dumps (#498)
* Fixed: Fix a typo when getting the current search index name (#512)
  * Fixes: MatchError: no match of right hand side value (#510)


# 17.02.08+1539.943a153

* Remove chat messages from Home Stream (#434)
* Add new-id function for bot creation (#472)
  * Replaces: Allow null title (and other required data) for bots (#414)
* Add tags to bots (#440)
* Add CLI to generate a new authentication token for a user (#478)
* Add commit SHA hash to version. 
  * Due to: Add git tag for each deployed release (#463)
* Support SSL for HTTP APIs (playbooks#19)


# 17.02.02+1521

(No data)
