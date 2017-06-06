# Change Log

Also: [Deployment history](https://github.com/hippware/tr-wiki/wiki/Server-deployment-history)

Ticket numbers refer to the ticket tracker for this project if not specified. 

Suggested subheadings: `Added`, `Changed`, `Deprecated`, `Removed`, `Fixed`, `Security`

If there are not many items, just list them sequentially. 


* Geo-search API not returning all expected bots (#641)
* Delete SNS endpoint when client de-registers for push notifications. 
  * Add logging.


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
