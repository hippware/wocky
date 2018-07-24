# Change Log

Also: [Deployment history](https://github.com/hippware/tr-wiki/wiki/Server-deployment-history)

Ticket numbers refer to the ticket tracker for this project if not specified. 


* Update: absinthe (PR #1693), espec (PR #1702)
* Extend auth token expiry to 60 days (#1680)
* Fix crash on nil description (#1691, PR #1701)
* Mark server fields as deprecated (PR #1700)
* Bot query tweaks (PR #1696)


# 2018.7.19+r0da9d8d

Fix update of pending bots; add more tests (PR #1690)


# 2018.7.19+r273a9c7

(Incorporates 2018.7.19+r87c6f60)

* Add test for botGuestVisitors (PR #1658)
* Update (via Dependabot): ex_guard (PR #1660)
* Move APNS call to async/on_response mode (PR #1663)
  * [Wocky/us1] RuntimeError: The SSL connection timed out (#1337)
* Fix staging services deploy file (PR #1664)
* Update (via Dependabot): bcrypt_elixir (PR #1667), lager (PR #1668)
* Metrics / Application metrics instrumentation (#1000)
  * Rework (PR #1665), Add some metrics (PR #1670)
* Update absinthe_phoenix (PR #1683)
* Fix dependencies (PR #1684)
* Add mutation support for bot preallocation (PR #1686)
  * Fixes: Cannot create 'empty' bot via GraphQL (#1682)
* Fix elixometer config (#1687, #1688, PR #1689)


# 2018.7.9+r7386f6f

* Update (via Dependabot): pigeon (PR #1656)
* Fix: hideUser mutation doesn't change profile hidden value (#1654)
* Fix debounce override (PR #1657)


# 2018.7.5+rfff6dfc

Important: Update to Erlang 21/Elixir 1.6.6 (PR #1630)

* Add createdAt to Bot fields returned via GraphQL (#1617)
* Speculative attempt to fix bot/sub locking (PR #1622)
* Update (via Dependabot): 
  * ex_aws_s3 (PR #1621), wocky_db_watcher (PR #1633, PR #1638), 
  * honeybadger (PR #1635, PR #1648), recon (PR #1644), peerage (PR #1653)
* Update the ECR credentials used by Codeship (PR #1624)
* Config cleanup (PR #1625)
* Various tweaks to improve test stability and error output (PR #1626)
* Add isFetch field to location updates (PR #1627)
* Update to Erlang 21/Elixir 1.6.6 (PR #1630)
* Consolidate AWS user credentials in Wocky (#1631)
* Update distillery for Erlang 21 (PR #1636)
* Remove auto-gen sub count; remove bot subscribers hash (#1628)
* Hook up wocky metrics to Tectonic's Prometheus (#1487)
* Capture all fields from location updates (#1634)
  * ... and some refactoring and fixes (PR #1649)
* Code cleanup (PR #1650)
* Implement 'discover mode' list of bots (#1651)


# 2018.6.19+r7c97339

(Includes 2018.6.19+ree2a071 and 2018.6.19+r9435c83)

* Add user location to bot creation, update and subscription with debounce skipping (#1576, PR #1583)
  * Includes: v2, Geofence entry notification, owner inside at t=0 (#1461)
* Update (via Dependabot): credo (PR #1584), lager (PR #1589), 
  * idna (PR #1592), excoveralls (PR #1592, PR #1607), guardian (PR #1593)
  * bamboo (PR #1600), distillery (PR #1608	)
* Update absinthe version (PR #1585)
* Clean up/remove `server` field from data (#1573, #1613)
* New icon field for bots (#1590, #1611)
* Disable weekly bot reports (#1599)
* Remove imagePullSecrets from K8s manifests (PR #1602)
* New user field: 'geo-invisible' / `hide` (#1596)
* Add `localBots` query (#1609)


# 2018.5.30+r136aa78

* Updated (via Dependabot): honeybadger (PR #1580)
* Fix media delete mutation to use URL parameter instead of ID (PR #1582, #1570)


# 2018.5.29+r0ba814c

* Enable dead link cleaning (PR #1575), Clean invalid tros ids (#1251)
* Add GraphQL operation to allow user to delete uploaded image (#1570, PR #1574)
* Updated (via Dependabot): pigeon (PR #1578)
* Move hasUsedGeofence field to currentUser object (#1577)


# 2018.5.24+r48e4c8d

* Remove mod_wocky_cli (PR #1526)
* Remove old slack token (PR #1529)
* Some more fixes for desktop mode (PR #1528)
* Add Rexbug and recon for runtime debugging (PR #1531)
* Add a SUBSCRIBED_NOT_OWNED relationship for bots (PR #1530)
  * Create new bot relationship to get all subscribed but not owned bots (#1522)
* Fix crash when non-string field is passed to authentication (#1515, PR #1535)
* Fix crash on unblock race (#1527, PR #1536)
* Fix test failures, intermittent CT failure (#1534, PR #1537)
* Updated (via Dependabot): 
  * prometheus_ecto (PR #1539), fast_tls (PR #1540), lager (PR #1541)
  * excoveralls (PR #1542), exml (PR #1548), ranch (PR #1550)
  * bcrypt_elixir (PR #1554), absinthe_phoenix (PR #1559)
* Update other dependencies (PR #1544, PR #1557, PR #1562)
* Return empty S3 URLs until images are marked as ready. (#1538)
* Home stream GraphQL subscription (PR #1546)
* Add bot delete mutation (PR #1558)
* Add media upload GraphQL query (PR #1556)
* Ensure correct fields are/are not set during subscription updates (PR #1555)
  * Newly created private geofence bot is not part of activeBots query (#1553)
* Add geofence `hasUsedGeofence` intro query (PR #1567)
* Add GraphQL for bot item management (PR #1569)


# 2018.5.15+ra3dbb55

* Update absinthe_relay for invalid cursor fix (PR #1514)
  * "Internal server error" on bot subscribers (#1512)
* Add dockerlint to Codeship; Make target for kubeval (PR #1516)
* New distillery version (PR #1523)
* Fix image uploads for pending bots (PR #1525)


# 2018.5.11+r280f940

* Fix crash on DB callback (#1486, PR #1491)
* Make graphql complexity configurable (PR #1495)
* Validate access rule in TROS metadata (#1492)
* User update mutation should return a useful error message (#1498)
* Reduce N+1 queries for one-to-one relationships (PR #1500)
* Update MIM to fix: badkey error when running ejabberd hooks (#1330)
* Blocking rework (#1501)
* Add Apollo tracing metadata to GraphQL (#1447)
* Create Docker setup for Front End devs to run Wocky locally (#1336)
  * Ensure wocky_db_watcher is started in desktop mode (PR #1510)
* Add sorting criteria to the other visitors query (PR #1509)
* Do visibility checking in the business logic (PR #1505)
  * Part of: Bake permissions and blocking into standard wocky interface (#1391)


# 2018.5.4+r4bdd0ed

* Expose user location events in the GraphQL API (#1480)
  * Also some minor refactoring and cleanup
* Increase graphql complexity limit to 2000 (PR #1493, PR #1494)


# 2018.5.3+reaf3ea8

* Implement query cost restrictions (#1427)
* GraphQL interface for `User` (PR #1484)


# 2018.5.2+r6abd519

* Disable geofence timeout (PR #1473, disables #1466)
* Update Dawdle; reinstate 30 minutes presence timeout (PR #1475)
* Add GraphQL schema documentation (#1404)
* Add logging for GraphQL traffic (PR #1476)
* Add active bots connection to current user (#1474)
* Review and refactor GraphQL middleware/security implementation (#1405)


# 2018.4.27+reb3df56

* Update some dependencies. Other housekeeping and fixes (PR #1460)
* Undisable the location retrieval API in prod (PR #1463)
* Fix ref bot deletion race crash (PR #1465)
  * [Wocky/staging] RuntimeError: DB Watcher callback crash (#1464)
* Extend geofence timeout to 30mins and disable PN on timeout (#1466)
* Update absinthe to fix subscription distribution error (PR #1469)
* Fix ref bot crash fix (PR #1470)
* Temporarily change timeout back to 900 seconds due to Dawdle limitation (PR #1472, #1471)


# 2018.4.23+r771bcf1

* Add types to Dawdle config values (PR #1458)


# 2018.4.23+r3a1a692

* Fix handling for `last` GraphQL parameter (PR #1457)
  * 'You must supply a count (total number of records)' error on GraphQL queries (#1456)


# 2018.4.23+r3993d0a

* Finish adding GraphQL tests for existing functionality (#1403)
* Exit all geofences upon 5 minute timeout (#1430)
  * Disabled by: Add a flag to disable the bot visit timeout (PR #1455)
* Retrieval of location data points (#1429)
* Update absinthe_relay and some other deps (PR #1449)
  * [Wocky/testing] ArithmeticError: bad argument in arithmetic expression (#1386)
* Fix crash on unauthenticated subscription (PR #1450)
  * [Wocky/staging] FunctionClauseError: no function clause matches (#1440)
* Add missing function clause (PR #1451)
  * [Wocky/staging] RuntimeError: DB Watcher callback crash (#1431)
* Relax some test timeouts (PR #1453)


# 2018.4.18+r06cb8d3

* Collections: Sharing and notifications (#1414)
* Public image fields (PR #1437)


# 2018.4.16+rcb85439

* Add Image/Avatar direct download URLs to GraphQL (#1417)
* Make bot items publicly viewable (#1420)
* Make GraphQL schema introspection available to unauthenticated connections (#1419)
* Collections: Baseline (#1413)
  * Except reordering of bots


# 2018.4.12+r0b53d08

* Update GraphQL to JWT authentication system (#1406)
* Rework bot visitor subscription (PR #1425)


# 2018.4.11+re9f3db0

* Rename GraphQL user search query to just `users` (#1408)
* Rework GraphQL mutations to use standard macros and naming (#1407)
* Add CORS support (for graphql) (PR #1418)


# 2018.4.10+r59b7f97

* GraphQL (PR #1400, PR #1401)
  * User search, conversations, home stream items
  * Other various changes
* Add bot to user relationships (PR #1410)


# 2018.4.6+r494667c

* Add authenticate mutation for websocket connections (PR #1397)


# 2018.4.5+r377998f

* GraphQL fixes (PR #1394, PR #1395)


# 2018.4.4+r9d2357b

* Fix a graphql crash (#1384)
* Make enter/exit debounce configurable and set to 0 on testing (#1383, PR #1390)
* Other graphql changes (PR #1392).
  * Add setLocation and subscription mutations.


# 2018.3.29+r5cb3244

* RuntimeError: DB Watcher callback crash (#1378, #1380)


# 2018.3.29+r6558ce9

* Append /visitors to bot perimeter event url (#1363)
* Notify HS if a referenced bot's geofence field changes (#1367)
* Geofence bot share should impart view access (#1368)
* Geofence related push notifications should not prefix bot with '@' (#1373)
* Implement full bots functionality in GraphQL (#1335)


# 2018.3.23+r1de2b1c

* Geofence subscription tweaks (PR #1357)
* Redo geofence share push notification to match standard share (PR #1360)
  * Verify that multicast addressing works for bot 'geofence' shares (#1358)


# 2018.3.21+r9592c13

* Geofence: geofence bot share (#1325)
* Geofence: request and response push notifications (#1326)
* When bot is geofence enabled, bot owner should be a guest (#1346)
* Geofence: mechanics and entry/exit push notifications (#1324)
* Move graphql endpoint to /graphql.
  * Unbreak location upload API (#1354)


# 2018.3.20+r483b095

* Geofence: Radius migration (#1317)
* Remove the code supporting "follow me" functionality (#1334)
* Speculative fix for missing home stream updates (PR #1340)
* Validate user IDs in GraphQL query (PR #1341)
* Test case in bot integration test suite is flapping (#1343)
* Geofence: Bot guest and visitor lists (#1347)


# 2018.3.15+rb6cb8d3

* Geofence: Baseline (#1312)
  * Adds geofence fields to bots


# 2018.3.13+r56c7ba1

* Formatting changes (PR #1315)
* Postgres based user search (#1235, PR #1314, PR #1323)
* Fix 8k limit on DB notifications for watcher system (#1327)
* New authentication protocol (#1262, PR #1316)


# 2018.3.6+rcff91d2

* Prevent re-authentication of a user who has been deleted (PR #1302)
* Handle text in the 'domain' part of SASL PLAIN authentication (PR #1303)
* Fix unique key issue with user disabling (PR #1306)


# 2018.3.6+rc3508e6

* Account refactoring (PR #1291, PR #1296)
* Add unfriend/2 function (PR #1294)
* Change image readiness system to use DB callbacks (#1230)
* Explicitly add inets application (PR #1301)
  * RuntimeError: IQ handler crash: hippware.com/hxep/user (#1300)


# 2018.2.27+r3ad256c

* Catch and report errors in watcher CB (PR #1288)
* Change format of conversation deep link (#1281)
* When a bot post is deleted, referencing HS items should also be deleted (#1227)


# 2018.2.23+r433c862

* Update to Elixir 1.6 (PR #1246).
* Code formatting changes (PR #1253)
* Build infrastructure changes (PR #1254)
* Better PN error logging (PR #1258)
* Update prod SSL cert (PR #1270)
* Rework DB modification callback system (PR #1267)
* Change bots to use DB callbacks (#1225, PR #1272)
  * When a bot is made private and unviewable, referencing HS items should be deleted (#1208)
  * Bot deletions not triggering live HS notifications for deleted items (#1266)
* Update MongooseIM to 2.1.1. (#1248)
* Dialyzer fixes (PR #1273)
* Add extra tests for HS notifications (PR #1274)
* Small fixes for dev mode (PR #1277)
* Prototype a GraphQL API within Wocky using Absinthe (#1278)
* Add deployment for wocky watcher (PR #1282)
  * Watcher deployment fixes (PR #1283, PR #1284, PR #1286)


# 2018.1.17+r1173119

* Fix a warning from Phoenix in the log files (PR #1239)
* Return an error when updating a nonexistent user (PR #1240)
* Return an error when the contacts RSM id isn't valid (PR #1241)
* Return an error for upload request for a missing user (PR #1242)


# 2018.1.15+r5e2d786

* Notify HS if a referenced bot's post count changes (#1182)
* Fix intermittent failure on traffic_log test (PR #1209)
* Fix: Positive online presence not transmitted when accounts become friends (#1165)
* Increase sandbox timeout to prevent errors during ct tests (PR #1215)
* Fix: Duplicate handle error is rendered with a 500 error code (#1199)
* Fix: Gracefully handle a missing user (#1138)
* Quick fixes for the weekly job (PR #1221)
* Handle invalid publishing versions gracefully (PR #1223)
* Fix crash on invalid contact_id (PR #1224)
* Implement DB modification callback system (#1082)
* Pregenerate S3 urls for image and avatar tags (#496)
* Updates to deployment infrastructure (PR #1232, PR #1233)
  * Bot report and db maintenance failing due to mismatched db schema (#1160)
  * Update deployment images to latest versions of Alpine, Erlang and Elixir


# 2017.12.19+r861dd8b

* Rewrite deployment functionality (#1191)


# 2017.12.18+r6eb2a57

* Change object fields to custom elements (#1132)
* Remove contents of bot title/description (notes) from bot report (#950)
* Reduce geosearch to simpler calculations (#1184)
* Fix invalid id crash (PR #1202)
* Fix: Auto following does not indicate followers as new (#1176)
* Clean up module startup order and system (PR #1204)
* (HS) Publishing: Flag 'too many updates' error (#1177)


# 2017.12.15+r2eb3b42

* Add HS catchup command (#1189)
* Use bare JIDs when searching for conversation IDs (PR #1192)
* Add a push notification for new followers (#1186)


# 2017.12.11+re507ecc

* Fix up stream management (session storage) (#1166)
* Fix updated_at on updated conversations (PR #1178)


# 2017.12.8+rff2d0b5

* Improve integration tests to catch crashes (#902)
* Add tests to clarify RSM behaviour in certain cases (#1153)
* A Private bot should unsubscribe users who can no longer view it (#1019)
* Don't colocate Wocky containers from the same env (PR #1163, #1158)
* Clean up old push notification artifacts (#1168)
* Add infrastructure for metrics collection (#1137)
* Add deep links to push notifications (#1099)


# 2017.12.1+r3ea2504

* Update mix_ct and enable fail_auto_skip (PR #1130)
* More efficiently calculate and track subscribers+hash bot field (#1038)
* Add RSM support to bot subscriber list retrieval (#1125)
* Always use sphereoid for distance sorting (PR #1134)
* A convenient way to update auto follow users and the system user together (#992)
* Retreiving list of subscribed bots should omit owned bots (#1131)
* Allow explore-nearby to take a rectangle instead of a radius (#1135)
* Add topic to push notifications (#1147, PR #1151)


# 2017.11.29+r8fea483

* Add `new` attribute for new HS item async updates (#1114)
  - Reverted by #1123 below.
* Block handles containing reserved words (#1113)
* Various fixes to ensure tests run cleanly (#1116)
  * Allow apostrophe in user names.
* Remove user FK constraint on traffic_logs (#1117)
* Add roles to roster response (#1118)
* Quick fix for bot integration tests (PR #1121)
* Rework (v2) / Revert (HS) Publishing protocol (#1123)
  * Reverts #1043, #1114
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
