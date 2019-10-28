# Change Log

Also: [Deployment history](https://github.com/hippware/tr-wiki/wiki/Server-deployment-history)

Ticket numbers refer to the ticket tracker for this project if not specified. 


* Update: credo_naming (PR #2887), mix_test_watch (PR #2888)


# 2019.10.22+rf55d9bd

* Update: excoveralls (PR #2882), geocalc (PR #2883)
  * observer_cli (PR #2886)


# 2019.10.17+r65711dd

* Socket cleanup (#2849, PR #2864)
  * Close all sockets belonging to a user once that user is deleted
  * Disallow re-authentication on an already authenticated connection
* Set badge count to unread messages (PR #2867)
* Import frequently used modules at iex startup (PR #2868)


# 2019.10.14+r400a75c

* Update: credo (PR #2856), pigeon (PR #2863)
* Implement dynamic location upload rate support (#2845)
* Handle negative or invalid accuracy gracefully (#2858, PR #2860)
* Enable Honeybadger breadcrumbs (PR #2861)
* Presence connection (PR #2862)


# 2019.10.4+ra3469c8

* Update: faker (PR #2847), httpoison (PR #2846), honeybadger (PR #2854)
* Fix selection of pid for authenticated websocket (PR #2848)


# 2019.10.1+r4c83ac3

* Update: hackney (PR #2843), httpoison (PR #2844)


# 2019.9.24+rf69ad87

* Update: postgrex (PR #2823), Erlang 22.1/Elixir 1.9.1 (PR #2832)
  * dialyxir (PR #2838)
* Counters for active connections and location uploads (PR #2817, PR #2828)
* 2821 loc share end self (PR #2824)
  * Reverted (PR #2829)
* Fix running ex_unit tests during `mix check` (PR #2830)
* Pull in SSL fix for Hackney on Erlang 22.1 (PR #2841)
* Update push notifications for iOS 13 (PR #2840)
* location_share_end discussion (#2821, PR #2831)
  * Implement location_share_end_self notification (disabled).
  * Allow location share end notifications to bypass user blocks.


# 2019.9.13+r03d6457

* Update: ex_json_logger (PR #2816), prometheus_process_collector (PR #2818)


# 2019.9.11+r8302726

* Data consistency funs (PR #2806)
* Move `us1` to SSL redis (PR #2809)
  * Move non-testing clusters to new redis server (#2171)
* Update: phoenix (PR #2810), rexbug (PR #2811), ex_check (PR #2813)
* Use a non-FIFO queue for Dawdle messages (PR #2807)
* User proximity fix (PR #2815)


# 2019.9.2+r8f3daf8

* Update: credo (PR #2805)


# 2019.8.27+r5ea6673

* Upgrade to DynamicSupervisor (#2797)
* Add backend support for user proximity (PR #2798)


# 2019.8.27+r7cedac4

* Update: credo (PR #2790), geo_postgis (PR #2795)
* Expose more RNBGL fields in the location audit (#2788)
* High priority for data-only push notification messages (#2793)
* Bump accuracy threshold to 65m (#2799, PR #2800)


# 2019.8.20+re723d19

* Update: apollo_tracing (PR #2779), pigeon (PR #2784)
  * ex_check (PR #2785), excoveralls (PR #2786)
* Add android sound params to FCM push notifications (#2778, PR #2780)
* Add bot and user indices to bot events table (PR #2782)
* Mark all old/existing chat messages as read (#2783, PR #2787)
* Remove location shares on block (PR #2789)


# 2019.8.15+r8edd193

* Add expiry to all redis records (#2774, PR #2775)
* Use APNS keys instead of certificates (#2095, PR #2776, PR #2777)


# 2019.8.13+rdf82a24

* Add 'mix check' (PR #2765)
* Move staging to SSL Redis server (PR #2766)
* Update: absinthe (PR #2767), ModuleConfig to version 1.0 (PR #2769)
* Cronjob fixes (PR #2771)


# 2019.8.8+r61efeaf

* Replace use of homoiconic enum with native PSQL enums (#2706, PR #2760)
* Add functions for (en|dis)ableing audit for a user (PR #2761)
* Remove unused migration utils module (PR #2762)
* Fix GraphQL error caused by user not having avatar set (PR #2764)
* Validate upload MIME type (PR #2763)


# 2019.8.7+r9e3d98b

* Checkpoint the database migrations (#2678)


# 2019.8.6+r7e54138

* Remove remaining ejson use from staging and us1 (PR #2753)
* Fix permissions for deploy container (PR #2757)
* Fix twilio default key value (PR #2758)


# 2019.8.6+r560b2f3

* Add line to actually log payload string (PR #2735)
* Vault related configuration (various PRs)
* Update: observer_cli (PR #2722), credo_naming (PR #2723, PR #2736)
  * bamboo (PR #2755), stringprep (PR #2754), DawdleDB (PR #2756)


# 2019.7.31+r4cb3a68

* Integrate the DB Watcher into Wocky (#2708, PR #2716)
* Tighten up argument type on location_request_trigger (PR #2718)
  * Ecto.Query.CastError: .../ecto/repo/queryable.ex:373: (#2717)
* Add stringified payload to push log (PR #2719)
* Confex vault adapter and configuration (PR #2720, PR #2721, PR #2724)
  * and other PRs


# 2019.7.29+r091e038

* Update: geo_postgis (PR #2702), plug (PR #2713), observer_cli (PR #2714)
* Create an endpoint to trigger silent push notification (#2692)
* Image files saved with thumbnail in original aspect ratio (#2701)
* Move to upstream ecto_enum release (PR #2707)
* Use direct dispatching feature of Dawdle (#2680, PR #2686, PR #2715)


# 2019.7.24+ra2df676

* Update phone number if it changes in Firebase (#2674, PR #2676)
* IAM Auth fixes (#2697, PR #2698)
* Update: credo (PR #2696), wocky_db_watcher (PR #2699)
* Replace credo_filename_consistency with credo_naming (PR #2700)
* Add separate IAM support for db dumper (PR #2694)


# 2019.7.22+r7529339

* Update: credo (PR #2677), geo (PR #2682), prometheus_phoenix (PR #2687)
* Add a checkpoint migration (#2678)
* Add '@impl' attributes to functions that implement callbacks (#2683)
* Use ModuleConfig (#2681)
* Use instance roles+kube2iam to manage permissions (PR #2689)
* More IAM role work (PR #2691)


# 2019.7.12+rbc1a11b

* LoadProfile of self contains presenceStatus:OFFLINE (#2670)


# 2019.7.8+rbb7609b

* Add vaultex (PR #2662, PR #2665)
* Update: phoenix (PR #2664)
* `userBulkLookup` returning incorrect null result (#2668)
  * Rework bulk user operations to fix corner cases (PR #2667)
* Presence subscription does not signal presence of self (#2666)


# 2019.7.3+r34a12a7

* Bump elixir version to 1.9 (PR #2630)
* Another try at fixing deployment notification race (PR #2631)
* Move back to upstream pigeon using callback-powered config (PR #2632)
* Update: ecto_sql (PR #2640), pigeon (PR #2639), plug_cowboy (PR #2641)
  * timex (PR #2638, PR #2646), absinthe (PR #2636), distillery (PR #2653)
* Remove unused (and non-functional) public scopes (PR #2635)
* Enable auditing for specific users (#2576)
* Improve test coverage and test organization (#2581)
* Fix naming of location logging flag (PR #2645)
  * User location data points not logged to loggly (#2644)
* Tweaks for test stability (PR #2643)
* Update terraform state file handling to 0.12 format (PR #2650)
* Update tzdata to fix crash (#2656, PR #2657)
* Better error handling during deploy (PR #2658)
  * Predeploy pod throwing errors (#2655)
* Fix dialyzer warnings (PR #2659)


# 2019.6.26+redac896

* SSL redis (PR #2625)
* Only use Running pods for post-deploy (PR #2626)
* Replace Kronky with AbsintheErrorPayload (PR #2618)
* Implement isRead field for messages and an API to mark as read (#2608)
* Update: distillery (PR #2627)
* Remove lager_logger (PR #2629)


# 2019.6.25+r9d7d6bb

* Remove '@server' component from push notification deeplinks (#2624)


# 2019.6.24+r58c0260

* Refactor bots and relations (#2607)
* Push notifications: User invite, acceptance, location share (#2614)


# 2019.6.21+r21d76f0

* Add clientData field to conversation view (PR #2615)


# 2019.6.21+r7a6804a

* Fix naming on messageSend mutation; deprecate old (PR #2613)


# 2019.6.21+r1ee3b2d

* Extra location audit fields (#2609, PR #2611)
* Allow storing client data for messages (Rework #2604, PR #2610)


# 2019.6.19+reb549c5

* Update: honeybadger (PR #2588), phoenix (PR #2589), credo (PR #2585)
  * gen_stage (PR #2595), ecto_sql (PR #2596), bimap (PR #2600)
* Add logging for audit fields (PR #2594)
* Add deployed tag to deployed image (PR #2597)
* Set correct region (PR #2598)
* Play default tone for iOS push notifications of a new message (#2603)
* Add a new text field to messages to store client data (#2604)
* Remove audit db (PR #2601), Send audit logs to Loggly not SQL (#2602)


# 2019.6.12+rcc76921

* Ability to ask the client to send a new location data point (#2025)
* Remove next environment and old ansible file (PR #2578)
* Consolidate auditing (PR #2575)
* Update: geo (PR #2582)
* Test cleanup (PR #2580)
* Dialyzer fixes (PR #2584)
* Re-enable traffic and location logging on testing (PR #2583)
* Fix a cast error in the location audit log (#2586, PR #2587)
* Try to fix location auditing crash (#2590, PR #2591)
  * Make sure UserLocation fields are properly cast (PR #2592)


# 2019.6.5+r05cadfa

* Update: ecto_sql (PR #2556), honeybadger (PR #2561)
  * elixir, erlang (PR #2563), plug (PR #2564), dawdle (PR #2565)
  * firebase_admin_ex (PR #2567)
* Configure location accuracy uncertainty threshold to 50m (#2555)
* Remove db lookup for REST location upload (PR #2559)
* Refactor location auth changes (PR #2560)
* Ensure location share cache is primed (PR #2558)
* Remove cached location when last share expires (PR #2566)
* Refactor the `User` module (#2549)
* Preload cleanup (PR #2570)


# 2019.5.29+ra59468d

* Remove subscription from cache on bot delete (PR #2543)
  * Fixes crash (#2541)
* Update: stringprep (PR #2545)
* Loc share cache (PR #2546)
* Aggregate REST request metrics (PR #2548)
* Extend the current location TTL to 2 days (PR #2551)
  * Related: Some catchup location data points are not sent (#2547)
* ALB cowboy tweaks (PR #2553)


# 2019.5.22+r65a1de1

* Drop testing replicas back to 2 (PR #2533)
* Fix capturedAt and createdAt in current location (PR #2536)
  * Fixes: FunctionClauseError: no function clause matching ... (#2534)
* Move weekly auto deploy back to staging (#2535)
* Fix dialyzer warning and expiry time (PR #2539)
* Fix get presence crash (PR #2538)
* Add in `device` field of traffic_logs (#2519)


# 2019.5.21+re1a0715

* Fix missing error view (PR #2509)
  * Function WockyAPI.ErrorView.render/2 is undefined (#2508)
* Update: geo (PR #2505), recon (PR #2522), distillery (PR #2527)
  * ecto_sql (PR #2528), prometheus-ecto (PR #2532)
* Various geofence fixes (PRs #2510, #2511, #2512, #2513, #2517)
* Geofence tweaks (PR #2521)
* Add migration to clean up invitations to non-owned bots (PR #2526)
  * Fixes: Server is returning `bot:null` (#2525)
* Add observer_cli lib (PR #2531)


# 2019.5.15+rc8e0699

* Update: postgrex (PR #2480), exrun (PR #2481), phoenix (PR #2482)
  * ex_aws_s3 (PR #2488), excoveralls (PR #2489), ecto_sql (PR #2490)
  * phoenix (PR #2494)
* Fix crash when sending bulk invitation to own phone number (PR #2486)
  * [Wocky/next] CaseClauseError: no case clause matching: :self (#2483)
* Dialyzer fix (PR #2492)
* User search to include user accounts with empty names (#2487)
* Optimize the geofence processing algorithm (#2484, PR #2495)


# 2019.5.7+r060bbe2

* Update: phoenix (PR #2462), excoveralls (PR #2468)
  * bcrypt_elixir (PR #2469), credo_filename_consistency (PR #2476)
  * ex_aws (PR #2477), absinthe_phoenix (PR #2473)
* Multi factory insert (PR #2461)
* Multi bot invite (PR #2459)
  * Fixes: Instant BotInvitationResponse with accepted=false (#2457)
* Support available/unavailable online presence (#2440, PR #2458)
* Remove self-presence restriction (PR #2465)
  * Fixes: ArgumentError: Can't get presence data on self (#2463)
* Add Credo file/module name consistency check (PR #2464)
* Expose `activity_confidence` field in Live Location points (#2467)
* Filter out Live Location points with high accuracy uncertainty (#2466)
* Invalidate tokens when APNs returns 'unregistered' (#1960, PR #2472)
* Load testing changes (PR #2479)


# 2019.4.30+re26e052

* Update: kronky (PR #2455)
* Make location handlers run under supervisor (PR #2456)
* Add factoryInsert graphQL operation (PR #2454)


# 2019.4.26+rfadca8f

* Add healthcheck endpoint (PR #2444)
* Trigger a rn-chat build instead of wocky-client. (PR #2445)
* Add 556 as bypass prefix for load tests (PR #2446)
* Fix the version number in the build (PR #2447)
* Update: httpoison (PR #2448)
* Clean up old user locations and bot events (#2017)
* Fix crash in RefreshCurrentUser when no user is set (#2449, PR #2452)
* FunctionClauseError: Wocky.Tasks.Recurring.handle_info/2 (#2451, PR #2453)


# 2019.4.24

* Update: ecto_sql (PR #2434), dawdle et al (PR #2436), credo (PR #2438)
* Add transient flag (PR #2433)
* Tweak container build (PR #2437)
* Expose `activity` in (Live Location) data points (#2441)
* Roll back `testing` to old redis server (PR #2443)


# 2019.4.17+r0b83314

* Update: absinthe (PR #2425), eventually (PR #2426), dawdle_db (PR #2427)
* Fix name setting when one name is set and existing is nil (PR #2420)
* Shared location data points expose extra fields (#2338)
  * Remove surplus fields from location object (PR #2424)
* Issues with bot subscription logic (#2396)
  * Limit bot subscriptions and invitations to friends (PR #2421)
* Rework relay connections to use SQL cursors for paging (#1957)
  * Paginator fixes (PR #2432)
* Unify in-band notifications and push notifications (#2402)
* Remove server side invisibility mode (#2365)
* Use PostgreSQL 10 for tests (PR #2431)


# 2019.4.14+re16466c

* Update: postgrex (PR #2416), ex_aws_sqs (PR #2417)
* Handle all FCM response values (PR #2419)


# 2019.4.12+rad30a44

* Migrate `us1` to new db server (PR #2412)
* Fix FCM config and error handling (PR #2414, PR #2415)
  * UndefinedFunctionError: function nil.status/0 is undefined (#2413)


# 2019.4.9+r45f56a5

* Migrate `next` to new DB server (PR #2398)
* Migrate `staging` to new DB server (PR #2399)
* Update: bcrypt_elixir (PR #2401)
* Geofence entry/exit notifications should only go to friends (#2392)
* Server generated dynamic links to include android values (#2405)
* Allow enforcement of strict ordering on subscriptions (#2351)


# 2019.4.4+r080c705

* Update: dialyxir (PR #2394), plug (PR #2395)
* Migrate `testing` to new DB server (PR #2397)


# 2019.4.2+r2f74353

* Update: ecto_sql (PR #2393)


# 2019.4.2+rb7a8275

* Update: phoenix_pubsub_redis (PR #2389)
* Use correct number for unlimited SMS (PR #2388)
* Implement handling of batch location uploads (#2358, PR #2390)
* Update: phoenix (PR #2391)


# 2019.3.29+r7f25b99

* Don't start Dawdle pollers in "DB only" mode (PR #2385)
  * Fixes: Missing UserInvitationNotification notification (#2380)
* Use correct number for unlimited SMS (PR #2388)


# 2019.3.28+r2f56493

* Update: dialyxir (PR #2372)
* Small fixes and updates (PR #2375)
* Increase max redlock retries (#2374, PR #2377)
* Open port 80 on testing (PR #2382)
* Move loc share expiry to a swarm-managed process (PR #2379)
  * Run loc share cleanup from dawdle rather than a cronjob (#2354)
* Update tzdata (#2378, PR #2384)


# 2019.3.26+r3acc76d

* Update watcher version with config fix (PR #2371)


# 2019.3.26+raa9ae1a

* Update: kronky (PR #2360), credo (PR #2367)
* Limit URLs passed by ALBs (#2356, PR #2362, PR #2364)
* Support Android push notifications (#1919)
* Add ALB to remaining environments (PR #2366)
* Migrate Wocky to the new Dawdle and DawdleDB (PR #2359, PR #2369)
* Dialyzer changes (PR #2368)
* Tweak the Credo config and address issues (PR #2370)


# 2019.3.19+rc79b90b

* Update: kronky (PR #2343), phoenix (PR #2344)
  * firebase-admin-ex (PR #2345), db watcher (PR #2346)
  * wocky_db_watcher (PR #2348), absinthe_phoenix (PR #2349)
  * plug_cowboy (PR #2352)
  * Remove the CSV dependency (PR #2355)
* Subscription 'catchup' item is sent too early (#2342)


# 2019.3.13+rf578b8a

* Update: credo (PR #2334), redix (PR #2336)
* Fix error messages from ReST API (#2335, PR #2337)
* Use ALB/ingress rather than ELB on `testing` (PR #2339)


# 2019.3.7+r0e3e105

* Reinstate prometheus_ecto (PR #2328)
* Add prometheus_process_collector tracking (PR #2331)
* Remove unused script file (PR #2330)
* Fix crash when no name is specified in user update (PR #2333)


# 2019.3.6+r663029b

* Update: wocky_db_watcher (PR #2309), bcrypt_elixir (PR #2310)
  * excoveralls (PR #2318), ex_machina (PR #2314), redlock (PR #2313)
  * csv (PR #2317), honeybadger (PR #2315), phoenix_pubsub_redis (PR #2323)
* Set the distribution cookie to K8s namespace name (#2312, PR #2319)
* Live location sharing improvements (#2304, PR #2320)
  * Allow user's current location when starting a live location share.
  * Add catchup to the user location subscription.
* Store client-side-only per-user variables in client data blob (#2308)
* Name fields rework (#1145, PR #2322)
* Move absinthe[_phonnix] to 'working' branches (PR #2324)
* Remove Enum.into() (PR #2325)
* Bump elixir version to 1.8 (PR #2327)


# 2019.2.26+r288db8d

* Added logging for Swarm operations (PR #2294)
* Cleanup location handler (PR #2295)
* Update: sweet_xml (PR #2298), phoenix_pubsub (PR #2300)
  * redlock (PR #2299, PR #2307), stringprep (PR #2305)
* Add password to Elasticache/Redis server (#2165)
  * Use secure redis cluster (PR #2297) / Fix redlock auth (PR #2302)


# 2019.2.22+rf3e6e9e

* Fix post-handoff crash (PR #2293). Fixes crash #2287.


# 2019.2.22+r7377a3b

* Update: wocky_db_watcher (PR #2286)
* Add sharer field to location share object (#2290, PR #2291)
* Add `createdAt` and `expiresAt` to LocationShareNotification (#2289)


# 2019.2.21+r218339f

* Fix more cronjob app tags (PR #2285)


# 2019.2.21+rbe01364

* Update: ex_phone_number (PR #2277)
* Add API to cancel all active location shares (#2276)
* Cancel location sharing on block or unfriend (#2268)
* Install newer db watcher (PR #2281, PR #2282)
* Make tasks only start the wocky DB subsystem (PR #2283)
  * Fixes: Cron tasks delaying DB callbacks and notifications (#2275)
* Fix callback crash on deleted user (PR #2284)


# 2019.2.20+r861d0ac

* Add rexbug (PR #2274)


# 2019.2.19+r8eb3a56

* The user may not exist during location callback (PR #2264)
  * Fixes crash #2263
* Remove deployment notifications from #development (#2266)
* Maintain state in swarm worker handoff (PR #2273)
  * Fixes crash #2269


# 2019.2.18+rad3b125

* Add whitelist of numbers that can exceed sent SMS limit. (#2250)
* Change cronjob app tag; space out loc share jobs a little (PR #2265)


# 2019.2.16+r3e13b29

* Retrieve accounts which are sharing location to the queryer (#2243)
* Authenticate a whitelist of agent names (#2231)
* Location share notifications (PR #2242)
  * Sharing location should generate a new notification (#2236)
  * When location sharing ends, generate a new notification (#2244)
  * Rename share expiration cronjob manifests (PR #2252)
  * Start the application when expiring location shares (PR #2253)
* Use a new curl image in the CI process (PR #2249)
* Add missing command definition to Distillery (PR #2251)
* Update alpine and erlang/elixir versions (PR #2247)


# 2019.2.13+r3f29a23

* Update: mock (PR #2237), phoenix (PR #2238)
* Correctly handle error on invalid SMS target number (PR #2240)


# 2019.2.12+r097e263

* Update: geo_postgis (PR #2235), bcrypt_elixir (PR #2234), redix (PR #2233)


# 2019.2.11+re4e2a40

* Fix crash when own number is in bulk lookup (PR #2225)
* Fix image ready check and handling (PR #2226)
* Update: ex_phone_number (PR #2228), dataloader (PR #2227)
* Enable Twilio SMS sending on `next` and staging (PR #2230)
* Update: plug (PR #2232)


# 2019.2.5+ra60052c

* Replace absinthe_ecto's assoc helper with dataloader (PR #2201)
* Serialise user location updates through a single per-user process (#2030, PR #2200)
* Update: absinthe (PR #2209), excoveralls (PR #2213), credo (PR #2215)
  * dataloader (PR #2214), faker (PR #2224), ecto_sql (PR #2223)
* Remove call to delete current location (#2210) / Fixes crash #2205
* Fix error on multiple identical bulk lookup numbers (#2207, PR #2211)
* Add relationship field to userBulkLookup (#2208, PR #2212)
* Add notificationDelete mutation (#2220, PR #2222)


# 2019.2.1+refd5789

(Skipped 2019.2.1+r36cfa26)

* Update: ecto_homoiconic_enum (PR #2197)
* Fix dialyzer warnings (PR #2196)
* Use secure redis cluster [take 2] (PR #2190), then reverted (PR #2199)
* Live location sharing, part 2 (PR #2198)
  * Part of: New APIs for live location sharing (#2146)
* Add firebase key to testing, next (PR #2204)
* Add logging for SMS sandbox (PR #2202)
* Fix goth creds (PR #2206)


# 2019.1.31+r43318c4

* Update: bamboo (PR #2193)
* Add types filter to Notifications retrieval (#2194)


# 2019.1.29+r24a524e

* Fix Twilio error handling; Use hardwired CC for bypass numbers (#2189)
  * Fixes crash #2181
* Live location sharing, part 1 (PR #2185)
  * Part of: New APIs for live location sharing (#2146)


# 2019.1.28+r69b1e67

* Update: credo (PR #2187)
* Add missing Twilio auth token to next (PR #2188)


# 2019.1.25+r518bc7d

* Fix bulk user crash (PR #2179, PR #2182)
* Update: prometheus_ex (PR #2180)


# 2019.1.22+r07e6454

* Update: guardian (PR #2164, PR #2167), timex (PR #2174), redix (PR #2177)
* New APIs for bulk phone number query, sms invite, user relation (#2121)
* Bot clustering search (#1986, PR #2163)
* Fix auth error formatting (PR #2168)
* Fix mock use (PR #2172)
* Use secure redis cluster (PR #2169), then reverted (PR #2175)
* Fix process aliveness check for presence (PR #2176). Fixes crash (#2138)


# 2019.1.16+r9a91d02

* Remove all public-scope graphql (#2156)
* Add bot_created flag to remember if user created a bot/location (#2153)
* Change `Block` type to include a sub-type of user (#2152)


# 2019.1.9+r47ead10

* Update: absinthe_metrics (PR #2132), geocalc (PR #2133)
  * ecto_sql (PR #2134), phoenix_pubsub_redis (PR #2135)
  * excoveralls (PR #2143), confex (PR #2144), meck (PR #2148)
  * wocky_db_watcher (PR #2147)
* Allow client versions with more than 3 digit groups (#2130, PR #2131)
* Add id field to message object (PR #2142)
* Rework relationship system (#2122)
  * Fix handling for contacts permission error (#2149, PR #2150)
  * Add handler for invalid pid construction (#2138, PR #2151)
* Create a way to get source/thumbnail url from `trosUrl` (#2136)


# 2018.12.20+r9697983

* Preload message sender/receiver as part of query 
  * Fixes new crash (#1287)


# 2018.12.20+r40b03e3

* Update: plug_cowboy (PR #2113), absinthe_relay (PR #2124)
* Post-XMPP audit and code cleanup (#2103, PR #2107)
* Fix a crash when there is a certain GQL error (#2106, PR #2109)
* Twilio integration (#1974, PR #2117)
* Remove the "initial contacts" code (#2114)
* Fix waiter msg crash (#2116, PR #2119)
* Improve test coverage (#2118, PR #2120)
* Refactor roster et al to use full user structs (PR #2123)
* Log push notification errors instead of sending Honeybadgers (#2126)
* Fix rendering of HTTP errors (#2111, PR #2125)
* Fix crash on invalid bot item publication changeset (#1953, PR #2128)
* Improve errors on message sending (PR #2129)


# 2018.12.13+rb32e32f

This is the first release on the 'non-XMPP' (formerly `next`) branch.

* Set up 'Next' server environment (#1948)
* Remove old features (PR #1913)
* GraphQL presence management (#1671, PR #1934)
* Migrate all unit tests to ExUnit (#1885)
* Update: jsx (PR #1962), bimap (PR #1963)
* Add checkpoint migration and remove bot_shares (PR #1964)
* Make some parts of the users tests synchronous (PR #1965)
* Update: faker (PR #1968), excoveralls (PR #1967)
* GraphQL messaging (#1672)
* Geofence anomalies for embedded location points (#1959)
  * Fix: Double processing
  * Fix: Sometimes the user's hidden status was ignored
* GraphQL support for (un)blocking (#1940)
* Update: phoenix_ecto (PR #1978), ex_machina (PR #1981), 
  * distillery (PR #1982, PR #1989), timex (PR #1983), cors_plug (PR #1990)
* Rename resource to device (PR #1979)
* Push notification token registration for GraphQL (#1917)
* Replace presence subscription with followee subscription (PR #1995)
* Add limits to localBots query (PR #1996)
* Pull all bot events at once during geofence calculation (PR #2000)
* Fix followee/following bug; add created_at to contacts (PR #2009)
* Don't save user locations on us1 unless hippware (PR #2011)
* Increase allowed query complexity (PR #2013)
* Disable tracing on staging (PR #2013)
* Rework localBots query to report if the query area is too large (PR #2018)
* Disable traffic log on us1 unless hippware (PR #1997)
* Fix the user Hippware check when email is nil (PR #2021)
* Update: ranch (PR #2026), xml_builder (PR #2031)
  * redlock (PR #2032), credo (PR #2033), excoveralls (PR #2034)
* Delete a user's S3 images when they delete their account (#2006)
* Restrict push notification logs on us1 (#2016)
* Allow using a server-generated JWT for location updates (PR #2028)
  * The auth mechanism for location uploads needs to be long lived (#1695)
* A couple of TROS store renamings (PR #2041)
* Remove XMPP/MongooseIM (PR #1985, PR #2048)
* Update: exrun (PR #2043), faker (PR #2045), redix (PR #2047)
* Delete Firebase data when a user deletes their account (#2005)
* Update: bypass (PR #2055)
* Upgrade to Phoenix 1.4 and Ecto 3.0 (#2003, PR #2044)
* Make push payload schema nullable (PR #2062)
* The Ecto pool size is not set correctly on deploy (#2060)
* Add JWT signing keys to next (PR #2070)
* Update: confex (PR #2068), absinthe (PR #2075), ecto_sql (PR #2078)
* Remove the push payload validation (#2057, PR #2067)
* Fix the Honeybadger environment (#2065, PR #2071)
* Add websocket-level logging to graphQL (#1970, PR #2077)
* Update: geo_postgis (PR #2080)
* Return error when contact relationship is none (PR #2084)
* Only call set_user on actual websocket connections (PR #2086)
* Authentication clean up and features (PR #2085)
  * Track user agent details in the database (#1923)
* Configure APNS certificate for 'next' server (#1955)
* Fix GQL file upload parameters (#2089, PR #2091)
* Remove XML from bot item API (#2081)
* Fix the generation of file upload headers (PR #2094)
  * Upload requests returning error regarding null headers (#2093)
* Replace the XML message envelope (#2024)
* Make the naming of attached images consistent across objects (#2098)
* Update: recon (PR #2102), confex (PR #2101)
* Remove remaining uses of JIDs (#2090, PR #2099)
* Make next mainline (#2110)

----------

# DISCONTINUITY

After `2018.10.23+r14cec96`, the source code tree split into two branches. For releases on the old branch, please see https://github.com/hippware/wocky/blob/xmpp-stable/CHANGELOG.md


# 2018.10.23+r14cec96

* GraphQL roster management (#1832)
* Add schema docs for contacts (PR #1943)
* Update: phoenix_pubsub (PR #1944)
* Geofence not triggering on location embedded within botInvitationRespond (#1939)
* Update: espec (PR #1949), geocalc (PR #1950)


# 2018.10.17+r06b8c08

* Update the bundle ID for push notifications (PR #1928)
  * ... already applied to us1.
* Update: espec (PR #1930), redix (PR #1931), kadabra (PR #1932)
  * lager (PR #1933), pigeon (PR #1938), geocalc (PR #1936)
  * phoenix_ecto (PR #1937)


# 2018.10.11+raebd3f3

* Update us1/Production APNS certificate (PR #1925)


# 2018.10.10+r7a88abd

* Enable connection draining for all environments (PR #1916, 2018.10.10+r2052921)
* Accept a location update as part of invitation acceptance (#1921)


# 2018.10.9+r3399de5

* Migrate bot shares to invitations (PR #1914)


# 2018.10.9+r5fba208

* Update: ecto (PR #1902), ex_guard (PR #1909), gen_stage (PR #1910)
* Tidy up expired user invitation codes (#1901)
* Full support for blocking on notifications and invitations (#1880, PR #1905)
* Add db-dump cronjob and db migration test step (PR #1904)
* Minimal changes to hard-wire public and geofence fields on bots (PR #1881)
* Fix blocking tests (PR #1911)


# 2018.10.3+ra0dc575

* Use a version of PostgreSQL with Healthcheck for tests (#1892)
* Update: hackney (PR #1894, PR #1895), bypass (PR #1896)
  * prometheus_ecto (PR #1897)
* User invite and redeem functionality (#1889)


# 2018.9.27+r9c0bbaa

* Update: faker (PR #1890)
* Reduce logging (PR #1893)


# 2018.9.25+r50db3a0

* Schedule `staging` deployments (#1852)
* Update: timex (PR #1869), honeybadger (PR #1868), distillery (PR #1867)
  * redix (PR #1866), absinthe_relay (PR #1873), lager (PR #1875)
  * credo (PR #1882), excoveralls (PR #1883)
* Fix up all the dialyzer warnings (PR #1865)
* Integrate GraphQL notification queries + subscriptions (#1871)
* Add beforeID to match afterID on notification query (PR #1877)


# 2018.9.19+r826f473

* Fix crash with further requests after user deletion (PR #1842)
* Update push notification certificates (#1843)
* Update: credo (PR #1846), pigeon (PR #1849), redix (PR #1851)
* New server side push notifications (#1847)


# 2018.9.13+r90cb411

* Make bots visible whenever a user is subscribed to them (PR #1841)


# 2018.9.12+r0531a1e

* Add deletion of notification to notification subscription (#1828)
* Add GraphQL `deleteUser` mutation (#1824)
* Add message archive interface to GraphQL (#1673)
* Attempting to load bot items on an accepted bot causes 403 error (#1830)
* Update: phoenix_ecto (PR #1835), elixometer (PR #1837)
  * hackney (PR #1840)
* Nudge test timing to allow DB callbacks more time to complete (PR #1838)


# 2018.9.5+r832450f

* Remove Collections code (#1804, PR #1807, PR #1815)
* Update: prometheus_ex (PR #1816), lager (PR #1818), exml (PR #1821)
* Add retry system to push notifications (PR #1814)
* Change `botInvite` mutation to accept an array of user ids (#1808)
* Add config option to disable welcome emails (#1819)
* Deployment improvements (PR #1823)


# 2018.8.30+r5c75ca6

* Set the accuracy threshold to 90m across the board (PR #1806)


# 2018.8.30+r5280dc0

* Update: distillery (PR #1791, PR #1792, PR #1801), excoveralls (PR #1794)
  * bcrypt_elixir (PR #1793, PR #1802), geo_postgis (PR #1800), idna (PR #1805)
* Set location captured_at to now if not provided by client (#1795)
* Change GraphQL metrics buckets to better reflect actual values (PR #1798)
* Subscribe user upon invitation acceptance (PR #1797)
* Deploy to testing on successful build (PR #1803)
  * Automatically deploy wocky to testing when tests pass (#1764)


# 2018.8.24+r43aa7c9


* Move prometheus role management to infrastructure (PR #1789)
* Notification tweaks (ie. Rename "reply" to "respond" etc.) (PR #1788)


# 2018.8.23+r04159a0

* Update: distillery (PR #1771), guardian (PR #1781)
* Deployment and infrastructure stuff
  * Automatically deploy wocky to testing when tests pass (#1764)
    * Then reverted (PR #1782)
  * Add names for ports; remove non-required service (PR #1773)
  * Update logging location for service LBs (PR #1776)
  * Use the makefile to deploy instead of the script (PR #1777)
  * Split deploy task for running from CI (PR #1779)
* Bump the accuracy threshold on staging to 90 (PR #1786)


# 2018.8.20+r5001d7d

* EKS deployment configuration (PR #1738)
* Reinstate new bot item IDs (PR #1756)
* Update: distillery (PR #1758, PR #1762, PR #1768), bamboo (PR #1761)
  * ranch (PR #1767)
* Sprinkle magic kube pixie dust on external services (PR #1759)
* Re-enable cross zone load balancing (PR #1760)
* [New feature] Notifications list (#1711)
* Add role bindings for new Prometheus setup (PR #1766)


# 2018.8.14+r3c0b954

* Update: bcrypt_elixir (PR #1729), phoenix (PR #1732)
  * exometer_prometheus (PR #1737), distillery (PR #1742)
  * excoveralls (PR #1740), espec (PR #1741), phoenix_pubsub (PR #1743)
  * distillery (PR #1749), guardian_firebase (PR #1750)
* Update to Elixir 1.7.1 (PR #1727, PR #1731)
* First iteration of Geofence/Debounce algorithm changes (PR #1726)
  * Geofence mechanics algorithm shortcomings (#1713)
* Disable old push notification tokens (#1714, PR #1736)
* Clean up and refactor DB callback handlers (PR #1739)
* Fix predeploy commands and migrations (PR #1751, PR #1752, PR #1754)
* Add unique bot item key (PR #1734)
  * Then reverted (PR #1753)


# 2018.8.3+r7321aad

* Add logging for failed push notification requests (PR #1723)
* Update: ranch (PR #1725)


# 2018.8.1+rea1193b

* Update: credo (PR #1720), algolia (PR #1717)
* Add missing watcher region config


# 2018.7.30+re04463a

* Replace Task.async with Task.start_link (PR #1719)
* Update: kadabra (PR #1721)


# 2018.7.23+r95ccfc3

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

Important: Update to Erlang 21/Elixir 1.6.6 (#1619, PR #1630)

* Add createdAt to Bot fields returned via GraphQL (#1617)
* Speculative attempt to fix bot/sub locking (PR #1622)
* Update (via Dependabot): 
  * ex_aws_s3 (PR #1621), wocky_db_watcher (PR #1633, PR #1638), 
  * honeybadger (PR #1635, PR #1648), recon (PR #1644), peerage (PR #1653)
* Update the ECR credentials used by Codeship (PR #1624)
* Config cleanup (PR #1625)
* Various tweaks to improve test stability and error output (PR #1626)
* Add isFetch field to location updates (PR #1627)
* Update to Erlang 21/Elixir 1.6.6 (#1619, PR #1630)
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
* Remove 
