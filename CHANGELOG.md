# Change Log

Also: [Deployment history](https://github.com/hippware/tr-wiki/wiki/Server-deployment-history)

Ticket numbers refer to the ticket tracker for this project if not specified. 


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
