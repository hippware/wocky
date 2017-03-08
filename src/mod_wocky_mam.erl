%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky C* Backend for mod_mam
%%%
%%% This module provides the archive_message_hook and lookup_messages_hook that
%%% are called by `mod_mam' to archive and retrive messages.
%%%
%%% See XEP-0313 for MAM details and XEP-0059 for Result Set Management (RSM)
%%% descriptions.
%%%
%%% Data is stored in the `message_archive' table and the `archive_id'
%%% materialized view. Of particular note are the two fields `id' and `time'.
%%% These are both unique, chronologically ordered IDs for the message. The key
%%% difference is that `id' is supplied by MIM when it archives the message. It,
%%% rather unfortunately, relies on Erlang's `now/0' function, the use of which
%%% is now frowned upon. In order to avoid our code relying on that, the `time'
%%% field is a C* v1 UUID which also encodes that time at which the message was
%%% archived. Thus, in this module, `id' is treated as a unique identifier, but
%%% we rely on neither its ordering nor its encoding of the message's timestamp,
%%% both of which are instead supplied by `time'.
%%%
%%% While archiving is relatively simple, there's a bunch of different ways to
%%% look up messages. Each option has an optional limit on the number of
%%% messages returned by the request. Each can also be asked to return a pair of
%%% counts, as described below:
%%%
%%% * Time-only:
%%%     Provides a start and/or end time and optional limit.
%%%     Counts are:
%%%         * Total messages in the archive within specified time range
%%%         * Messages in archive before first returned
%%%
%%% * Borders-only:
%%%     Provides an `#mam_borders{}' record with inclusive or exclusive start
%%%     and/or end ID values.
%%%     Counts are:
%%%         * Total messages in the archive within the specified range of IDs
%%%         * Messages in archive before first returned
%%%
%%% * RSM ID-paging:
%%%     Uses a `#rsm_in{}' record to provide Result Set Management paging. This
%%%     allows forward or backwards paging from a specified ID.
%%%     Counts are:
%%%         * Total messages in the archive
%%%         * The index of the ''oldest'' message in the returned set.
%%%
%%% * RSM Index-paging:
%%%     Provides jump straight to a fixed offset into a user's full archive,
%%%     using the `index' field of an `#rsm_in{}' record. Note that this is
%%%     necessarily a slower operation than any of the above (and is specified
%%%     as such in the RSM XEP) since the concept of an "index" is based
%%%     entirely on the number of items in the archive and cannot therefore be
%%%     easily optimised beyond O(N).
%%%     Counts are:
%%%         * Total messages in the archive
%%%         * The index of the first message (should match requested index
%%%           assuming that falls within the valid range)
%%%
%%%
%%% Other random notes:
%%%
%%% The `rms_in.max' field is at all times treated as being interchangeable with
%%% the `PageSize' parameter. If one is `undefined', the valid value will be
%%% used. It is an error to call `lookup_message_hook' with two different
%%% integer values in these fields.
%%%
-module(mod_wocky_mam).

-behaviour(gen_mod).

-compile({parse_transform, fun_chain}).

-include("wocky.hrl").
-include_lib("ejabberd/include/jlib.hrl").

% This value MUST be higher than the one in the backend being used (such as
% mod_mam_riak_timed_arch_yz) so that the messages are processed
-define(LOOKUP_HOOK_PRIORITY, 100).

%% gen_mod handlers
-export([start/2, stop/1]).

-export([
         %% MAM hook handlers
         lookup_messages_hook/14
        ]).

-ignore_xref([lookup_messages_hook/14]).

-type result_row() :: {non_neg_integer(), ejabberd:jid(), exml:element()}.

%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE,
                       lookup_messages_hook, ?LOOKUP_HOOK_PRIORITY),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(mam_lookup_messages, Host, ?MODULE,
                          lookup_messages_hook, ?LOOKUP_HOOK_PRIORITY),
    ok.

%%%===================================================================
%%% mam_lookup_messages callback
%%%===================================================================

-spec lookup_messages_hook(
        Result         :: term(),
        Host           :: ejabberd:server(),
        UserID         :: mod_mam:archive_id(),
        UserJID        :: ejabberd:jid(),
        RSM            :: jlib:rsm_in() | undefined,
        Borders        :: mod_mam:borders() | undefined,
        Start          :: mod_mam:unix_timestamp() | undefined,
        End            :: mod_mam:unix_timestamp() | undefined,
        Now            :: mod_mam:unix_timestamp(),
        WithJID        :: ejabberd:jid() | undefined,
        PageSize       :: non_neg_integer(),
        LimitPassed    :: boolean(),
        MaxResultLimit :: non_neg_integer(),
        IsSimple       :: boolean()
       ) ->
    {ok, {TotalCount  :: non_neg_integer() | undefined,
          Offset      :: non_neg_integer() | undefined,
          MessageRows :: [result_row()]}}.

lookup_messages_hook({ok, {Count, Offset, MessageList}},
                     _Host, _UserID, _UserJID, #rsm_in{reverse = true},
                     _Borders, _Start, _End, _Now, _WithJID,
                     _PageSize, _LimitPassed, _MaxResultLimit,
                     _Simple) ->
    {ok, {Count, Offset, lists:reverse(MessageList)}};

lookup_messages_hook(Result, _Host, _UserID, _UserJID, _RSM,
                     _Borders, _Start, _End, _Now, _WithJID,
                     _PageSize, _LimitPassed, _MaxResultLimit,
                     _Simple) ->
    Result.
