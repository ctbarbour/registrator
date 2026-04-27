-module(registrator_replica_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([prop_remote_idempotent/1,
	 prop_local_bounce_is_noop/1,
	 prop_seen_monotonic/1,
	 prop_stale_ops_are_noop/1,
	 prop_add_remove_clears/1,
	 prop_merge_idempotent/1,
	 prop_merge_commutative/1,
	 prop_two_replica_convergence/1,
	 prop_merge_recovers_from_partition/1]).

all() ->
    [
     prop_remote_idempotent,
     prop_local_bounce_is_noop,
     prop_seen_monotonic,
     prop_stale_ops_are_noop,
     prop_add_remove_clears,
     prop_merge_idempotent,
     prop_merge_commutative,
     prop_two_replica_convergence,
     prop_merge_recovers_from_partition
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(_Config) ->
    ok.

prop_remote_idempotent(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica:prop_remote_idempotent(), Config).

prop_local_bounce_is_noop(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica:prop_local_bounce_is_noop(), Config).

prop_seen_monotonic(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica:prop_seen_monotonic(), Config).

prop_stale_ops_are_noop(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica:prop_stale_ops_are_noop(), Config).

prop_add_remove_clears(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica:prop_add_remove_clears(), Config).

prop_merge_idempotent(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica:prop_merge_idempotent(), Config).

prop_merge_commutative(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica:prop_merge_commutative(), Config).

prop_two_replica_convergence(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica_statem:prop_two_replica_convergence(), Config).

prop_merge_recovers_from_partition(Config) ->
    ct_property_test:quickcheck(
      prop_registrator_replica_statem:prop_merge_recovers_from_partition(), Config).
