-module(erlconf_tests).

-include_lib("eunit/include/eunit.hrl").

tmdb_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(app()),
      ?_test(rebar())
   ]}.

% Tests

app() ->
  {Status, Conf} = erlconf:open(app, "../test/app.app", [{save_on_close, false}]),
  ?assertEqual(ok, Status),
  [{application, erlconf, Term}] = erlconf:term(Conf),
  {applications, List} = lists:keyfind(applications, 1, Term),
  Term1 = lists:keyreplace(applications, 1, Term, {applications, [lager|List]}),
  erlconf:term(Conf, {application, erlconf, Term1}),
  ?assertEqual([{application, erlconf, Term1}], erlconf:term(Conf)),
  ?assertEqual(ok, erlconf:save(Conf, "app-copy.app")),
  ?assertEqual(close, erlconf:close(Conf)).


rebar() ->
  {Status, _} = erlconf:open(rebar, "../test/rebar.config", [{save_on_close, false}, {format, none}]),
  ?assertEqual(ok, Status),
  Term = erlconf:term(rebar),
  Term1 = lists:keyreplace(cover_enabled, 1, Term, {cover_enabled, false}),
  ?assertEqual({ok, Term1}, erlconf:term(rebar, Term1)),
  ?assertEqual(ok, erlconf:save(rebar, "rebar-copy.config")),
  ?assertEqual(close, erlconf:close(rebar)).

% Helpers

setup() ->
  ok.

teardown(_) ->
  ok.
