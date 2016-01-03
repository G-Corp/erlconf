-module(erlconf_tests).

-include_lib("eunit/include/eunit.hrl").

tmdb_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(app())
      , ?_test(rebar())
      , ?_test(custom())
   ]}.

% Tests

app() ->
  {Status, Conf} = erlconf:open(app, "test/app.app", [{save_on_close, false}]),
  ?assertEqual(ok, Status),
  [{application, erlconf, Term}] = erlconf:term(Conf),
  {applications, List} = lists:keyfind(applications, 1, Term),
  Term1 = lists:keyreplace(applications, 1, Term, {applications, [lager|List]}),
  erlconf:term(Conf, {application, erlconf, Term1}),
  ?assertEqual([{application, erlconf, Term1}], erlconf:term(Conf)),
  ?assertEqual(ok, erlconf:save(Conf, ".tests/app-copy.app")),
  ?assertEqual(close, erlconf:close(Conf)).


rebar() ->
  {Status, _} = erlconf:open(rebar, "test/rebar.config", [{save_on_close, false}, {format, none}]),
  ?assertEqual(ok, Status),
  Term = erlconf:term(rebar),
  Term1 = lists:keyreplace(cover_enabled, 1, Term, {cover_enabled, false}),
  ?assertEqual({ok, Term1}, erlconf:term(rebar, Term1)),
  ?assertEqual(ok, erlconf:save(rebar, ".tests/rebar-copy.config")),
  ?assertEqual(close, erlconf:close(rebar)).

custom() ->
  ?assertMatch({ok, _}, erlconf:open(custom, ".tests/custom.config", [{save_on_close, false}])),
  Term = [
          {release,{jorel,"1.0.0"},
           [jorel,vsn,tempfile,sh,getopt,eutils,erlydtl,erlconf,edown,color,
            bucs,rebar,sasl]},
          {boot,[jorel,sasl]},
          {all_deps,false},
          {output_dir,"_jorel"},
          {exclude_dirs,["_jorel","_relx","_rel","test"]},
          {include_src,false},
          {include_erts,false},
          {sys_config,"config/jorel.config"},
          {vm_args,"config/vm.args"},
          {disable_relup,false},
          {providers,[jorel_provider_tar,jorel_provider_zip,jorel_provider_deb,
                      jorel_provider_git_tag]}
         ],
  ?assertEqual({ok, Term}, erlconf:term(custom, Term)),
  ?assertEqual(ok, erlconf:save(custom)),
  ?assertEqual(close, erlconf:close(custom)),
  ?assert(filelib:is_regular(".tests/custom.config")).

% Helpers

setup() ->
  file:make_dir(".tests").

teardown(_) ->
  ok.
