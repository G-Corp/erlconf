-module(erlconf).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
  open/2,
  open/3,
  close/1,
  save/1,
  save/2,
  saved/1,
  term/1,
  term/2,
  format/1
  ]).

-record(conf, {
    file,
    data,
    lasts = [],
    modified = false,
    autosave = false,
    save_on_close = true,
    history = 0,
    format = pretty
  }).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% internal use only
-export([none/1, pretty/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

open(Name, File) ->
  open(Name, File, []).
open(Name, File, Options) when is_atom(Name) ->
  Conf = #conf{ file = File },
  case filelib:is_regular(File) of
    true ->
      case file:consult(File) of
        {ok, Data} -> 
          gen_server:start({local, Name}, ?MODULE, [Conf#conf{data = Data}, Options], []);
        E -> E 
      end;
    false ->
      gen_server:start({local, Name}, ?MODULE, [Conf#conf{data = []}, Options], [])
  end.


close({ok, PID}) -> close(PID);
close(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> throw(invalid_name);
    PID -> close(PID)
  end;
close(PID) when is_pid(PID) ->
  case gen_server:call(PID, save_on_close) of
    ok -> gen_server:call(PID, stop);
    error -> {error, save_failed}
  end.

term({ok, PID}) -> term(PID);
term(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> throw(invalid_name);
    PID -> term(PID)
  end;
term(PID) when is_pid(PID) ->
  gen_server:call(PID, term).

term({ok, PID}, Term) -> term(PID, Term);
term(Name, Term) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> throw(invalid_name);
    PID -> term(PID, Term)
  end;
term(PID, Term) ->
  gen_server:call(PID, {term, Term}).

save({ok, PID}) -> save(PID);
save(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> throw(invalid_name);
    PID -> save(PID)
  end;
save(PID) when is_pid(PID) ->
  gen_server:call(PID, save).

save({ok, PID}, File) -> save(PID, File);
save(Name, File) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> throw(invalid_name);
    PID -> save(PID, File)
  end;
save(PID, File) when is_pid(PID) ->
  gen_server:call(PID, {save, File}).

saved({ok, PID}) -> saved(PID);
saved(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> throw(invalid_name);
    PID -> saved(PID)
  end;
saved(PID) when is_pid(PID) ->
  gen_server:call(PID, saved).

format({ok, PID}) -> format(PID);
format(Name) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> throw(invalid_name);
    PID -> format(PID)
  end;
format(PID) when is_pid(PID) ->
  gen_server:call(PID, format).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Conf, Options]) ->
  Conf1 = case lists:keyfind(autosave, 1, Options) of
    {autosave, true} -> Conf#conf{autosave = true};
    {autosave, false} -> Conf#conf{autosave = false};
    _ -> Conf
  end,
  Conf2 = case lists:keyfind(format, 1, Options) of
    {format, pretty} -> Conf1#conf{format = pretty};
    {format, none} -> Conf1#conf{format = none};
    _ -> Conf1
  end,
  Conf3 = case lists:keyfind(history, 1, Options) of
    {history, V} when is_integer(V) -> Conf2#conf{history = V};
    {history, _} -> Conf2#conf{history = 0};
    _ -> Conf2
  end,
  Conf4 = case lists:keyfind(save_on_close, 1, Options) of
    {save_on_close, true} -> Conf3#conf{save_on_close = true};
    {save_on_close, false} -> Conf3#conf{save_on_close = false, autosave = false};
    _ -> Conf3
  end,
  Conf5 = if
    Conf4#conf.history > 0 -> Conf4#conf{lasts = [Conf4#conf.data]};
    true -> Conf4
  end,
  {ok, Conf5}.

handle_call(term, _From, State) ->
  {reply, State#conf.data, State};
handle_call({term, Term}, _From, State) ->
  State1 = if
    is_list(Term) -> State#conf{data = Term};
    true -> State#conf{data = [Term]}
  end,
  State2 = State1#conf{modified = true},
  case autosave(State2) of
    {ok, State3} -> {reply, {ok, State3#conf.data}, State3};
    E -> {reply, E, State}
  end;
handle_call(saved, _From, State) ->
  {reply, is_saved(State), State};
handle_call(save, _From, State) ->
  {Result, State1} = do_save(State),
  {reply, Result, State1};
handle_call(save_on_close, _From, State) ->
  {Result, State1} = if
    State#conf.save_on_close =:= false -> {ok, State};
    true -> do_save(State)
  end,
  {reply, Result, State1};
handle_call({save, File}, _From, State) ->
  {Result, State1} = do_save(State, File),
  {reply, Result, State1};
handle_call(format, _From, State) ->
  {reply, do_format(State), State};
handle_call(stop, _From, State) ->
  {stop, normal, close, State};
handle_call(_Request, _From, State) ->
  io:format("handle_call~n"),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  io:format("handle_cast~n"),
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("handle_info~n"),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

is_saved(State) ->
  not State#conf.modified.

autosave(State) ->
  if
    State#conf.autosave =:= true -> do_save(State);
    true -> {ok, State}
  end.

do_save(State) ->
  do_save(State, State#conf.file).
do_save(State, File) ->
  case is_saved(State) of
    true -> {ok, State};
    false ->
      State1 = historize(State), 
      Result = do_format(State1),
      case file:write_file(File, Result) of
        ok -> {ok, State1#conf{modified = false}};
        _ -> {error, State}
      end
  end.

do_format(State) ->
  FormatFun = State#conf.format,
  lists:foldl(fun(Term, Content) ->
          Content ++ apply(?MODULE, FormatFun, [Term]) 
      end, "", State#conf.data).

historize(State) ->
  case State#conf.history of
    0 -> State;
    N -> 
      Lasts = State#conf.lasts,
      Lasts1 = if
        length(Lasts) =:= N ->
          [_|RevLasts] = lists:reverse(Lasts),
          [State#conf.data|lists:reverse(RevLasts)];
        true ->
          [State#conf.data|Lasts]
      end,
      State#conf{lasts = Lasts1}
  end.

none(Term) -> lists:flatten(io_lib:format("~p", [Term]) ++ ". ").

% The following functions where initialy developped by Fabian Linzberger
% (https://gist.github.com/lefant/4671722) and modified my Gregoire Lejeune
pretty(Term) ->
  Abstract = erl_syntax:abstract(Term),
  AnnF = fun(Node) -> annotate_tuple(Node) end,
  AnnAbstract = postorder(AnnF, Abstract),
  HookF = fun(Node, Ctxt, Cont) ->
      Doc = Cont(Node, Ctxt),
      prettypr:above(prettypr:empty(), Doc)
  end,
  io_lib:format("~s~n", [
      lists:flatten(
        erl_prettypr:format(
          AnnAbstract, [{hook, HookF}, {paper, 160}, {ribbon, 145}])) ++ "."]).

annotate_tuple(Node) ->
  case erl_syntax:type(Node) of
    tuple -> erl_syntax:add_ann(tuple, Node);
    _ -> Node
  end.

postorder(F, Tree) ->
  F(case erl_syntax:subtrees(Tree) of
      [] -> Tree;
      List -> erl_syntax:update_tree(Tree,
                                     [[postorder(F, Subtree)
                                       || Subtree <- Group]
                                      || Group <- List])
    end).

