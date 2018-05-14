%%%-------------------------------------------------------------------
%% @doc odc public API
%% @end
%%%-------------------------------------------------------------------

-module(odc).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%% Supervisor callbacks
-export([init/1, config/0, jamdb_test/0, erloci_test/0, erloci_nif_test/0]).

%%====================================================================
%% API
%%====================================================================

start() -> application:ensure_all_started(?MODULE).
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
stop() -> application:stop(?MODULE).
stop(_State) ->
    ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

config() ->
    file:consult(code:where_is_file(".config.erl")).

% Current count 1948464
-define(COUNT_SQL, "
SELECT
    COUNT(*)
FROM
    BDCBX
WHERE
    CBX_DATETIME >= TO_DATE('04.04.2018')
    AND CBX_DATETIME < TO_DATE('05.04.2018')
").

-define(ROWS_SQL, "
SELECT
    CBX_ID, CBX_BIHID, CBX_MAIN_ATTRS, CBX_DATETIME, CBX_EVENTTYPE,
    CBX_GROUP_ATTRS, CBX_UMS_NODE_ID, CBX_REDIRECT_REASON, CBX_RESELLER_ID,
    CBX_ORGANISATION_ID, CBX_COS_ID, CBX_MEDIABOX, CBX_LAST_COS_CHANGE, CBX_IMSI
    , CBX_SESSION_ID, CBX_PARAM_ATTRS, CBX_CALLER, CBX_CALLEE, CBX_DURATION,
    CBX_PAGES, CBX_DATESTART, CBX_DATEEND, CBX_ADMIN_MESSAGE_ID, CBX_MESSAGE_ID,
    CBX_MEMO_ID, CBX_STATUS, CBX_REASON, CBX_CALL_TYPE, CBX_MEDIABOX_ID,
    CBX_MEDIABOX_TYPE, CBX_DESTINATION, CBX_DESTINATION_PHONE,
    CBX_DESTINATION_EMAIL, CBX_DESTINATION_TYPE, CBX_PREFIX,
    CBX_TERMINATION_SIDE, CBX_TERMINATION_REASON, CBX_MESSAGE_DEPOSIT_DATE
FROM
    BDCBX
WHERE
    CBX_DATETIME >= TO_DATE('04.04.2018')
    and CBX_DATETIME < TO_DATE('05.04.2018')
").

-define(PROFILE(_M, _F, _A),
        (fun() ->
            {_T, _V} = timer:tc(_M, _F, _A),
            io:format("[~p:~p:~p] ~p:~p/~p in ~p us~n",
                      [?MODULE, ?FUNCTION_NAME, ?LINE, _M, _F, length(_A), _T]),
            _V
         end)()
       ).

jamdb_test() ->
    case catch config() of
        {ok, [#{host := Host, port := Port, user := User,
                password := Password, service_name := Service}]} ->
            Opts = [{host, Host},
                    {port, Port},
                    {user, User},
                    {password, Password},
                    {service_name, Service},
                    {app_name, "jamdbtest"}],
            {ok, Pid} = ?PROFILE(jamdb_oracle, start_link, [Opts]),
            ?PROFILE(jamdb_oracle, sql_query, [Pid, ?COUNT_SQL]);
        Error -> error(Error)
    end.

erloci_test() ->
    case catch config() of
        {ok, [#{tns := TnsStr, user := UserStr, password := PasswordStr}]} ->
            OciPort = ?PROFILE(
                         erloci, new,
                         [[{logging, false},
                           {env, [{"NLS_LANG", "GERMAN_SWITZERLAND.AL32UTF8"}]}]]
                       ),
            Tns = list_to_binary(TnsStr),
            User = list_to_binary(UserStr),
            Password = list_to_binary(PasswordStr),
            OciSession = ?PROFILE(OciPort, get_session, [Tns, User, Password]),
            SelCountStmt = ?PROFILE(OciSession, prep_sql, [?COUNT_SQL]),
            ?PROFILE(SelCountStmt, exec_stmt, []),
            {{rows,[[RowCount]]},true} = ?PROFILE(SelCountStmt, fetch_rows, [10]),
            Total = oci_util:from_num(RowCount),
            io:format("          COUNT_SQL : rowcount ~s~n", [Total]),
            ?PROFILE(SelCountStmt, close, []),
            {ReadTime, _} = timer:tc(fun erloci_read_rows/2, [OciSession, Total]),
            io:format("!!!! ROWS_SQL ~p rows in ~p us~n", [Total, ReadTime]),
            ?PROFILE(OciSession, close, []),
            ?PROFILE(OciPort, close, []);
        Error -> error(Error)
    end.

erloci_read_rows(OciSession, Total) ->
    SelStmt = ?PROFILE(OciSession, prep_sql, [?ROWS_SQL]),
    ColDef = ?PROFILE(SelStmt, exec_stmt, []),
    io:format("          ROWS_SQL : columns ~p~n", [ColDef]),
    erloci_read_rows(SelStmt, false, Total, 0, 0, 0, 0).

erloci_read_rows(SelStmt, true, _, _, _, _, _) ->
    ?PROFILE(SelStmt, close, []);
erloci_read_rows(SelStmt, false, Total, OldRowCount, RowBytes, RowCount, FetchCalls) ->
    {{rows,Rows},More} = SelStmt:fetch_rows(1000),
    {OldRowCount1, RowBytes1, FetchCalls1} = 
    if RowCount - OldRowCount > 1000 ->
           io:format("ROWS_SQL read ~p of ~s, fetched bytes ~p in ~p trips~n",
                     [RowCount, Total, RowBytes, FetchCalls]),
           {RowCount, 0, 0};
       true ->
           {OldRowCount,
            RowBytes + byte_size(term_to_binary(Rows)),
            FetchCalls + 1}
    end,
    erloci_read_rows(SelStmt, More, Total, OldRowCount1,
                     RowBytes1,
                     RowCount + length(Rows),
                     FetchCalls1).

erloci_nif_test() ->
    case catch config() of
        {ok, [#{tns := TnsStr, user := UserStr, password := PasswordStr}]} ->
               E1 =  erloci_nif:ociEnvNlsCreate(0,0),
               {ok, CharsetId} = erloci_nif_drv:ociNlsCharSetNameToId(E1, <<"AL32UTF8">>),
               ok = erloci_nif:ociEnvHandleFree(E1),
               Envhp =  erloci_nif:ociEnvNlsCreate(CharsetId,CharsetId),
               ok = erloci_nif:ociAttrSet(Envhp, 'OCI_HTYPE_ENV', text, <<"GERMAN">>,'OCI_ATTR_ENV_NLS_LANGUAGE'),
               ok = erloci_nif:ociAttrSet(Envhp, 'OCI_HTYPE_ENV', text, <<"SWITZERLAND">>,'OCI_ATTR_ENV_NLS_TERRITORY'),
            Tns = list_to_binary(TnsStr),
            User = list_to_binary(UserStr),
            Password = list_to_binary(PasswordStr),

            {ok, Spoolhp} =  erloci_nif:ociSessionPoolCreate(Envhp, Tns,
                   2, 10, 1, User, Password),
            {ok, Authhp} =  erloci_nif:ociAuthHandleCreate(Envhp, User, Password),
            {ok, Svchp} =  erloci_nif:ociSessionGet(Envhp, Authhp, Spoolhp),
            {ok, Stmthp} = erloci_nif:ociStmtHandleCreate(Envhp),
            ok = ?PROFILE(erloci_nif, ociStmtPrepare, [Stmthp, list_to_binary(?COUNT_SQL)]),

            {ok, _X} = ?PROFILE(erloci_nif, ociStmtExecute, [Svchp, Stmthp, #{}, 0, 0, 'OCI_DEFAULT']),
            %% io:format("Stmt res = ~p\r\n",[X]),

            {ok, [[Total]], _} = ?PROFILE(erloci_nif, ociStmtFetch, [Stmthp, 1]),


            %% {{rows,[[RowCount]]},true} = ?PROFILE(SelCountStmt, fetch_rows, [10]), oci_util:from_num(T
            io:format("          COUNT_SQL : rowcount ~s~n", [Total]),

            ok = ?PROFILE(erloci_nif, ociStmtHandleFree, [Stmthp]),

            {ReadTime, _} = timer:tc(fun erloci_nif_read_rows/3, [Envhp, Svchp, Total]),
            io:format("!!!! ROWS_SQL ~p rows in ~p us~n", [Total, ReadTime]),

            ok = ?PROFILE(erloci_nif, ociSessionRelease, [Svchp]),
            ok = ?PROFILE(erloci_nif, ociSessionPoolDestroy, [Spoolhp]),
            ok = ?PROFILE(erloci_nif, ociEnvHandleFree, [Envhp]),
            ok = ?PROFILE(erloci_nif, ociTerminate, []);
            %%{ReadTime, _} = timer:tc(fun erloci_read_rows/2, [OciSession, Total]),
            %%io:format("!!!! ROWS_SQL ~p rows in ~p us~n", [Total, ReadTime]),

        Error -> error(Error)
    end.

erloci_nif_read_rows(Envhp, Svchp, Total) ->
    {ok, Stmthp} = erloci_nif:ociStmtHandleCreate(Envhp),
    ok = ?PROFILE(erloci_nif, ociStmtPrepare, [Stmthp, list_to_binary(?ROWS_SQL)]),
    {ok, ColDef} = ?PROFILE(erloci_nif, ociStmtExecute, [Svchp, Stmthp, #{}, 0, 0, 'OCI_DEFAULT']),
    io:format("          ROWS_SQL : columns ~p~n", [ColDef]),
    erloci_nif_read_rows(Stmthp, false, Total, 0, 0, 0, 0).

erloci_nif_read_rows(Stmthp, true, _, _, _, _, _) ->
    ok = ?PROFILE(erloci_nif, ociStmtHandleFree, [Stmthp]);
erloci_nif_read_rows(Stmthp, false, Total, OldRowCount, RowBytes, RowCount, FetchCalls) ->
    {ok, Rows, More} = ?PROFILE(erloci_nif, ociStmtFetch, [Stmthp, 1000]),

    {OldRowCount1, RowBytes1, FetchCalls1} = 
    if RowCount - OldRowCount > 1000 ->
           io:format("ROWS_SQL read ~p of ~s, fetched bytes ~p in ~p trips~n",
                     [RowCount, Total, RowBytes, FetchCalls]),
           {RowCount, 0, 0};
       true ->
           {OldRowCount,
            RowBytes + byte_size(term_to_binary(Rows)),
            FetchCalls + 1}
    end,
    erloci_nif_read_rows(Stmthp, More, Total, OldRowCount1,
                     RowBytes1,
                     RowCount + length(Rows),
                     FetchCalls1).

