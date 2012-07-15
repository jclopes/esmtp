%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Simple one-shot client using esmtp_sock.
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_client).

%% API
-export([send/4]).

%%====================================================================
%% API
%%====================================================================

% TODO: this is over simplifyed
% need to return some error messages
%  - fail to authenticate
%  - bad server address
send({Host, Port, SocketType, Ehlo, Authentication}, From, ToList, Msg) ->
    {ok, S0} = esmtp_sock:connect(Host, Port, SocketType),
    {ok, S1, {220, _, _Banner}} = esmtp_sock:read_response_all(S0),
    {ok, S2, {250, _, _Msg}} = esmtp_sock:command(S1, {ehlo, Ehlo}),
    AuthS = case Authentication of
        {xoauth, XOAuthToken} ->
            {ok, S3, {235, _, _}} = esmtp_sock:command(S2, {auth_xoauth, XOAuthToken}),
            S3;
        {User,Pass} ->
            {ok, S3, {334, _, _}} = esmtp_sock:command(S2, {auth, "PLAIN"}),
            {ok, S4, {235, _, _}} = esmtp_sock:command(S3, {auth_plain, User, Pass}),
            S4;
        no_login ->
            S2
    end,
    {ok, S10, {250, _, _}} = esmtp_sock:command(AuthS, {mail_from, From}),
    S11 = lists:foldl(
        fun(To, Acc) ->
            {ok, NewAcc, {250, _, _}} = esmtp_sock:command(Acc, {rcpt_to, To}),
            NewAcc
        end,
        S10,
        ToList
    ),
    {ok, S12, {250, _, _}} = esmtp_sock:send_data(S11, Msg),
    esmtp_sock:close(S12)
.

%%====================================================================
%% Internal functions
%%====================================================================
