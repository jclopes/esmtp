%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ESMTP api module
%% @end
%%%-------------------------------------------------------------------
-module(esmtp).

-include("../include/esmtp_mime.hrl").

%% API
-export([start/0
         ,send/1
         ,send/2
         ,send/3
         ,send/6
         ,mailq/0]).

start() ->
    application:start(esmtp).

%%====================================================================
%% API
%%====================================================================

send(Msg= #mime_msg{}) ->
    send(esmtp_mime:from(Msg),
         esmtp_mime:to(Msg),
         esmtp_mime:encode(Msg)).

send(To, Msg) ->
    send(undefined, To, Msg).

send(undefined, To, Msg) ->
    From = esmtp_app:config(default_from),
    send(From, To, Msg);
send(From, To, Message) ->
    {Host, Port} = esmtp_app:config(smarthost),
    MX = case esmtp_app:need_ssl(Port) of
             true -> {Host, Port, ssl, esmtp_app:config(login)};
             false -> {Host, Port, gen_tcp, no_login}
         end,
    Ehlo = esmtp_app:config(default_ehlo),
    send0(MX, Ehlo, From, To, Message).

% @spec send(SmtpServ, Authentication, From::string(), To::string(), Subject::string(), Body::string())
send({Host, Port}, Authentication, From, To, Subject, Body) ->
    Message = assembly_msg(To, Subject, Body),
    MX = case esmtp_app:need_ssl(Port) of
             true -> {Host, Port, ssl, Authentication};
             false -> {Host, Port, gen_tcp, no_login}
         end,
    Ehlo = esmtp_app:config(default_ehlo),
    send0(MX, Ehlo, From, To, Message)
.

mailq() ->
    supervisor:which_children(esmtp_sup).

%%====================================================================
%% Internal functions
%%====================================================================

send0(MX, Ehlo, From, To, Msg) ->
    esmtp_client:send(MX, Ehlo, From, To, Msg)
.

assembly_msg(To, Subject, Body) ->
    "To: "++To++"\r\n" ++
    "Subject: "++Subject++"\r\n" ++
    "\r\n" ++
    Body
.

