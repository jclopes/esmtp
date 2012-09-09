%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @author JC Lopes <jc.lopes@gmail.com>
%% @version {@vsn}, {@date} {@time}
%% @doc ESMTP api module
%% @end
%%%-------------------------------------------------------------------
-module(esmtp).

-include("../include/esmtp_mime.hrl").

%% API
-export([
    conn/3,
    send/2,
    send/5
]).

-define(SMTP_PORT_TLS, 587).
-define(SMTP_PORT_SSL, 465).

%%====================================================================
%% API
%%====================================================================

% @doc Setup a SMTP connection object.
% This object is passed to the send command.
% Arguments:
%  ClientFQDN: this should be a FQDN of the client. RFC 2821.
%  {Host, Port}: SMTP server Hostname and SMTP server port
%  Authentication: this will be {user, pass} or XOAUTH token
% @spec conn(string(), {string(), integer()}, term()) -> SmtpConn::term()
conn(ClientFQDN, {Host, Port}, Authentication) when is_integer(Port) ->
    Ehlo = ClientFQDN,
    case need_ssl(Port) of
        true -> {Host, Port, ssl, Ehlo, Authentication};
        false -> {Host, Port, gen_tcp, Ehlo, Authentication}
    end
.

% @TODO: send a sparate message to each Bcc recipient
send(SmtpConn, #mime_msg{}=Msg) ->
    F = fun(ComposedAddress) ->
        {ok, _Name, Addr} = parse_address(ComposedAddress),
        Addr
    end,
    FromAddress = esmtp_mime:from(Msg),
    RcptTo = [F(Addr) || Addr <- esmtp_mime:to(Msg)],
    RcptCc = [F(Addr) || Addr <- esmtp_mime:cc(Msg)],
    RcptBcc = [F(Addr) || Addr <- esmtp_mime:bcc(Msg)],
    Rcpt = RcptTo++RcptCc++RcptBcc,
    case Rcpt of
        [] ->
            {error, "Missing destination address."}
        ;
        [_H | _T] ->
            esmtp_client:send(
                SmtpConn,
                FromAddress,
                Rcpt,
                esmtp_mime:encode_no_bcc(Msg)
            )
    end
.

% @spec send(SmtpConn, From::unicode_binary(), To::[unicode_binary()], Subject::unicode_binary(), Body::string()) -> term()
send(SmtpConn, From, To, Subject, Body) ->
    {ok, FromName, FromAddress} = parse_address(From),
    ToList = [
        begin
            {ok, Name, Addr} = parse_address(T),
            {Name, Addr}
        end
    ||
        T <- To
    ],
    ToAddresses = [Addr||{_, Addr} <- ToList],
    Message = assembly_msg({FromName, FromAddress}, ToList, Subject, Body),
    esmtp_client:send(SmtpConn, FromAddress, ToAddresses, Message)
.

%%====================================================================
%% Internal functions
%%====================================================================

assembly_msg({FromName, FromAddress}, ToList, Subject, Body) ->
    % TODO: support other encoding
    EncFrom = "=?UTF-8?B?"++base64:encode_to_string(FromName)++"?= "++FromAddress,
    EncTo = lists:foldl(
        fun({Name, Addr}, Acc) ->
            Acc++"=?UTF-8?B?"++
            base64:encode_to_string(Name)++
            "?= "++Addr++", "
        end,
        [],
        ToList
    ),
    EncSubject = "=?UTF-8?B?"++base64:encode_to_string(Subject)++"?=",
    "Subject: "++EncSubject++"\r\n"++
    "From: "++EncFrom++"\r\n"++
    "To: "++EncTo++"\r\n"++
    "\r\n"++
    Body
.

need_ssl(?SMTP_PORT_TLS) -> true;
need_ssl(?SMTP_PORT_SSL) -> true;
need_ssl(_) -> false.

% @doc Parses an email adress into a pair of {Name, Address}
% accepts email in the form of "Name &lt;user@domain&gt;" or "&lt;user@domain&gt;".
% NOTE: This function does not validate an email address
% @spec parse_address(ComposedAddress) -> {Name::unicode_binary(), Address::string()}
parse_address(ComposedAddress) ->
    RegExp = "(?<NAME>.*)(?<ADDR><[^<>]*>\\h*$)", % must contain '<' and '>'.
    Ops = [unicode, {capture, ['NAME', 'ADDR'], binary}],
    case re:run(ComposedAddress, RegExp, Ops) of
        {match, [Name, Address]} -> {ok, Name, binary_to_list(Address)};
        _ -> {error, invalid_mail_address}
    end
.
