%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <geoff@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Email MIME encoding library.
%% @end
%%%-------------------------------------------------------------------
-module(esmtp_mime).

-include_lib("esmtp_mime.hrl").

%% API
-export([
    encode/1,
    encode_no_bcc/1,
    send/5,
    msg/0, msg/3, msg/4,
    from/1, from/2,
    to/1, to/2,
    cc/1, cc/2,
    bcc/1, bcc/2,
    subject/1, subject/2,
    add_text_part/2,
    add_attachment_part/3
]).

-export([
    test_msg/0,
    test_img_msg/1,
    send_test/4,
    test/0
 ]).

%%====================================================================
%% API
%%====================================================================

% From = {Name, Address}
% To = [{Name, Address}]
msg(From, To, Subject) ->
    Msg0 = from(msg(), From),
    Msg1 = to(Msg0, To),
    subject(Msg1, Subject)
.

msg(From, To, Subject, Body) ->
    Msg = msg(From, To, Subject),
    add_text_part(Msg, Body)
.

msg() ->
    #mime_msg{
        boundary=invent_mime_boundary(),
        headers=[{"Date", httpd_util:rfc1123_date()}]
    }
.

encode(Msg) ->
    encode_headers(headers(Msg)) ++
    "\r\n\r\n" ++
    encode_parts(Msg) ++
    "--" ++ Msg#mime_msg.boundary ++
    "--\r\n"
.

encode_no_bcc(#mime_msg{headers=H}=Msg) ->
    NoBccHeaders = proplists:delete("Bcc", H),
    encode_headers(headers(Msg#mime_msg{headers=NoBccHeaders})) ++
    "\r\n\r\n" ++
    encode_parts(Msg) ++
    "--" ++ Msg#mime_msg.boundary ++
    "--\r\n"
.

to(Msg, To) ->
    replace_header(Msg, "To", To)
.

to(#mime_msg{headers=H}) ->
    proplists:get_value("To", H, [])
.

cc(Msg, Cc) ->
    replace_header(Msg, "Cc", Cc)
.

cc(#mime_msg{headers=H}) ->
    proplists:get_value("Cc", H, [])
.

bcc(Msg, Bcc) ->
    replace_header(Msg, "Bcc", Bcc)
.

bcc(#mime_msg{headers=H}) ->
    proplists:get_value("Bcc", H, [])
.

from(Msg, From) ->
    replace_header(Msg, "From", From)
.

from(#mime_msg{headers=H}) ->
    proplists:get_value("From", H, undefined)
.

subject(Msg, Subject) ->
    replace_header(Msg, "Subject", Subject)
.

subject(#mime_msg{headers=H}) ->
    proplists:get_value("Subject", H, undefined)
.

% DateTime = {{YYYY,MM,DD}, {Hour,Min,Sec}}
date(Msg, DateTime) ->
    Date = httpd_util:rfc1123_date(DateTime),
    replace_header(Msg, "Date", Date)
.

add_text_part(Msg = #mime_msg{parts=Parts}, Text) ->
    Msg#mime_msg{parts=Parts ++ [#mime_part{data=Text}]}
.

add_attachment_part(#mime_msg{parts=Parts}=Msg, Filename, FileData) when
    is_binary(FileData)
->
    MimePart = #mime_part{
        data=binary_to_list(base64:encode(FileData)),
        type=attachment,
        encoding={"base64", "application/octet-stream", "iso-8859-1"},
        name=Filename
    },
    Msg#mime_msg{parts=Parts ++ [MimePart]}
.

%%====================================================================
%% Internal functions
%%====================================================================

replace_header(#mime_msg{headers=H}=Msg, Key, Value) ->
    Msg#mime_msg{headers=[{Key, Value} | proplists:delete(Key, H)]}
.

test_msg() ->
    #mime_msg{boundary=invent_mime_boundary(),
              headers=[{"To", ["João <joao@spotify.com>"]},
                       {"Subject", "dam attachment"},
                       {"From", "zun bid <zunbid@gmail.com>"},
                       {"Date", httpd_util:rfc1123_date()}
                      ],
              parts=[#mime_part{data="This is a test..."},
                     #mime_part{data="This,is,a,test\r\nof,something,ok,maybe",
                                type=attachment,
                                encoding={"7bit","text/plain","iso-8859-1"},
                                name="foo.csv"}]}.

test_img_msg(Filename) ->
    {ok, Data} = file:read_file(Filename),
    #mime_msg{
        boundary=invent_mime_boundary(),
        headers=[
            {"To", ["joao <joao@spotify.com>"]},
            {"Subject", "A bin file"},
            {"From", "Zunbid <zunbid@gmail.com>"},
            {"Date", httpd_util:rfc1123_date()}
        ],
        parts=[
            #mime_part{data="This is a test..."},
            #mime_part{
                data=base64:encode_to_string(Data),
                type=attachment,
                encoding={
                    "base64",
                    "application/octet-stream",
                    "iso-8859-1"
                },
                name=Filename
            }
        ]
    }
.

test() ->
    io:format("~s~n", [encode(test_msg())]).

send(Ip, Host, From, To, Msg=#mime_msg{}) ->
    ok = smtpc:sendmail(Ip, Host, From, To, encode(Msg)).

send_test(Ip, Host, From, To) ->
    send(Ip, Host, From, To, test_msg()).

encode_address({Name, Address}) ->
    "=?UTF-8?B?"++base64:encode_to_string(Name)++"?= "++Address
.

encode_subject(Subject) ->
    "=?UTF-8?B?"++base64:encode_to_string(Subject)++"?="
.

encode_header({"Subject", Subject}) ->
    Value = encode_subject(Subject),
    "Subject" ++ ": " ++ Value
;
encode_header({"From", NameAddress}) ->
    Value = encode_address(NameAddress),
    "From" ++ ": " ++ Value
;
encode_header({"To", NameAddressList}) ->
    List = [encode_address(NameAddress) || NameAddress <- NameAddressList],
    "To" ++ ": " ++ join(List, ", ")
;
encode_header({"Cc", NameAddressList}) ->
    List = [encode_address(NameAddress) || NameAddress <- NameAddressList],
    "Cc" ++ ": " ++ join(List, ", ")
;
encode_header({"Bcc", NameAddressList}) ->
    List = [encode_address(NameAddress) || NameAddress <- NameAddressList],
    "Bcc" ++ ": " ++ join(List, ", ")
;
encode_header({Header, [V|Vs]}) when is_list(V) ->
    Hdr = lists:map(
        fun ({K, Value}) when is_list(K), is_list(Value) ->
                K ++ "=" ++ Value
            ;
            ({K, Value}) when is_atom(K), is_list(Value) ->
                atom_to_list(K) ++ "=" ++ Value
            ;
            (Value) when is_list(Value) ->
                Value
        end,
        [V | Vs]
    ),
    Header ++ ": " ++ join(Hdr, "; ")
;
encode_header({Header, Value}) when is_list(Header), is_list(Value) ->
    Header ++ ": " ++ Value
;
encode_header({Header, Value}) when is_atom(Header), is_list(Value) ->
    atom_to_list(Header) ++ ": " ++ Value
.

encode_headers(PropList) ->
    join(lists:map(fun encode_header/1, PropList), "\r\n")
.

encode_parts(#mime_msg{parts=Parts, boundary=Boundary}) ->
    lists:map(fun (P) -> encode_part(P,Boundary) end, Parts).

encode_part(#mime_part{data=Data} = P, Boundary) ->
    "--" ++ Boundary ++ "\r\n" ++
    encode_headers(part_headers(P)) ++ "\r\n\r\n" ++
    Data ++ "\r\n".

part_headers(
    #mime_part{
        type=undefined,
        encoding={Enc, MimeType, Charset},
        name=undefined
    }
) ->
    [
        {"Content-Transfer-Encoding", Enc},
        {"Content-Type", [MimeType, {charset, Charset}]}
    ]
;
part_headers(
    #mime_part{
        type=Type,
        encoding={Enc, MimeType, _Charset},
        name=Name
    }
) when Type == attachment ->
    [
        {"Content-Transfer-Encoding", Enc},
        {"Content-Type", [MimeType, "name=" ++ Name]},
        {"Content-Disposition", [atom_to_list(Type), {"filename", Name}]}
    ]
;
part_headers(
    #mime_part{type=Type,
    encoding={Enc, MimeType, Charset},
    name=Name}
) when Type == inline ->
    [
        {"Content-Transfer-Encoding", Enc},
        {"Content-Type", [MimeType, "charset=" ++ Charset, "name=" ++ Name]},
        {"Content-Disposition", [atom_to_list(Type), {"filename", Name}]}
    ]
.

headers(#mime_msg{headers=H, boundary=Boundary}) ->
    [{"MIME-Version", "1.0"}] ++
    H ++
    [{"Content-Type", ["multipart/mixed", "boundary=" ++ Boundary]}]
.

invent_mime_boundary() ->
    string:copies("0", 10) ++ list_rand(boundary_chars(), 30)
.

list_rand(List, N) ->
    lists:map(
        fun (_) -> list_rand(List) end,
        lists:seq(1,N)
    )
.

list_rand(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)), List)
.

boundary_chars() ->
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789"
%    "'()+_,-./=?"
.

join([H1, H2| T], S) when is_list(H1), is_list(H2), is_list(S) ->
    H1 ++ S ++ join([H2| T], S)
;
join([H], _) ->
    H
;
join([], _) ->
    []
.
