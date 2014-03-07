-module(lcp_msg).
-export([parse/1, create/1]).

-behavior(msg_factory).

-include("msg_type.hrl").

-define(REQUEST,    2#00).
-define(RESPONSE,   2#01).
-define(COMMAND,    2#10).
-define(INDICATION, 2#11).

-define(DEVICE_INIT_REQ,    2#00000001).
-define(RTP_PORT_ALLOC_CMD, 2#00100010).

-define(DEVICE_INIT_RESP,    2#01000001).
-define(RTP_PORT_ALLOC_RESP, 2#01100010).

-define(KEEP_ALV_SET_CMD,     2#10000010).
-define(RTP_OPEN_CMD,         2#10100000).
-define(RTP_CLOSE_CMD,        2#10100001).
-define(RTP_PORT_RELEASE_CMD, 2#10100011).
-define(DATAGRAM_CMD,         2#10110000).

-define(TRANSPORT_COOKIE_IND, 2#11000000).
-define(DEVICE_RELEASE_IND,   2#11000001).
-define(KEEP_ALV_IND,         2#11000010).
-define(DATAGRAM_IND,         2#11110000).

-define(CorNet_TS_Lite, 0).


get_protocol(<<?CorNet_TS_Lite>>) -> cornet_msg.

set_protocol(cornet_msg) -> <<?CorNet_TS_Lite>>.

%Requests
parse(<<?DEVICE_INIT_REQ>>) -> device_init_req;

parse(<<?RTP_PORT_ALLOC_CMD>>) -> rtp_port_alloc_cmd;

%Responses
parse(<<?DEVICE_INIT_RESP, PhoneId/binary>>) ->
    #device_init_resp{phone_id=PhoneId};

parse(<<?RTP_PORT_ALLOC_RESP>>) -> rtp_port_alloc_resp;

%Commands
parse(<<?KEEP_ALV_SET_CMD, Interval, BackFreq>>) ->
    #keep_alv_set_cmd{interval=Interval, back_freq=BackFreq};

parse(<<?RTP_OPEN_CMD, StreamId, Codec, Payload, 
                 LocalPort:16, RemotePort:16, RemoteIp:32>>) ->
    <<IP/bitstring>> = RemoteIp, %TODO: parse IP address
    #rtp_open_cmd{stream_id=StreamId, codec_number=Codec, payload_type=Payload,
                 local_port=LocalPort, remote_port=RemotePort, remote_ip=IP};

parse(<<?RTP_CLOSE_CMD, StreamId>>) ->
    #rtp_close_cmd{stream_id=StreamId};

parse(<<?RTP_PORT_RELEASE_CMD>>) -> rtp_port_release_cmd;

parse(<<?DATAGRAM_CMD, Protocol, Msg/binary>>) ->
    Proto = get_protocol(Protocol),
    ParsedMsg = Proto:parse(Msg),
    #datagram_cmd{protocol=Proto, msg=ParsedMsg};

%Indications
parse(<<?TRANSPORT_COOKIE_IND, Cookie/binary>>) ->
    #transport_cookie_ind{cookie=Cookie};

parse(<<?DEVICE_RELEASE_IND>>) -> device_release_ind;

parse(<<?KEEP_ALV_IND>>) -> keep_alv_ind;

parse(<<?DATAGRAM_IND, Protocol, Msg/binary>>) ->
    Proto = get_protocol(Protocol),
    ParsedMsg = Proto:parse(Msg),
    #datagram_ind{protocol=Proto, msg=ParsedMsg}.

%requests
create(device_init_req) -> <<?DEVICE_INIT_REQ>>;

create(rtp_port_alloc_cmd) -> <<?RTP_PORT_ALLOC_CMD>>;

%responses
create(#device_init_resp{phone_id=PhoneId}) ->
    <<?DEVICE_INIT_RESP, PhoneId/binary>>;

create(rtp_port_alloc_resp) -> <<?RTP_PORT_ALLOC_RESP>>;

%commands
create(#keep_alv_set_cmd{interval=Interval, back_freq=BackFreq}) ->
    <<?KEEP_ALV_SET_CMD, Interval, BackFreq>>;

create(#rtp_open_cmd{stream_id=StreamId, codec_number=Codec, payload_type=Payload,
                 local_port=LocalPort, remote_port=RemotePort, remote_ip=IP}) ->
    <<RemoteIp/binary>> = IP, %TODO: parse IP address
    <<?RTP_OPEN_CMD, StreamId, Codec, Payload, LocalPort:16, RemotePort:16, RemoteIp:32>>;

create(#rtp_close_cmd{stream_id=StreamId}) -> <<?RTP_CLOSE_CMD, StreamId>>;

create(rtp_port_release_cmd) -> <<?RTP_PORT_RELEASE_CMD>>;

create(#datagram_cmd{protocol=Proto, msg=ParsedMsg}) ->
    Msg = Proto:create(ParsedMsg),
    Protocol = set_protocol(Proto),
    <<?DATAGRAM_CMD, Protocol, Msg/binary>>;

%indications
create({transport_cookie_ind, Cookie}) ->
    <<?TRANSPORT_COOKIE_IND, Cookie/binary>>;

create(device_release_ind) -> <<?DEVICE_RELEASE_IND>>;

create(keep_alv_ind) -> <<?KEEP_ALV_IND>>;

create(#datagram_ind{protocol=Proto, msg=ParsedMsg}) ->
    Msg = Proto:create(ParsedMsg),
    Protocol = set_protocol(Proto),
    <<?DATAGRAM_IND, Protocol, Msg/binary>>.
