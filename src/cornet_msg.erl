-module(cornet_msg).
-export([parse/1, create/1]).

-behavior(msg_factory).

-include("msg_type.hrl").

-define(CorNet_TS_Lite, 0).

-define(DSP_TEXT_CMD,   2#00010000).
-define(LED_SET_CMD,    2#00100000).
-define(AU_CONTROL_CMD, 2#00110000).
-define(AU_VOLUME_CMD,  2#00110001).

-define(KBD_DOWN_IND, 2#10010000).
-define(KBD_UP_IND,   2#10010001).


parse(<<?DSP_TEXT_CMD, Dn, Row, Col, Len, Attr, Text/binary>>) ->
    #dsp_text_cmd{display_number=Dn, row=Row, column=Col, 
                  length=Len, attributes=Attr, text=Text};

parse(<<?LED_SET_CMD, Num, Val>>) when Val =< 7 ->
    #led_set_cmd{led_num=Num, value=Val};

parse(<<?AU_CONTROL_CMD, StrId, In, Out>>) when In =< 8 ->
    #au_control_cmd{stream_id=StrId, input_device=In, output_device=Out};

parse(<<?AU_VOLUME_CMD, Out, Volume>>) when Out =< 8 ->
    #au_volume_cmd{output_device=Out, volume=Volume};

parse(<<?KBD_DOWN_IND, Key>>) ->
    #kbd_down_ind{key=Key};

parse(<<?KBD_UP_IND, Key>>) ->
    #kbd_down_ind{key=Key}.

create({dsp_text_cmd, DisplayNumber, Row, Col, Len, Attr, Text}) ->
    <<?DSP_TEXT_CMD, DisplayNumber, Row, Col, Len, Attr, Text/binary>>;

create({led_set_cmd, LedNum, Val}) when Val =< 7 ->
    <<?LED_SET_CMD, LedNum, Val>>;

create({au_control_cmd, StreamId, In, Out}) when In =< 8 ->
    <<?AU_CONTROL_CMD, StreamId, In, Out>>;

create({au_volume_cmd, Out, Vol}) when Out =< 8 ->
    <<?AU_VOLUME_CMD, Out, Vol>>;

create({kbd_down_ind, Key}) ->
    <<?KBD_DOWN_IND, Key>>;

create({kbd_up_ind, Key}) ->
    <<?KBD_UP_IND, Key>>.
