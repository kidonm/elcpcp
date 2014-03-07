%LCPCP
-record(transport_cookie_ind, {cookie}).
-record(device_init_resp, {phone_id}).
-record(keep_alv_set_cmd, {interval, back_freq}).
-record(rtp_open_cmd, {stream_id, codec_number, payload_type, 
                      local_port, remote_port, remote_ip}).
-record(rtp_close_cmd, {stream_id}).
-record(datagram_cmd, {protocol, msg}).
-record(datagram_ind, {protocol, msg}).

%CorNet
-record(dsp_text_cmd, {display_number, row, column, length, attributes, text}).
-record(led_set_cmd, {led_num, value}).
-record(au_control_cmd, {stream_id, input_device, output_device}).
-record(au_volume_cmd, {output_device, volume}).
-record(kbd_down_ind, {key}).
-record(kbd_down_up, {key}).
