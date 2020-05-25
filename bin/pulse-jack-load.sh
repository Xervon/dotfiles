#!/usr/bin/env bash

{
pactl load-module module-jack-sink   channels=2 connect=0 sink_name="out_music"     client_name="out_music"
pactl load-module module-jack-sink   channels=2 connect=0 sink_name="out_web"       client_name="out_web"
pactl load-module module-jack-sink   channels=2 connect=0 sink_name="out_game"      client_name="out_game"
pactl load-module module-jack-sink   channels=1 connect=0 sink_name="out_other"     client_name="out_other"
pactl load-module module-jack-sink   channels=1 connect=0 sink_name="out_voice"     client_name="out_voice"
pactl load-module module-jack-sink   channels=1 connect=0 sink_name="out_voice_rtn" client_name="out_voice_rtn"
pactl load-module module-jack-sink   channels=1 connect=0 sink_name="out_game_rtn"  client_name="out_game_rtn"

pactl load-module module-jack-source channels=1 connect=0 source_name="in_game"     client_name="in_game"
pactl load-module module-jack-source channels=1 connect=0 source_name="in_voice"    client_name="in_voice"
pactl load-module module-jack-source channels=2 connect=0 source_name="in_gtr_raw"  client_name="in_gtr_raw"
pactl load-module module-jack-source channels=2 connect=0 source_name="in_gtr_amp"  client_name="in_gtr_amp"

pactl set-default-sink "out_other"
pactl set-default-source "in_voice"
} >/dev/null
