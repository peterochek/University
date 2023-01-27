#!/bin/bash

ps aux --sort start_time | tail -n5 | head -n1 | awk '{print $2}'
#tail -n5 <-> 4 cmds pipelined