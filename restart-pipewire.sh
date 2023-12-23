#!/bin/bash
systemctl --user restart pipewire.service
systemctl --user restart pipewire-pulse.service
# systemctl --user restart pipewire-media-session.service
