-- Pull in the wezterm API
local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.color_scheme = 'nord'
config.font = wezterm.font 'Monaco'
config.font_size = 14

return config

