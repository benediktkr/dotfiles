-- Pull in the wezterm API
local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- config.color_scheme = 'nord'
-- config.color_scheme = 'Everforest Dark'
-- config.color_scheme = 'Everforest Light'
-- config.color_scheme = 'Everblush'
-- config.color_scheme = 'Jup'
config.color_scheme = 'Breeze'
config.font = wezterm.font 'Monaco'
config.font_size = 14

return config

