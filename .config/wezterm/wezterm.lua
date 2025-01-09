-- Config docs: https://wezfurlong.org/wezterm/config/files.html
--        list: https://wezfurlong.org/wezterm/config/lua/config

local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.initial_cols = 180
config.initial_rows = 70
config.audible_bell = "Disabled"

config.font = wezterm.font 'DejaVu Sans Mono'
config.font_size = 14

config.colors = {
  -- Based on the color palette for the vim-theme "Everforest"
  -- https://github.com/sainnhe/everforest/blob/master/palette.md
  background = "#1e2326",
  foreground = "#d3c6aa",
}

config.leader = { key = "l", mods = "CMD", timeout_milliseconds = 1000 }
local actions = {
  vsplit = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  hsplit = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
  npane = wezterm.action.ActivatePaneDirection 'Left',
  ppane = wezterm.action.ActivatePaneDirection 'Right',
}
config.keys = {
  -- -- vsplit
  -- { key = "t", mods = "CTRL", action = actions.vsplit },
  -- { key = "t", mods = "CMD",  action = actions.vsplit },
  -- -- hsplit
  -- { key = "T", mods = "CTRL|SHIFT", action = actions.hsplit },
  -- { key = "T", mods = "CMD|SHIFT",  action = actions.hsplit },
  -- -- npane
  -- { key = "o", mods = "CTRL", action = actions.npane },
  -- { key = "o", mods = "CMD",  action = actions.npane },
  -- -- ppane
  -- { key = "p", mods = "CTRL", action = actions.ppane },
  -- { key = "p", mods = "CMD",  action = actions.ppane },
  -- -- send CTRL
  -- { key = "l", mods = "CMD", action = wezterm.action.SendKey { key = "l", mods = "CTRL" } },
  -- { key = "c", mods = "CMD", action = wezterm.action.SendKey { key = "c", mods = "CTRL" } },
  -- { key = "d", mods = "CMD", action = wezterm.action.SendKey { key = "d", mods = "CTRL" } },
}

return config
