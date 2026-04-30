-- Config docs: https://wezfurlong.org/wezterm/config/files.html
--        list: https://wezfurlong.org/wezterm/config/lua/config

local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.initial_cols = 180
config.initial_rows = 70
config.audible_bell = 'Disabled'
config.window_close_confirmation = 'NeverPrompt'

config.font = wezterm.font 'DejaVu Sans Mono'
config.font_size = 14

config.colors = {
  -- Based on the color palette for the vim-theme "Everforest"
  -- https://github.com/sainnhe/everforest/blob/master/palette.md
  --background = '#1e2326',
  --foreground = '#d3c6aa',
}

config.leader = { key = 'l', mods = 'CMD', timeout_milliseconds = 1000 }
local act = wezterm.action
local actions = {
  vsplit = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
  hsplit = act.SplitVertical { domain = 'CurrentPaneDomain' },
  next_pane = act.ActivatePaneDirection 'Left',
  prev_pane = act.ActivatePaneDirection 'Right',
  pane_index = act.ActivatePaneByIndex,
  tab_index = act.ActivateTab,
  open_link = act.OpenLinkAtMouseCursor,
}
config.keys = {
  -- vsplit
  { key = 't', mods = 'CTRL', action = actions.vsplit },
  { key = 't', mods = 'CMD', action = actions.vsplit },
  -- hsplit
  { key = 'T', mods = 'CTRL|SHIFT', action = actions.hsplit },
  { key = 'T', mods = 'CMD|SHIFT', action = actions.hsplit },

  -- next_pane
  --{ key = "o", mods = "CTRL", action = actions.next_pane },
  --{ key = "a", mods = "CMD",  action = actions.next_pane },
  -- prev_pane
  --{ key = "p", mods = "CTRL", action = actions.prev_pane },
  --{ key = "p", mods = "CMD",  action = actions.prev_pane },
  -- send CTRL
  -- { key = "l", mods = "CMD", action = wezterm.action.SendKey { key = "l", mods = "CTRL" } },
  -- { key = "c", mods = "CMD", action = wezterm.action.SendKey { key = "c", mods = "CTRL" } },
  -- { key = "d", mods = "CMD", action = wezterm.action.SendKey { key = "d", mods = "CTRL" } },
}

-- Map CMD+[n] to switch between panes
for i = 1, 8 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = 'CMD',
    action = actions.pane_index(i - 1),
  })
end

-- Map ALT+[n] to switch between tabs
for i = 1, 8 do
  -- https://wezterm.org/config/lua/keyassignment/ActivateTab.html
  -- ActivateTab now accepts negative numbers; these wrap around from the start of the tabs to the end, so -1 references the right-most tab, -2 the tab to its left and so on.
  -- ALT + number to activate that tab
  table.insert(config.keys, {
    key = tostring(i),
    mods = 'ALT',
    action = actions.tab_index(i - 1),
  })
end

return config
