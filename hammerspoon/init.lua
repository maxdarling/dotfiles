-- SPOONS
-- hs.loadSpoon("ReloadConfiguration") -- broken!?
-- spoon.ReloadConfiguration:start()

hs.loadSpoon("ClipboardTool")
spoon.ClipboardTool:start()
spoon.ClipboardTool.show_copied_alert = false
spoon.ClipboardTool.paste_on_select = true -- todo: try

-- WINDOW MANAGEMENT
function expandWindowLeft() -- todo: parameterize L/R
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end

function expandWindowRight()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end

-- KEYBOARD
-- F keys for common apps (more ergo than key-chord). this is Xah Lee floating app philosophy.
-- 5/31/23: optimized for the glove (F4 and F7 are by far the easiest to reach).
hs.hotkey.bind({}, 'F3', function() hs.application.open('Slack') end)
hs.hotkey.bind({}, 'F4', function() hs.application.open('Google Chrome') end)
hs.hotkey.bind('shift', 'F4', function() hs.application.open('Spotify') end)
hs.hotkey.bind({}, 'F6', function() hs.application.open('Emacs') end)
hs.hotkey.bind({}, 'F7', function() hs.application.open('iTerm') end)
hs.hotkey.bind('shift', 'F7', function() hs.application.open('Visual Studio Code') end)
hs.hotkey.bind({}, 'F8', function() hs.application.open('IntelliJ IDEA CE') end)

-- strat: use a single key (e.g. F17) for hyper mode (instead of the traditional 4 mod keys)
-- so that in case I remove hyper I have a useful key (F17) to rebind instead of unusable mods.
hyper = hs.hotkey.modal.new({}, 'F20') -- throwaway key
hs.hotkey.bind(
  {},
  'F17', -- this is the hyper key
  function() hyper:enter() end,
  function() hyper:exit() end
)

  local hyperMappings = {
    -- utils
    { 'p', function() spoon.ClipboardTool:showClipboard() end },
    { 'Left', function() expandWindowLeft() end },
    { 'Right', function() expandWindowRight() end },
    -- debug
    {'q', function() hs.alert'Pressed q in hypermode' end},
  }

  for i, mapping in ipairs(hyperMappings) do
    local key = mapping[1]
    local app = mapping[2]
    hyper:bind({}, key, function()
      if (type(app) == 'function') then
        app()
      else
        hs.logger.new('hyper'):e('Invalid mapping for Hyper +', key)
      end
    end)
  end
