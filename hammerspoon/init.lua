------------
-- SPOONS --
------------
-- hs.loadSpoon('ReloadConfiguration') -- broken!?
-- spoon.ReloadConfiguration:start()

hs.loadSpoon('ClipboardTool')
spoon.ClipboardTool:start()
spoon.ClipboardTool.show_copied_alert = false
spoon.ClipboardTool.paste_on_select = true -- todo: try


--------------
-- KEYBOARD --
--------------
-- virtual layer (so that we can disable hotkeys later)
hotkeys = hs.hotkey.modal.new({}, 'F20')

-- hotkeys:bind('shift', 'F3', function() hs.application.open('zoom.us') end)
-- hotkeys:bind({}, 'F3', function() hs.application.open('Slack') end)
hotkeys:bind({}, 'F4', function() hs.application.open('Google Chrome') end)
hotkeys:bind('shift', 'F4', function() hs.application.open('Spotify') end)
-- F5 = whispr key

hotkeys:bind({}, 'F6', function() hs.application.open('Cursor') end)
hotkeys:bind({}, 'F7', function() hs.application.open('iTerm') end)
hotkeys:bind('shift', 'F7', function() hs.application.open('Emacs') end)
-- hotkeys:bind({}, 'F8', function() hs.application.open('IntelliJ IDEA CE') end)

-- ignore certain apps. source: https://github.com/Hammerspoon/hammerspoon/issues/2081#issuecomment-668283868
ignoredApps = {["RuneLite"] = true, ["League Of Legends"] = true}
function disablerCallback(appName, eventType, appObject)
  if (ignoredApps[appName]) then
    if (eventType == hs.application.watcher.activated) then
      hotkeys:exit()
    elseif (eventType == hs.application.watcher.deactivated) then
      hotkeys:enter()
    end
  end
end
watcher = hs.application.watcher.new(disablerCallback)
watcher:start()
hotkeys:enter()

-- HYPER
-- note: I map F17 as my hyper modifier on my keyboard layout, as opposed to the traditional cmd+shift+alt+ctrl).
hyper = hs.hotkey.modal.new({'shift'}, 'F20') -- virtual layer for hyper, similar to above 
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


-----------------------
-- WINDOW MANAGEMENT --
-----------------------
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
