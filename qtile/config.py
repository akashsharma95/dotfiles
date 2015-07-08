# -*- coding: utf-8 -*-
from libqtile.bar import Bar
from libqtile.widget import Spacer
from libqtile import bar, hook, layout, widget
from libqtile.command import lazy
from libqtile.config import Click, Drag, Group, Key, Screen
import subprocess

wmname = 'qtile'
mod = 'mod4'
alt = 'mod1'
term = "gnome-terminal"
lock = "i3lock -efb -c333333"

# Key bindings
keys = [
    # Window manager controls
    Key([mod, 'control'], 'r', lazy.restart()),
    Key([mod, 'control'], 'q', lazy.shutdown()),
    Key([mod], 'Return', lazy.spawn('gnome-terminal')),
    Key([mod], 'w',      lazy.window.kill()),
    Key([alt], 'F2',      lazy.spawncmd()),

    Key([mod], "Tab", lazy.group.next_window()),
    Key([mod], 'Left', lazy.screen.prev_group()),
    Key([mod], 'Right', lazy.screen.next_group()),

    # Layout modification
    Key([mod, 'control'], 'space', lazy.window.toggle_floating()),

    # Switch between windows in current stack pane
    Key([mod, alt], "Up", lazy.layout.down()),
    Key([mod, alt], "Down", lazy.layout.up()),
    Key([mod, alt], "Left", lazy.layout.left()),
    Key([mod, alt], "Right", lazy.layout.right()),

    # Move windows up or down in current stack
    Key([mod, 'control'], "Left", lazy.layout.swap_left()),
    Key([mod, 'control'], "Right", lazy.layout.swap_right()),
    Key([mod, 'control'], "Up", lazy.layout.shuffle_down()),
    Key([mod, 'control'], "Down", lazy.layout.shuffle_up()),

    # Switch window focus to other pane(s) of stack
    Key([mod], 'space', lazy.layout.next()),

    # Toggle between different layouts as defined below
    Key([mod, alt], "space", lazy.next_layout()),
    # Lock
    Key([mod], "l", lazy.spawn(lock)),

    # Additional Bindings
    Key([mod], "i", lazy.layout.grow()),
    Key([mod], "m", lazy.layout.shrink()),
    Key([mod], "n", lazy.layout.normalize()),
    Key([mod], "o", lazy.layout.maximize()),
    Key([mod, 'shift'], "space", lazy.layout.flip()),

    # Some Launchers
    Key([mod, alt], "t", lazy.spawn("termite -e tmux")),
    Key([mod, alt], "f", lazy.spawn("firefox")),
    Key([mod, alt], "n", lazy.spawn("nautilus")),
    Key([mod, alt], "m", lazy.spawn("audacious")),
    Key([mod, alt], "p", lazy.spawn("gnome-mplayer")),
    Key([mod, alt], "e", lazy.spawn("evolution")),
    Key([mod, alt], "c", lazy.spawn("gnome-terminal -e weechat")),

    # Change the volume if your keyboard has special volume keys.
    Key(
        [], "XF86AudioRaiseVolume",
        lazy.spawn("amixer -c 0 -q set Master 2dB+")),
    Key(
        [], "XF86AudioLowerVolume",
        lazy.spawn("amixer -c 0 -q set Master 2dB-")),
    Key(
        [], "XF86AudioMute",
        lazy.spawn("amixer -c 0 -q set Master toggle")),
    Key(
        [], "XF86MonBrightnessUp",
        lazy.spawn("xbacklight +1")),
    Key(
        [], "XF86MonBrightnessDown",
        lazy.spawn("xbacklight -1")),
    Key(
        [mod, alt], "s",
        lazy.spawn("sh -c \'"+lock+" && systemctl suspend\'")),
]

# Mouse bindings and options
mouse = (
    Drag([mod], 'Button1', lazy.window.set_position_floating(),
        start=lazy.window.get_position()),
    Drag([mod], 'Button3', lazy.window.set_size_floating(),
        start=lazy.window.get_size()),
)

bring_front_click = True
cursor_warp = False
follow_mouse_focus = True

# Groups
group_names = [
    ("Work1", {'layout': 'max'}),
    ("Work2", {'layout': 'max'}),
    ("Web",   {'layout': 'max'}),
    ("Email", {'layout': 'max'}),
    ("Chat",  {'layout': 'tile'}),
    ("Music", {}),
    ("VM",    {}),
]
groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name)))

dgroups_key_binder = None
dgroups_app_rules = []

# Layouts
layouts = [
    layout.Max(),
    layout.MonadTall(),
    layout.Stack(num_stacks=2),
    layout.Tile(),
    layout.RatioTile(),
    layout.Matrix(),
]

floating_layout = layout.Floating()

# Colors
colors =   [["#7cfcff", "#00afff"], # cyan gradiant
            ["#323335", "#525355"], # grey gradiant
            ["#040404", "#111113"]] # darker grey gradiant
# Screens and widget options
screens = [
    Screen(
        top=bar.Bar(
            widgets=[
               widget.TextBox(text="◤ ", fontsize=45, padding=-8,
                              foreground=colors[1], background=colors[2]),
               widget.GroupBox(fontsize=11, padding=4, borderwidth=1,
                               this_current_screen_border=colors[0]),
               widget.TextBox(text="◤", fontsize=45, padding=-1,
                              foreground=colors[2], background=colors[1]),
               widget.TaskList(borderwidth=1, background=colors[1],
                               border=colors[0], urgent_border=colors[0]),
               widget.Prompt(prompt="$ ", font="DejaVu Sans Mono", foreground=colors[0], completer="cmd"),
               widget.Notify(),
               widget.Systray(background=colors[1]),
               widget.TextBox(text="◤", fontsize=45, padding=-1,
                              foreground=colors[1], background=colors[2]),
               # widget.BitcoinTicker(),
               widget.Net(interface="wlp3s0", update_interval=1),
               widget.TextBox(text="CPU:", foreground=colors[0], fontsize=12),
               widget.CPUGraph(core="all"),
               widget.TextBox(text="Layout:", foreground=colors[0], fontsize=12),
               widget.CurrentLayout(),
               widget.TextBox(text="Vol:", foreground=colors[0], fontsize=12),
               widget.Volume(),
               widget.TextBox(text="BAT:", foreground=colors[0], fontsize=12),
               widget.Battery(battery_name="BAT1", low_percentage=0.50, format='{percent:1.0%} {hour:d}:{min:02d}', foreground="#0FFF00"),
               widget.TextBox(text=" ⌚", foreground=colors[0], fontsize=18),
               widget.Clock(fmt="%a %d-%m-%Y %H:%M")
            ],
            size=25,
        ),
    ),
]

widget_defaults = dict(
    font="DejaVu",
    fontsize=11,
    padding=2,
    background=colors[2]
)

auto_fullscreen = True

@hook.subscribe.client_new
def floating(window):
    floating_types = ['notification', 'toolbar', 'splash', 'dialog']
    transient = window.window.get_wm_transient_for()
    if window.window.get_wm_type() in floating_types or transient:
        window.floating = True

@hook.subscribe.startup_once
def autostart():
    subprocess.Popen("start-pulseaudio-x11")
    subprocess.Popen("/usr/bin/gnome-keyring-daemon --start --components=gpg".split())
    subprocess.Popen("/usr/bin/gnome-keyring-daemon --start --components=secrets".split())
    subprocess.Popen("/usr/bin/gnome-keyring-daemon --start --components=pkcs11".split())
    subprocess.Popen("/usr/bin/gnome-keyring-daemon --start --components=ssh".split())
    subprocess.Popen("/usr/lib/evolution/evolution-alarm-notify")
    subprocess.Popen("nm-applet")
    # subprocess.Popen("gnome-terminal")
    subprocess.Popen("redshift -l 22.8265277:86.17281530000002".split())

def main(qtile):
    pass
