from gi.repository import GLib
import dbus
from dbus.mainloop.glib import DBusGMainLoop

def notifications(bus, message):
    if message.get_member() != "Notify":
        return
    keys = ["app_name", "replaces_id", "app_icon", "summary",
            "body", "actions", "hints", "expire_timeout"]
    args = message.get_args_list()
    if len(args) == 8:
        notification = dict([(keys[i], args[i]) for i in range(8)])
        print('%s|%s|%s'%(notification["app_name"], notification["summary"], notification["body"]))

DBusGMainLoop(set_as_default=True)

bus = dbus.SessionBus()
bus.add_match_string("interface='org.freedesktop.Notifications'")
bus.add_message_filter(notifications)

mainloop = GLib.MainLoop()
mainloop.run()
