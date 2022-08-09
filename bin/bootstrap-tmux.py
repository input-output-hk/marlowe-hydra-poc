import argparse
import libtmux
from collections import namedtuple
import os
import subprocess
import sys
import time
import yaml

# generic tmux bootstraper

PaneCfg = namedtuple('PaneCfg', ['cmd', 'await_cmd'])

WindowCfg = namedtuple('WindowCfg', ['name', 'panes'])

SetupCfg = namedtuple('SetupCfg', ['root', 'cmds'])

BootstraperCfg = namedtuple(
    'BootstraperCfg',
    ['session_name', 'setup', 'windows']
)


def load_config(config):
    session_name = config['session_name']
    setup = SetupCfg(**config['setup'])
    windows = []
    for name, window in config['windows'].items():
        panes = []
        for pane in window:
            cmd = ' '.join(pane['cmd'])
            panes.append(PaneCfg(cmd, pane.get('await_cmd', 'sleep 0')))
        windows.append(WindowCfg(name, panes))
    if not windows:
        raise Exception("You should provide windows configuartion")
    return BootstraperCfg(session_name, setup, windows)


def setup_window(session, window_cfg, window=None):
    if window_cfg.panes:
        if window is None:
            window = session.new_window(
                window_name=window_cfg.name,
                attach=False
            )

        [pane] = window.list_panes()
        pane_cfg = window_cfg.panes[0]
        pane.send_keys(pane_cfg.cmd)
        time.sleep(1)
        pane.enter()
        subprocess.run(pane_cfg.await_cmd, check=True, shell=True)

        for pane_cfg in window_cfg.panes[1:]:
            pane = window.split_window(vertical=True)
            pane.send_keys(pane_cfg.cmd)
            # Please don't ask me why we need to `sleep`
            # before we can perform working `enter`...
            time.sleep(1)
            pane.enter()
            subprocess.run(pane_cfg.await_cmd, check=True, shell=True)
    return window


def bootstrap(bootstraper_cfg):
    server = libtmux.Server()
    os.chdir(bootstraper_cfg.setup.root)
    for cmd in bootstraper_cfg.setup.cmds:
        subprocess.run(cmd, shell=True)

    session = server.find_where({'session_name': bootstraper_cfg.session_name})
    if not session:
        session = server.new_session(session_name=bootstraper_cfg.session_name)
        time.sleep(1)
        if os.fork() <= 0:
            window_cfg = bootstraper_cfg.windows[0]
            [window] = session.list_windows()
            setup_window(session, window_cfg, window=window)
            for window_cfg in bootstraper_cfg.windows[1:]:
                setup_window(session, window_cfg)
            sys.exit(0)
    server.attach_session(bootstraper_cfg.session_name)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Bootstrap tmux session')
    parser.add_argument('config', metavar='CONFIG', help='Tmux session config')
    args = parser.parse_args()
    # Default `hydra-demo` specific behaviour
    if not os.path.exists(args.config):
        raise Exception("Given config doesn't exists")
    # TODO: parse args and fallback
    with open(args.config, 'r') as stream:
        config = yaml.safe_load(stream)
    bootsraper_cfg = load_config(config)
    bootstrap(bootsraper_cfg)
