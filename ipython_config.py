#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Configuration for IPython."""


c = get_config()

c.TerminalIPythonApp.display_banner = False
c.InteractiveShell.confirm_exit = False

c.InteractiveShellApp.extensions = ["autoreload"]
c.InteractiveShellApp.exec_lines = ["%autoreload 2"]

c.PromptManager.justify = False
c.PromptManager.in_template = "{color.LightGreen}>> "
c.PromptManager.out_template = "{color.LightRed}> "
