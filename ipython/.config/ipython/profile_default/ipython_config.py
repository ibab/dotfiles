
c = get_config()

# This can cause problems with functions that expect
# non-unicode strings, but is generally worth it
c.InteractiveShellApp.exec_lines = ["from __future__ import absolute_import, division, print_function, unicode_literals"]

c.TerminalIPythonApp.display_banner = False
c.TerminalInteractiveShell.show_rewritten_input = False
c.TerminalInteractiveShell.colors = 'Linux'
c.TerminalInteractiveShell.separate_in = ''
c.TerminalInteractiveShell.wildcards_case_sensitive = True
c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.term_title = True
c.PromptManager.out_template = '[\\#]: '
c.PromptManager.in_template = r' [\#]: '
