from IPython.terminal.prompts import Prompts, Token


c = get_config()

c.TerminalIPythonApp.display_banner = False
c.InteractiveShell.confirm_exit = False

c.InteractiveShellApp.extensions = ["autoreload"]
c.InteractiveShellApp.exec_lines = ["%autoreload 2"]


class MyPrompt(Prompts):
    def in_prompt_tokens(self, cli=None):
        return [(Token.Prompt, '>>> ')]

    def out_prompt_tokens(self):
        return [(Token.OutPrompt, '> ')]


c.TerminalInteractiveShell.prompts_class = MyPrompt
