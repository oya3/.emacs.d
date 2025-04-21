copy *.json %APPDATA%\Code\User\.
for /F %%i in (vscode-extensions.txt) do code --install-extension %%i
