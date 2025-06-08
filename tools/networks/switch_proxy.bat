@echo off
setlocal

rem プロキシ設定のレジストリパス
set REG_PATH="HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings"
set PROXY_ENABLE_KEY="ProxyEnable"
set PROXY_SERVER_KEY="ProxyServer"
set PROXY_OVERRIDE_KEY="ProxyOverride"

rem プロキシサーバーのアドレスとポート（ご自身の環境に合わせて変更してください）
set PROXY_ADDRESS="172.17.20.213:8080"

rem 例外リスト（必要に応じて変更してください。セミコロン区切りで指定）
rem プロキシを使わないアドレス群
set PROXY_BYPASS="172.17.*.*;localhost;127.0.0.1;192.168.*.*;10.231.20.*"

for /f "tokens=3" %%A in ('reg query %REG_PATH% /v %PROXY_ENABLE_KEY% 2^>nul') do set CURRENT_PROXY_STATUS=%%A

if "%CURRENT_PROXY_STATUS%"=="0x0" (
    rem 現在オフなのでオンにする
    echo プロキシを有効にします...
    reg add %REG_PATH% /v %PROXY_ENABLE_KEY% /t REG_DWORD /d 1 /f >nul
    reg add %REG_PATH% /v %PROXY_SERVER_KEY% /t REG_SZ /d %PROXY_ADDRESS% /f >nul
    reg add %REG_PATH% /v %PROXY_OVERRIDE_KEY% /t REG_SZ /d %PROXY_BYPASS% /f >nul
    echo プロキシが有効になりました。
) else (
    rem 現在オンなのでオフにする
    echo プロキシを無効にします...
    reg add %REG_PATH% /v %PROXY_ENABLE_KEY% /t REG_DWORD /d 0 /f >nul
    rem 無効にする際はProxyServerとProxyOverrideを削除しても良いですが、
    rem 再度有効にした際に設定し直す必要があるため、ここではEnableだけ変更します。
    echo プロキシが無効になりました。
)

rem 設定の反映には時間がかかる場合があります。
rem または、Internet Explorer/Edgeのプロセスを再起動することで即時反映されることがあります。
rem プロンプトを閉じるまで少し待つ
pause