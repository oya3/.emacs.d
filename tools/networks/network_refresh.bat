@echo off
color 0B
title ネットワークリフレッシュスクリプト

echo.
echo ===========================================
echo   Windows ネットワークリフレッシュスクリプト
echo ===========================================
echo.

rem 管理者権限チェック
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo.
    echo 警告: このスクリプトは管理者として実行してください。
    echo.
    pause
    exit /b 1
)

echo DNSキャッシュをクリアしています...
ipconfig /flushdns
echo.

rem echo Winsockカタログをリセットしています...
rem netsh winsock reset
rem echo.

rem echo TCP/IPスタックをリセットしています...
rem netsh int ip reset
rem echo.

rem echo WinHTTPプロキシ設定をリセットしています...
rem netsh winhttp reset proxy
rem echo.

echo 全てのIPアドレスを解放し、再取得しています...
ipconfig /release
ipconfig /renew
echo.

echo ネットワークリフレッシュが完了しました。
echo PCを再起動することを推奨します。
echo.

pause
exit /b 0