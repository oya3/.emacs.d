@echo off
setlocal

rem �v���L�V�ݒ�̃��W�X�g���p�X
set REG_PATH="HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings"
set PROXY_ENABLE_KEY="ProxyEnable"
set PROXY_SERVER_KEY="ProxyServer"
set PROXY_OVERRIDE_KEY="ProxyOverride"

rem �v���L�V�T�[�o�[�̃A�h���X�ƃ|�[�g�i�����g�̊��ɍ��킹�ĕύX���Ă��������j
set PROXY_ADDRESS="172.17.20.213:8080"

rem ��O���X�g�i�K�v�ɉ����ĕύX���Ă��������B�Z�~�R������؂�Ŏw��j
rem �v���L�V���g��Ȃ��A�h���X�Q
set PROXY_BYPASS="172.17.*.*;localhost;127.0.0.1;192.168.*.*;10.231.20.*"

for /f "tokens=3" %%A in ('reg query %REG_PATH% /v %PROXY_ENABLE_KEY% 2^>nul') do set CURRENT_PROXY_STATUS=%%A

if "%CURRENT_PROXY_STATUS%"=="0x0" (
    rem ���݃I�t�Ȃ̂ŃI���ɂ���
    echo �v���L�V��L���ɂ��܂�...
    reg add %REG_PATH% /v %PROXY_ENABLE_KEY% /t REG_DWORD /d 1 /f >nul
    reg add %REG_PATH% /v %PROXY_SERVER_KEY% /t REG_SZ /d %PROXY_ADDRESS% /f >nul
    reg add %REG_PATH% /v %PROXY_OVERRIDE_KEY% /t REG_SZ /d %PROXY_BYPASS% /f >nul
    echo �v���L�V���L���ɂȂ�܂����B
) else (
    rem ���݃I���Ȃ̂ŃI�t�ɂ���
    echo �v���L�V�𖳌��ɂ��܂�...
    reg add %REG_PATH% /v %PROXY_ENABLE_KEY% /t REG_DWORD /d 0 /f >nul
    rem �����ɂ���ۂ�ProxyServer��ProxyOverride���폜���Ă��ǂ��ł����A
    rem �ēx�L���ɂ����ۂɐݒ肵�����K�v�����邽�߁A�����ł�Enable�����ύX���܂��B
    echo �v���L�V�������ɂȂ�܂����B
)

rem �ݒ�̔��f�ɂ͎��Ԃ�������ꍇ������܂��B
rem �܂��́AInternet Explorer/Edge�̃v���Z�X���ċN�����邱�Ƃő������f����邱�Ƃ�����܂��B
rem �v�����v�g�����܂ŏ����҂�
pause