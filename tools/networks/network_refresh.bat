@echo off
color 0B
title �l�b�g���[�N���t���b�V���X�N���v�g

echo.
echo ===========================================
echo   Windows �l�b�g���[�N���t���b�V���X�N���v�g
echo ===========================================
echo.

rem �Ǘ��Ҍ����`�F�b�N
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo.
    echo �x��: ���̃X�N���v�g�͊Ǘ��҂Ƃ��Ď��s���Ă��������B
    echo.
    pause
    exit /b 1
)

echo DNS�L���b�V�����N���A���Ă��܂�...
ipconfig /flushdns
echo.

rem echo Winsock�J�^���O�����Z�b�g���Ă��܂�...
rem netsh winsock reset
rem echo.

rem echo TCP/IP�X�^�b�N�����Z�b�g���Ă��܂�...
rem netsh int ip reset
rem echo.

rem echo WinHTTP�v���L�V�ݒ�����Z�b�g���Ă��܂�...
rem netsh winhttp reset proxy
rem echo.

echo �S�Ă�IP�A�h���X��������A�Ď擾���Ă��܂�...
ipconfig /release
ipconfig /renew
echo.

echo �l�b�g���[�N���t���b�V�����������܂����B
echo PC���ċN�����邱�Ƃ𐄏����܂��B
echo.

pause
exit /b 0