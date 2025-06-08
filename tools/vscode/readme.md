vscodeは基本的にwindowsアプリのため、windows側で実行するだけで問題はずだが、
extensionsはwsl2のそれぞれの仮想環境にインストールされるので注意すること。

## bat 説明

- backup.bat
  現vscode設定をバックアップする
- setup.bat
  保持している設定をvscodeに反映する
  ※extensionsの内容を破棄しておいたほうがいいかもしれない。（更新されない場合がある）
    setup.bat実行するまえにcleanup_extension.batを実行しておくといいかもしれない

## proxy の場合

proxy ディレクトリ内の設定を利用すること

## 保存場所

### windows

- %AppData%\Code\User
  settings.json, keybindings.json が配置
- %UserProfile%\.vscode
  extensions が配置

### linux

※ settings.json, keybindings.json は windows 側を利用する
※ linux側のextensionsを削除するには cleanup_extensions.sh を実行

- ~/.vscode-server
  extensions が配置

  